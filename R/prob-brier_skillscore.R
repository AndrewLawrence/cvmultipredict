# Contain an extension to yardstick that calculates the Brier skill-score
#   i.e. 1 - (BS_model / BS_reference)
# where BS model is the brier score for a given model and
#   BS_reference is the brier score for an intercept only model

# re-implement some yardstick internal functions to avoid :::

#' @keywords internal
.event_level <- function() {
  "first"
}

#' @keywords internal
.ys_validate_event_level <- function(event_level) {
  if (identical(event_level, "first")) {
    return(invisible())
  }
  if (identical(event_level, "second")) {
    return(invisible())
  }
  cli::cli_abort("{.arg event_level} must be {.val first} or {.val second}.")
}

#' @keywords internal
.ys_abort_if_class_pred <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "class_pred")) {
    cli::cli_abort("{.arg truth} should not a {.cls class_pred} object.",
                   call = call)
  }
  invisible(x)
}


#' Brier Skill Score for probability output of classification models
#'
#' Compute the Brier Skill Score for a classification model.
#'
#' @family class probability metrics
#' @seealso [brier_class][yardstick::brier_class],
#'  [rsq_trad][yardstick::rsq_trad],
#'  [All yardstick probability metrics][yardstick::prob-metrics]
#' @details
#' Brier score is a metric that should be minimised.
#' The output ranges from 0 to 1, with 1 indicating perfect predictions.
#'
#' The Brier Skill Score is analogous to [rsq_trad][yardstick::rsq_trad]
#' in regression models, but calculated from the brier scores of classification
#' model predicted probabilities. The [Brier score][yardstick::brier_class] is
#' the difference between a binary indicator for a class and its corresponding
#' class probability, squared and averaged. To convert to a skill score the
#'  formula:
#'
#'  \deqn{BSS = 1 - \frac{BS_{\text{model}}}{BS_{\text{ref}}}}
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @inheritParams yardstick::pr_auc
#' @importFrom rlang enquo check_bool
#' @importFrom vctrs vec_rbind vec_cbind
#' @import yardstick
#' @export
bss_class <- function(data, ...) {
  UseMethod("bss_class")
}
bss_class <- new_prob_metric(
  bss_class,
  direction = "minimize",
  range = c(0, 1)
)

#' @export
#' @rdname bss_class
bss_class.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  event_level = .event_level(),
  case_weights = NULL
) {
  case_weights_quo <- enquo(case_weights)

  prob_metric_summarizer(
    name = "bss_class",
    fn = bss_class_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!case_weights_quo
  )
}

#' @rdname bss_class
#' @export
bss_class_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  event_level = .event_level(),
  case_weights = NULL,
  ...
) {
  check_bool(na_rm)
  .ys_abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, metric_class = "bss_class")

  check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  bss_class_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

bss_class_estimator_impl <- function(
  truth,
  estimate,
  estimator,
  event_level,
  case_weights
) {
  if (identical(estimator, "binary")) {
    bss_class_binary(truth, estimate, event_level, case_weights)
  } else {
    bss_factor(truth, estimate, case_weights)
  }
}

bss_class_binary <- function(truth, estimate, event_level, case_weights) {
  if (!identical(event_level, "first")) {
    lvls <- levels(truth)
    truth <- stats::relevel(truth, lvls[[2]])
  }

  estimate <- matrix(c(estimate, 1 - estimate), ncol = 2)

  bss_factor(truth, estimate, case_weights)
}

# If `truth` is already a vector or matrix of binary data
bss_ind <- function(truth, estimate, case_weights = NULL) {
  if (is.vector(truth)) {
    truth <- matrix(truth, ncol = 1)
  }

  if (is.vector(estimate)) {
    estimate <- matrix(estimate, ncol = 1)
  }
  # In the binary case:
  if (ncol(estimate) == 1 && ncol(truth) == 2) {
    estimate <- unname(estimate)
    estimate <- vec_cbind(estimate, 1 - estimate, .name_repair = "unique_quiet")
  }

  resids <- (truth - estimate)^2

  if (is.null(case_weights)) {
    case_weights <- rep(1, nrow(resids))
  }

  not_missing <- !is.na(case_weights)
  resids <- resids[not_missing, , drop = FALSE]
  case_weights <- case_weights[not_missing]

  # Normalize weights (in case negative weights)
  # subtracting max to avoid Inf in calculations
  #  exp(x - max(x)) / sum(exp(x - max(x)))
  #  = (exp(x) / exp(max(x))) / (sum(exp(x)) / exp(max(x)))
  #  = exp(x) / sum(exp(x))
  case_weights <- case_weights - max(case_weights)
  case_weights <- exp(case_weights) / sum(exp(case_weights))

  res <- sum(resids * case_weights) / (2 * sum(case_weights))

  # Skill calculations:
  #   res is the standard Brier score.
  null_estimate <- matrix(
    colMeans(truth),
    ncol = NCOL(truth),
    nrow = NROW(truth),
    byrow = TRUE
  )
  null_resids <- (truth - null_estimate)^2

  null_res <- sum(null_resids * case_weights) / (2 * sum(case_weights))

  1 - (res / null_res)
}

# When `truth` is a factor
bss_factor <- function(truth, estimate, case_weights = NULL) {
  inds <- hardhat::fct_encode_one_hot(truth)

  case_weights <- vctrs::vec_cast(case_weights, to = double())

  bss_ind(inds, estimate, case_weights)
}
