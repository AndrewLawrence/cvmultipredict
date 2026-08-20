#' Select predictors using random forest importance
#'
#' @param recipe A recipe object.
#' @param ... Predictor selectors.
#' @param role Not used.
#' @param trained Logical.
#' @param n_selected Number of variables to retain.
#' @param outcome Name of the outcome variable.
#' @param selected_predictors Filled during prep().
#' @param skip Skip when baking new data.
#' @param id Unique id.
#'
#' @export
step_selectbyrfimp <- function(recipe,
                               ...,
                               role = NA,
                               trained = FALSE,
                               n_selected = 10L,
                               outcome = NULL,
                               selected_predictors = NULL,
                               skip = FALSE,
                               id = recipes::rand_id("selectbyrfimp")) {
  recipes::add_step(
    recipe,
    step_selectbyrfimp_new(
      terms = rlang::enquos(...),
      role = role,
      trained = trained,
      n_selected = n_selected,
      outcome = outcome,
      selected_predictors = selected_predictors,
      skip = skip,
      id = id
    )
  )
}

step_selectbyrfimp_new <- function(terms,
                                   role,
                                   trained,
                                   n_selected,
                                   outcome,
                                   selected_predictors,
                                   skip,
                                   id) {
  recipes::step(
    subclass = "selectbyrfimp",
    terms = terms,
    role = role,
    trained = trained,
    n_selected = n_selected,
    outcome = outcome,
    selected_predictors = selected_predictors,
    skip = skip,
    id = id
  )
}

#' @importFrom recipes prep
#' @importFrom dplyr arrange
#' @importFrom filtro score_imp_rf
#' @export
prep.step_selectbyrfimp <- function(x, training, info = NULL, ...) {
  cols <- recipes::recipes_eval_select(x$terms, training, info)

  if (is.null(x$outcome)) {
    outcome_col <-
      info$variable[info$role == "outcome"]

    if (length(outcome_col) != 1) {
      rlang::abort("step_selectbyrfimp() currently supports exactly one outcome.")
    }

  } else {
    outcome_col <- x$outcome
  }

  dat <- training[, c(outcome_col, cols), drop = FALSE]

  filter_results <- filtro::score_imp_rf |>
    fit(stats::as.formula(paste(outcome_col, "~ .")), data = dat)

  imp <- filter_results@results |>
    dplyr::arrange(-.data$score)

  n_keep <- min(as.integer(x$n_selected), nrow(imp))

  selected_predictors <- imp[["predictor"]][seq_len(n_keep)]

  step_selectbyrfimp_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    n_selected = x$n_selected,
    outcome = outcome_col,
    selected_predictors = selected_predictors,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom recipes bake
#' @export
bake.step_selectbyrfimp <- function(object, new_data, ...) {
  keep_cols <- unique(c(object$outcome, object$selected_predictors))

  new_data[, keep_cols, drop = FALSE]
}

#' @export
print.step_selectbyrfimp <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  cat("Random forest importance selection ")

  if (x$trained) {
    cat(paste0(
      "[",
      length(x$selected_predictors),
      " selected predictors]"
    ))
  } else {
    cat("[untrained]")
  }

  cat("\n")

  invisible(x)
}

#' @importFrom recipes tidy
#' @export
tidy.step_selectbyrfimp <- function(x, ...) {
  if (!x$trained) {
    tibble::tibble(terms = recipes::sel2char(x$terms), value = NA_character_)
  } else {
    tibble::tibble(terms = x$selected_predictors)
  }
}

#' @importFrom tune tunable
#' @export
tunable.step_selectbyrfimp <- function(x, ...) {

  tibble::tibble(
    name = "n_selected",
    call_info = list(
      list(
        pkg = "cvmultipredict",
        fun = "rf_n_selected"
      )
    ),
    source = "recipe",
    component = "step_selectbyrfimp",
    component_id = x$id
  )
}

#' rf_n_selected
#'
#' Number of predictors selected by Random Forest importance used in
#'     [step_selectbyrfimp].
#' @details
#'    This parameter is integer. Upper bound should be determined by
#'    the number of predictors in the model.
#' @inheritParams dials::learn_rate range trans
#' @importFrom dials unknown new_quant_param
#' @export
rf_n_selected <- function(range = c(1L, dials::unknown()),
                          trans = NULL) {

  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(
      n_selected = "# Selected Predictors"
    ),
    finalize = get_rf_n_selected
  )
}

#' @importFrom dials parameters
#' @export
parameters.step_selectbyrfimp <- function(x, ...) {

  dials::parameters(
    rf_n_selected()
  )
}

#' @keywords internal
get_rf_n_selected <- function(object, x, ...) {

  if (inherits(x, "recipe")) {
    vars <- summary(x)
    n_pred <- sum(vars$role == "predictor")
  } else {
    n_pred <- ncol(x)
  }

  object$range$upper <- n_pred

  object
}
