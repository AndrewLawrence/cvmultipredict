#nolint start
# Dev Note: key differences between tidymodels code written for regression and
#               binary classification are as follows:
#
# 1) The model and metricset change:
#     model = parsnip::logistic_reg() rather than linear_reg
#     metricset = metricset_classification() rather than metricset_regression
#
# 2) predict.workflow type = "prob" rather than "numeric"
#     postprocess to select the second column (cvmp package assumes this)
#
#     i.e.:
        # preds <- predict(m, new_data = df, type = "numeric") |>
        #   bind_cols(y = df$y) |>
        #   bind_cols(label = "apparent")
#       BECOMES:
        # predict(m, new_data = df, type = "prob") |>
        #   dplyr::select(-1) |>
        #   setNames(".pred") |>
        #   bind_cols(y = df$y) |>
        #   bind_cols(label = "apparent")
#
# 3) metricset must specify the event_level:
#     metricset(truth = "y", ".pred")
#   becomes
#     metricset(truth = "y", ".pred", event_level = "second")
#nolint end


#' @importFrom checkmate assertIntegerish assert_class
#' @keywords internal
call_from_analysis <- function(x, idx) {
  checkmate::assert_class(x, classes = c("analysis"))

  mtab <- check_models(x)
  man <- manifest(x)

  checkmate::assertIntegerish(idx,
                              len = 1,
                              lower = 1,
                              upper = nrow(mtab))

  fxn <- str2lang(mtab[["model"]][[idx]])

  params <- man$models[[idx]][["params"]]

  folder <- mtab[["path"]][[idx]]

  the_call <- list(fxn)
  the_call <- append(the_call, list(x = as.name("x")))
  the_call <- append(the_call, list(folder = folder))
  the_call <- append(the_call, params)

  as.call(the_call)
}

#' @importFrom yardstick metric_set mse rmse mae rsq rsq_trad
#' @keywords internal
metricset_regression <- function() {
  metric_set(mse,
             rmse,
             mae,
             rsq,
             rsq_trad)
}

#' @importFrom yardstick metric_set brier_class roc_auc
#' @keywords internal
metricset_classification <- function() {
  metric_set(brier_class,
             bss_class,
             roc_auc)
}

#' @importFrom cli cli_alert_info
#' @keywords internal
msg_model_start <- function(x) {
  cli::cli_bullets(c(">" = "Model: {.val {x}}"))
}

#' @keywords internal
msg_model_finish <- function(x) {
  cli::cli_alert_info("{.val {x}} complete")
  cli::cli_text("")
}

#' @keywords internal
msg_cv_running <- function(model, id, nid) {
  cli::cli_alert_info(
    "running - model: {.val {model}}, CV {.int {id}} / {.int {nid}}"
  )
}

#' @keywords internal
msg_cv_loading <- function(model, id, nid) {
  cli::cli_alert_info(
    "loading - model: {.val {model}}, CV {.int {id}} / {.int {nid}}"
  )
}

#' @keywords internal
msg_norun_mainmissing <- function() {
  cli::cli_abort("Cannot collect results for analysis: final model is missing")
}

# used for ranges.
#' @keywords internal
tune_or_fix_range <- function(x) {
  if (length(x) == 2) {
    tune()
  } else if (is.na(x)) {
    NULL
  } else {
    x
  }
}

# used for discrete values:
#' @keywords internal
tune_or_fix_discrete <- function(x) {
  if (length(x) > 1) {
    tune()
  } else if (is.na(x)) {
    NULL
  } else {
    x
  }
}
