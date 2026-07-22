
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
