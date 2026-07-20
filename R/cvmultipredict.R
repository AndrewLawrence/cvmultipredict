
#' Apply to an analysis to check/run models
#' @param x an [analysis] object linking a [problem] and a [manifest] to a
#'    working directory
#' @return a list of results from the models which were run
#' @export
cvmultipredict <- function(x) {
  force(x)
  x <- validate_analysis(x)

  mtab <- check_models(x)

  to_run <- mtab[["supported"]]

  to_run <- setNames(to_run, mtab[["label"]])

  run_ints <- which(to_run)

  res <- lapply(
    run_ints,
    \(.x) eval(call_from_analysis(x, .x), envir = list(x = x))
  )
  res
}
