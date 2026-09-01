
#' Apply to an analysis to run models or collate results from disk
#'
#' @param x an [analysis] object (which links a [problem] and a [manifest] to a
#'    working directory)
#' @param .RUN should missing models be calculated?
#'     If FALSE then cvmultipredict will attempt to load pre-calculated
#'     results from disk. At least the final model stage must have completed.
#'
#' @return A named list containing, for each model in the [manifest],
#'     a data.frame of performance metrics.
#'
#' @seealso [problem] [manifest] [analysis] [pool_results]
#' @export
cvmultipredict <- function(x, .RUN = TRUE) {
  force(x)
  x <- validate_analysis(x)

  # message:
  msg_cvmp_start(x)

  mtab <- check_models(x)

  to_run <- mtab[["supported"]]

  to_run <- setNames(to_run, mtab[["label"]])

  run_ints <- which(to_run)

  res <- lapply(
    run_ints,
    \(.x) {
      msg_model_start(mtab[["label"]][.x])
      cl <- call_from_analysis(x, .x)
      cl[[".RUN"]] <- .RUN
      r <- eval(cl, envir = list(x = x))
      msg_model_finish(mtab[["label"]][.x])
      r
    }
  )
  res
}

msg_cvmp_start <- function(x) {
  p <- problem(x)
  force(p)
  m <- manifest(x)

  cli::cli_alert_info(
    "running cvmultipredict on {.val {p[['label']]}}"
  )
  cli::cli_alert_info(
    "a {.val {problem_type(p)}} with {.val {nrow(p$x)}} observations and {.val {ncol(p$x)}} predictors" #nolint
  )
  cli::cli_text("---")
  cli::cli_text("Models:")
  for ( i in seq_along(m$models) ) {
    cli::cli_alert_info(
      "{.val {names(m$models)[[i]]}} - {.val {m$models[[i]][['function_name']]}}"
    )
  }
  cli::cli_text("---")
}
