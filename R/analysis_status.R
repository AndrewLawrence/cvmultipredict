# analysis-status.R

#' Check runtime model status for an analysis
#'
#' Placeholder checker.
#'
#' A model is considered present if the directory specified by its relative
#' resultpath exists inside the analysis directory.
#'
#' This does not check fitted model objects, metrics, logs, marker files,
#' timestamps, or any other completion metadata.
#'
#' @param x An analysis object.
#' @param enabled_only Logical. If TRUE, check only enabled models.
#'
#' @return A data.frame containing model status.
#'
#' @export
check_analysis_models <- function(x, enabled_only = TRUE) {
  validate_analysis(x)

  manifest <- analysis_manifest(x)

  model_names <- names(manifest$models)

  if (isTRUE(enabled_only)) {
    model_names <- manifest_enabled_models(manifest)
  }

  resultpaths <- manifest_resultpaths(manifest)[model_names]
  absolute_paths <- file.path(x$dir, resultpaths)

  data.frame(
    model = model_names,
    enabled = vapply(
      manifest$models[model_names],
      function(entry) isTRUE(entry$enabled),
      logical(1)
    ),
    resultpath = unname(resultpaths),
    path = unname(absolute_paths),
    exists = dir.exists(absolute_paths),
    stringsAsFactors = FALSE
  )
}


#' Return enabled models with missing result folders
#'
#' Intended to support a future run.analysis() method.
#'
#' @param x An analysis object.
#'
#' @return Character vector of pending model names.
#'
#' @export
analysis_pending_models <- function(x) {
  status <- check_analysis_models(x, enabled_only = TRUE)

  status$model[!status$exists]
}


#' Return enabled models with existing result folders
#'
#' @param x An analysis object.
#'
#' @return Character vector of model names.
#'
#' @export
analysis_existing_models <- function(x) {
  status <- check_analysis_models(x, enabled_only = TRUE)

  status$model[status$exists]
}
