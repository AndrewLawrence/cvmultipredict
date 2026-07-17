# analysis-helpers.R

#' Return the manifest for an analysis
#'
#' @param x An analysis object.
#'
#' @return A model manifest list.
#'
#' @export
analysis_manifest <- function(x) {
  UseMethod("analysis_manifest")
}


#' @export
analysis_manifest.analysis <- function(x) {
  validate_analysis(x)
  read_model_manifest(x$manifest_path)
}


#' Return enabled model names from a manifest
#'
#' @param manifest A model manifest list.
#'
#' @return Character vector.
#'
#' @export
manifest_enabled_models <- function(manifest) {
  validate_model_manifest(manifest)

  names(manifest$models)[
    vapply(
      manifest$models,
      function(entry) isTRUE(entry$enabled),
      logical(1)
    )
  ]
}


#' Return model result paths from a manifest
#'
#' @param manifest A model manifest list.
#'
#' @return Named character vector.
#'
#' @export
manifest_resultpaths <- function(manifest) {
  validate_model_manifest(manifest)

  stats::setNames(
    vapply(
      manifest$models,
      function(entry) entry$resultpath,
      character(1)
    ),
    names(manifest$models)
  )
}


#' Return model params from a manifest
#'
#' @param manifest A model manifest list.
#'
#' @return Named list.
#'
#' @export
manifest_params <- function(manifest) {
  validate_model_manifest(manifest)

  lapply(
    manifest$models,
    function(entry) {
      if (is.null(entry$params)) {
        list()
      } else {
        entry$params
      }
    }
  )
}


#' Return enabled model names for an analysis
#'
#' @param x An analysis object.
#'
#' @return Character vector.
#'
#' @export
analysis_enabled_models <- function(x) {
  manifest_enabled_models(analysis_manifest(x))
}


#' Return result paths for an analysis
#'
#' @param x An analysis object.
#'
#' @return Named character vector.
#'
#' @export
analysis_resultpaths <- function(x) {
  manifest_resultpaths(analysis_manifest(x))
}


#' Return model params for an analysis
#'
#' @param x An analysis object.
#'
#' @return Named list.
#'
#' @export
analysis_params <- function(x) {
  manifest_params(analysis_manifest(x))
}
