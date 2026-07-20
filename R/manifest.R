# manifest.R

#' Create a default model manifest
#'
#' @return A model manifest list.
#'
#' @export
default_model_manifest <- function() {
  list(
    file_version = 1L,
    models = list(
      linear = model_manifest_entry(
        function_name = "linear",
        resultpath = "models/",
        params = list(pca = "auto", pca_ncomp = "auto")
      ),
      elasticnet = model_manifest_entry(
        function_name = "elasticnet",
        resultpath = "models/",
        params = list()
      ),
      xgboost = model_manifest_entry(
        function_name = "xgboost",
        resultpath = "models/",
        params = list()
      ),
      neuralnet = model_manifest_entry(
        function_name = "neuralnet",
        resultpath = "models/",
        params = list()
      )
    )
  )
}

#' Create a model entry for a model manifest
#'
#' @param function_name the underlying model function used by cvmultipredict
#' @param resultpath Character scalar. Relative path from the analysis directory
#'   to the model output directory.
#' @param params Named list. Model-specific fitting options.
#'
#' @return A list.
#'
#' @keywords internal
model_manifest_entry <- function(
  function_name,
  resultpath,
  params = list()
) {

  if (missing(function_name)) {
    stop("function must be supplied.", call. = FALSE)
  }

  if (missing(resultpath)) {
    stop("resultpath must be supplied.", call. = FALSE)
  }

  list(
    function_name = function_name,
    resultpath = resultpath,
    params = params
  )
}


#' Read a model manifest from JSON
#'
#' @param path Path to model_manifest.json.
#'
#' @return A manifest list.
#'
#' @export
read_model_manifest <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to read model manifests.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("Manifest file does not exist.", call. = FALSE)
  }

  jsonlite::fromJSON(path, simplifyVector = FALSE)
}


#' Write a model manifest to JSON
#'
#' @param manifest A model manifest list.
#' @param path Output path.
#'
#' @return The path, invisibly.
#'
#' @export
write_model_manifest <- function(manifest, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to write model manifests.", call. = FALSE)
  }

  validate_model_manifest(manifest)

  out_dir <- dirname(path)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  jsonlite::write_json(
    manifest,
    path = path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  invisible(path)
}


#' Validate a model manifest
#'
#' @param manifest A model manifest list.
#'
#' @return The validated manifest.
#'
#' @export
validate_model_manifest <- function(manifest) {

  # ---- Top-level structure ----

  if (!is.list(manifest)) {
    stop("Manifest must be a list.", call. = FALSE)
  }

  if (is.null(manifest$file_version)) {
    stop("Manifest must contain file_version.", call. = FALSE)
  }

  if (
    !is.numeric(manifest$file_version) ||
      length(manifest$file_version) != 1 ||
      is.na(manifest$file_version)
  ) {
    stop("Manifest file_version must be a numeric scalar.", call. = FALSE)
  }

  if (is.null(manifest$models) || !is.list(manifest$models)) {
    stop("Manifest must contain a models list.", call. = FALSE)
  }

  if (length(manifest$models) == 0) {
    stop("Manifest models list must contain at least one model.", call. = FALSE)
  }

  # ---- Model names (canonical IDs) ----

  model_names <- names(manifest$models)

  if (is.null(model_names) || any(model_names == "")) {
    stop("All manifest models must be named.", call. = FALSE)
  }

  if (anyDuplicated(model_names)) {
    stop("Manifest model names must be unique.", call. = FALSE)
  }

  # force folder-safe model names
  if (
    any(grepl("[/\\\\]", model_names)) ||
      any(grepl("\\.\\.", model_names))
  ) {
    stop("Model names must be folder-safe (no '/' '\\\\' or '..').", call. = FALSE)
  }

  # ---- Per-model validation ----

  for (model_name in model_names) {

    entry <- manifest$models[[model_name]]

    if (!is.list(entry)) {
      stop(
        "Manifest entry for model '",
        model_name,
        "' must be a list.",
        call. = FALSE
      )
    }

    # function_name
    if (
      is.null(entry$function_name) ||
        !is.character(entry$function_name) ||
        length(entry$function_name) != 1 ||
        is.na(entry$function_name) ||
        entry$function_name == ""
    ) {
      stop(
        "Manifest entry for model '",
        model_name,
        "' must contain non-empty character scalar function_name.",
        call. = FALSE
      )
    }

    # resultpath
    if (
      is.null(entry$resultpath) ||
        !is.character(entry$resultpath) ||
        length(entry$resultpath) != 1 ||
        is.na(entry$resultpath) ||
        entry$resultpath == ""
    ) {
      stop(
        "Manifest entry for model '",
        model_name,
        "' must contain non-empty character scalar resultpath.",
        call. = FALSE
      )
    }

    if (is_absolute_path(entry$resultpath)) {
      stop(
        "Manifest entry for model '",
        model_name,
        "' must use a relative resultpath.",
        call. = FALSE
      )
    }

    if (has_parent_path_component(entry$resultpath)) {
      stop(
        "Manifest entry for model '",
        model_name,
        "' must not contain '..' path components.",
        call. = FALSE
      )
    }

    # params
    if (!is.null(entry$params) && !is.list(entry$params)) {
      stop(
        "Manifest entry for model '",
        model_name,
        "' has params, but params is not a list.",
        call. = FALSE
      )
    }
  }

  manifest
}


#' Check whether a path is absolute
#'
#' @param path Character scalar.
#'
#' @return Logical scalar.
#'
#' @keywords internal
is_absolute_path <- function(path) {
  grepl("^/", path) || grepl("^[A-Za-z]:[/\\\\]", path)
}


#' Check whether a relative path contains parent directory components
#'
#' @param path Character scalar.
#'
#' @return Logical scalar.
#'
#' @keywords internal
has_parent_path_component <- function(path) {
  parts <- unlist(strsplit(path, "[/\\\\]+"))
  any(parts == "..")
}


#' Read an analysis manifest
#'
#' The manifest of an analysis is a JSON specifying which models to run, along
#'   with additional options.
#'
#' @param x Object to read manifest for.
#' @param ... Additional arguments.
#'
#' @return a list setting out file_version and models: a list of
#'    the models to run for a given analysis.
#'
#' @export
manifest <- function(x, ...) {
  UseMethod("manifest")
}

#' @export
manifest.default <- function(x, ...) {
  # attempt to read json file location
  validate_model_manifest(read_model_manifest(path = x))
}

#' @export
manifest.analysis <- function(x, ...) {
  manifest.default(x$manifest_path)
}
