# analysis-helpers.R

#' Return enabled model names from a manifest
#'
#' @param x A model manifest
#' @param ... unused
#'
#' @return Character vector.
#'
#' @export
enabled_models <- function(x, ...) {
  UseMethod("enabled_models")
}

#' @rdname enabled_models
#' @export
enabled_models.analysis <- function(x, ...) {
  enabled_models.default(manifest(x))
}

#' @rdname enabled_models
#' @export
enabled_models.default <- function(x, ...) {
  validate_model_manifest(x)

  names(x$models)[
    vapply(
      x$models,
      function(entry) isTRUE(entry$enabled),
      logical(1)
    )
  ]
}


#' Return model result paths from a manifest
#'
#' @param x An analysis or its model manifest
#' @param ... unused
#'
#' @return Named character vector.
#'
#' @export
resultpaths <- function(x, ...) {
  UseMethod("resultpaths")
}

#' @rdname resultpaths
#' @export
resultpaths.analysis <- function(x, ...) {
  resultpaths.default(manifest(x))
}

#' @rdname resultpaths
#' @export
resultpaths.default <- function(x, ...) {
  validate_model_manifest(x)

  stats::setNames(
    vapply(
      x$models,
      function(entry) entry$resultpath,
      character(1)
    ),
    names(x$models)
  )
}


#' Return model setting params from a manifest
#'
#' @param x An analysis or its model manifest list.
#' @param ... unused.
#' @return Named list.
#'
#' @export
manifest_params <- function(x, ...) {
  UseMethod("manifest_params")
}

#' @rdname manifest_params
#' @export
manifest_params.analysis <- function(x, ...) {
  manifest_params.default(manifest(x))
}

#' @rdname manifest_params
#' @export
manifest_params.default <- function(x, ...) {
  validate_model_manifest(x)

  lapply(
    x$models,
    function(entry) {
      if (is.null(entry$params)) {
        list()
      } else {
        entry$params
      }
    }
  )
}


#' Check runtime model status for an analysis
#'
#' Placeholder checker.
#'
#' A model is considered present if the directory specified by its relative
#' resultpath exists inside the analysis directory.
#'
#' @param x An analysis object.
#' @param enabled_only Logical. If TRUE, check only enabled models.
#'
#' @return A data.frame containing model status.
#'
#' @keywords internal
check_models <- function(x,
                         enabled_only = TRUE,
                         ...) {
  UseMethod("check_models")
}

#' @rdname check_models
#' @export
check_models.default <- function(x, enabled_only = TRUE, ...) {
  stop("Only implemented for analysis class", call. = FALSE)
}

#' @rdname check_models
#' @export
check_models.analysis <- function(x, enabled_only = TRUE, ...) {
  validate_analysis(x)

  manifest <- manifest(x)

  model_names <- names(manifest$models)

  if (isTRUE(enabled_only)) {
    model_names <- enabled_models(manifest)
  }

  resultpaths <- resultpaths(manifest)[model_names]
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
#' @param ... unused.
#' @return Character vector of pending model names.
#'
#' @export
pending_models <- function(x, ...) {
  UseMethod("pending_models")
}

#' @rdname pending_models
#' @export
pending_models.default <- function(x, ...) {
  stop("Only implemented for analysis class", call. = FALSE)
}

#' @rdname pending_models
#' @export
pending_models.analysis <- function(x, ...) {
  status <- check_models(x, enabled_only = TRUE)

  status$model[!status$exists]
}


#' Return enabled models with existing result folders
#'
#' Intended to support a future run.analysis() method.
#'
#' @param x An analysis object.
#' @param ... unused.
#' @return Character vector of existing model names.
#'
#' @export
existing_models <- function(x, ...) {
  UseMethod("existing_models")
}

#' @rdname existing_models
#' @export
existing_models.default <- function(x, ...) {
  stop("Only implemented for analysis class", call. = FALSE)
}

#' @rdname existing_models
#' @export
existing_models.analysis <- function(x, ...) {
  status <- check_models(x, enabled_only = TRUE)

  status$model[!status$exists]
}
