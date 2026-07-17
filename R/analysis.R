# analysis.R

#' Low-level analysis constructor
#'
#' @param problem A problem object.
#' @param dir Character scalar. Analysis output directory.
#' @param manifest_path Character scalar. Path to model_manifest.json.
#'
#' @return An object of class c("<problem_type>_analysis", "analysis").
#'
#' @keywords internal
new_analysis <- function(problem, dir, manifest_path) {
  structure(
    list(
      problem = problem,
      dir = normalizePath(dir, mustWork = FALSE),
      manifest_path = normalizePath(manifest_path, mustWork = FALSE)
    ),
    class = c(paste0(problem_type(problem), "_analysis"), "analysis")
  )
}


#' Construct a reproducible machine-learning analysis
#'
#' Creates an analysis object linking a problem object to an output directory
#' and a model manifest.
#'
#' The manifest is intentionally analysis agnostic. It contains only:
#' file_version, enabled models, relative result paths, and model parameters.
#'
#' @param problem A problem object.
#' @param dir Character scalar. Analysis output directory.
#' @param manifest Optional manifest list. If NULL, an existing manifest will be
#'   read if present, otherwise a default manifest will be created.
#' @param create Logical. Should the analysis directory be created?
#' @param overwrite_manifest Logical. Should an existing manifest be
#'   overwritten?
#'
#' @return An object of class c("<problem_type>_analysis", "analysis").
#'
#' @export
analysis <- function(
  problem,
  dir,
  manifest = NULL,
  create = TRUE,
  overwrite_manifest = FALSE
) {
  if (!inherits(problem, "problem")) {
    stop("problem must inherit from class 'problem'.", call. = FALSE)
  }

  if (!is.character(dir) || length(dir) != 1 || is.na(dir) || dir == "") {
    stop("dir must be a non-empty character scalar.", call. = FALSE)
  }

  if (!dir.exists(dir)) {
    if (isTRUE(create)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    } else {
      stop("Analysis directory does not exist.", call. = FALSE)
    }
  }

  if (!dir.exists(dir)) {
    stop("Failed to create analysis directory.", call. = FALSE)
  }

  manifest_path <- file.path(dir, "model_manifest.json")

  if (is.null(manifest)) {
    if (file.exists(manifest_path) && !isTRUE(overwrite_manifest)) {
      manifest <- read_model_manifest(manifest_path)
    } else {
      manifest <- default_model_manifest()
    }
  }

  validate_model_manifest(manifest)

  if (!file.exists(manifest_path) || isTRUE(overwrite_manifest)) {
    write_model_manifest(manifest, manifest_path)
  } else {
    validate_model_manifest(read_model_manifest(manifest_path))
  }

  x <- new_analysis(
    problem = problem,
    dir = dir,
    manifest_path = manifest_path
  )

  validate_analysis(x)
}


#' Validate an analysis object
#'
#' @param x An analysis object.
#'
#' @return The validated analysis object.
#'
#' @keywords internal
validate_analysis <- function(x) {
  if (!inherits(x, "analysis")) {
    stop("x must inherit from class 'analysis'.", call. = FALSE)
  }

  if (!inherits(x$problem, "problem")) {
    stop("analysis$problem must inherit from class 'problem'.", call. = FALSE)
  }

  if (!is.character(x$dir) || length(x$dir) != 1 || is.na(x$dir) || x$dir == "") {
    stop("analysis$dir must be a non-empty character scalar.", call. = FALSE)
  }

  if (!dir.exists(x$dir)) {
    stop("analysis$dir does not exist.", call. = FALSE)
  }

  if (!is.character(x$manifest_path) ||
        length(x$manifest_path) != 1 ||
        is.na(x$manifest_path) ||
        x$manifest_path == "") {
    stop("analysis$manifest_path must be a non-empty character scalar.",
         call. = FALSE)
  }

  if (!file.exists(x$manifest_path)) {
    stop("model_manifest.json does not exist.", call. = FALSE)
  }

  validate_model_manifest(read_model_manifest(x$manifest_path))

  x
}

# print-summary-analysis.R

#' Print an analysis
#'
#' @param x An analysis object.
#' @param ... Ignored.
#'
#' @export
print.analysis <- function(x, ...) {
  validate_analysis(x)

  status <- check_analysis_models(x, enabled_only = TRUE)

  cat("\n")
  cat("Analysis\n")
  cat("--------\n")
  cat("Problem :", x$problem$label, "\n")
  cat("Type    :", problem_type(x$problem), "\n")
  cat("Dir     :", x$dir, "\n")
  cat("Manifest:", x$manifest_path, "\n")
  cat("Models  :", nrow(status), "enabled\n")
  cat("Existing:", sum(status$exists), "result folders found\n")
  cat("Pending :", sum(!status$exists), "result folders missing\n")

  invisible(x)
}


#' Summarise an analysis
#'
#' @param object An analysis object.
#' @param ... Ignored.
#'
#' @return An object of class "summary.analysis".
#'
#' @export
summary.analysis <- function(object, ...) {
  validate_analysis(object)

  manifest <- analysis_manifest(object)
  status_all <- check_analysis_models(object, enabled_only = FALSE)
  status_enabled <- status_all[status_all$enabled, , drop = FALSE]

  out <- list(
    problem_label = object$problem$label,
    problem_type = problem_type(object$problem),
    n = nrow(object$problem$x),
    p = ncol(object$problem$x),
    n_folds = length(object$problem$folds),
    dir = object$dir,
    manifest_path = object$manifest_path,
    file_version = manifest$file_version,
    model_status = status_all,
    n_models = nrow(status_all),
    n_enabled = sum(status_all$enabled),
    n_existing = sum(status_enabled$exists),
    n_pending = sum(!status_enabled$exists)
  )

  class(out) <- c(
    paste0("summary.", problem_type(object$problem), "_analysis"),
    "summary.analysis"
  )

  out
}


#' @export
print.summary.analysis <- function(x, ...) {
  cat("Analysis summary\n")
  cat("----------------\n")
  cat("Problem :", x$problem_label, "\n")
  cat("Type    :", x$problem_type, "\n")
  cat("Rows    :", x$n, "\n")
  cat("Features:", x$p, "\n")
  cat("Folds   :", x$n_folds, "\n")
  cat("Dir     :", x$dir, "\n")
  cat("Manifest:", x$manifest_path, "\n")
  cat("Version :", x$file_version, "\n")
  cat("\n")
  cat("Models\n")
  cat("------\n")
  cat("Total   :", x$n_models, "\n")
  cat("Enabled :", x$n_enabled, "\n")
  cat("Existing:", x$n_existing, "\n")
  cat("Pending :", x$n_pending, "\n")
  cat("\n")

  print(x$model_status, row.names = FALSE)

  invisible(x)
}
