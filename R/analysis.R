# analysis.R

#' Low-level analysis constructor
#'
#' @param problem A problem object.
#' @param dir Character scalar. Analysis output directory.
#' @param problem_path Character scalar. Path to problem.RData.
#' @param manifest_path Character scalar. Path to model_manifest.json.
#'
#' @return An object of class c("<problem_type>_analysis", "analysis").
#'
#' @keywords internal
new_analysis <- function(problem, dir, problem_path, manifest_path) {
  structure(
    list(
      problem = problem,
      dir = normalizePath(dir, mustWork = FALSE),
      problem_path = normalizePath(problem_path, mustWork = FALSE),
      manifest_path = normalizePath(manifest_path, mustWork = FALSE)
    ),
    class = c(paste0(problem_type(problem), "_analysis"), "analysis")
  )
}


#' Construct or resume a reproducible machine-learning analysis
#'
#' An analysis links a [problem] to a directory containing
#'  a [manifest] setting out prediction models to run as well
#'  as stored prediction models and results.
#'
#' This function creates or loads an analysis based on the specified
#'  directory `dir`.
#'
#' If called with a [problem] and optionally a [manifest]
#'    it will attempt to initialise a new analysis in the directory.
#'
#' Creation mode:
#'
#' `analysis(problem = prob, dir = "my_analysis")`
#'
#' Resume mode:
#'
#' `analysis(dir = "my_analysis")`
#'
#' In creation mode, the problem is saved to `problem.RData` inside the analysis
#' directory, and a `model_manifest.json` file is created if one is not already
#' present.
#'
#' In resume mode, the problem and manifest are read from the analysis directory.
#'
#' @param problem Optional problem object. Required when creating a new analysis.
#'   If NULL, the analysis is reconstructed from the directory.
#' @param dir Character scalar. Analysis output directory.
#' @param manifest Optional manifest list. Used only in creation mode.
#' @param create Logical. Should the analysis directory be created if needed?
#' @param overwrite_problem Logical. Should an existing problem.RData be
#'   overwritten in creation mode?
#' @param overwrite_manifest Logical. Should an existing model_manifest.json be
#'   overwritten in creation mode?
#'
#' @return An object of class c("<problem_type>_analysis", "analysis").
#'
#' @export
analysis <- function(
  problem = NULL,
  dir,
  manifest = NULL,
  create = TRUE,
  overwrite_problem = FALSE,
  overwrite_manifest = FALSE
) {
  if (!is.character(dir) || length(dir) != 1 || is.na(dir) || dir == "") {
    stop("dir must be a non-empty character scalar.", call. = FALSE)
  }

  problem_path <- file.path(dir, "problem.RData")
  manifest_path <- file.path(dir, "model_manifest.json")

  if (is.null(problem)) {
    return(
      resume_analysis_from_dir(
        dir = dir,
        problem_path = problem_path,
        manifest_path = manifest_path
      )
    )
  }

  create_analysis_in_dir(
    problem = problem,
    dir = dir,
    problem_path = problem_path,
    manifest_path = manifest_path,
    manifest = manifest,
    create = create,
    overwrite_problem = overwrite_problem,
    overwrite_manifest = overwrite_manifest
  )
}


#' Create an analysis in a directory
#'
#' @param problem A problem object.
#' @param dir Character scalar.
#' @param problem_path Character scalar.
#' @param manifest_path Character scalar.
#' @param manifest Optional manifest list.
#' @param create Logical.
#' @param overwrite_problem Logical.
#' @param overwrite_manifest Logical.
#'
#' @return An analysis object.
#'
#' @keywords internal
create_analysis_in_dir <- function(
  problem,
  dir,
  problem_path,
  manifest_path,
  manifest = NULL,
  create = TRUE,
  overwrite_problem = FALSE,
  overwrite_manifest = FALSE
) {
  if (!inherits(problem, "problem")) {
    stop("problem must inherit from class 'problem'.", call. = FALSE)
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

  if (file.exists(problem_path) && !isTRUE(overwrite_problem)) {
    existing_problem <- read_analysis_problem(problem_path)

    if (!identical_problem_identity(problem, existing_problem)) {
      stop(
        "problem.RData already exists and appears to describe a different problem. ",
        "Use overwrite_problem = TRUE to replace it.",
        call. = FALSE
      )
    }
  } else {
    write_analysis_problem(problem, problem_path)
  }

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
    problem = read_analysis_problem(problem_path),
    dir = dir,
    problem_path = problem_path,
    manifest_path = manifest_path
  )

  validate_analysis(x)
}


#' Resume an analysis from a directory
#'
#' @param dir Character scalar.
#' @param problem_path Character scalar.
#' @param manifest_path Character scalar.
#'
#' @return An analysis object.
#'
#' @keywords internal
resume_analysis_from_dir <- function(dir, problem_path, manifest_path) {
  if (!dir.exists(dir)) {
    stop("Analysis directory does not exist.", call. = FALSE)
  }

  if (!file.exists(problem_path)) {
    stop(
      "Cannot resume analysis because problem.RData does not exist.",
      call. = FALSE
    )
  }

  if (!file.exists(manifest_path)) {
    stop(
      "Cannot resume analysis because model_manifest.json does not exist.",
      call. = FALSE
    )
  }

  problem <- read_analysis_problem(problem_path)

  validate_model_manifest(read_model_manifest(manifest_path))

  x <- new_analysis(
    problem = problem,
    dir = dir,
    problem_path = problem_path,
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

  if (
    !is.character(x$problem_path) ||
      length(x$problem_path) != 1 ||
      is.na(x$problem_path) ||
      x$problem_path == ""
  ) {
    stop(
      "analysis$problem_path must be a non-empty character scalar.",
      call. = FALSE
    )
  }

  if (!file.exists(x$problem_path)) {
    stop("problem.RData does not exist.", call. = FALSE)
  }

  stored_problem <- read_analysis_problem(x$problem_path)

  if (!inherits(stored_problem, "problem")) {
    stop("Stored problem does not inherit from class 'problem'.", call. = FALSE)
  }

  if (!identical_problem_identity(x$problem, stored_problem)) {
    stop(
      "analysis$problem does not match the problem stored in problem.RData.",
      call. = FALSE
    )
  }

  if (
    !is.character(x$manifest_path) ||
      length(x$manifest_path) != 1 ||
      is.na(x$manifest_path) ||
      x$manifest_path == ""
  ) {
    stop(
      "analysis$manifest_path must be a non-empty character scalar.",
      call. = FALSE
    )
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

  status <- check_models(x, enabled_only = TRUE) #nolint

  cli::cli_h1("Analysis")

  cli::cli_text("{.field Problem} : {.val {x$problem$label}}")
  cli::cli_text("{.field Type} : {.val {problem_type(x$problem)}}")
  cli::cli_text("{.field Directory} : {.path {x$dir}}")
  cli::cli_text("{.field Problem file} : {.file {x$problem_path}}")
  cli::cli_text("{.field Manifest} : {.file {x$manifest_path}}")

  cli::cli_h2("Models")

  cli::cli_text("{.field Enabled} : {.val {nrow(status)}} enabled")
  cli::cli_text("{.field Existing} : {.val {sum(status$exists)}} result folders found")  #nolint
  cli::cli_text("{.field Pending} : {.val {sum(!status$exists)}} result folders missing")  #nolint

  cli::cli_h2("Problem")

  print(problem(x))

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
  print(object)
}
