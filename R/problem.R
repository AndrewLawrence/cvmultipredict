## Devnotes: a problem is a list containing a particular structure:
##
## A problem consists of:
##
## label - unique problem identifier
## y - response vector
## x - predictor data frame
## ids - unique row identifiers
## folds - repeated cross-validation folds, typically generated with
##          caret::createMultiFolds()
##
## The target type is represented by the first element of the S3 class:
## class : c("regression", "problem")
## class : c("classification", "problem")


#' Low-level problem constructor
#'
#' @param label Character scalar identifying the problem.
#' @param ytype Character scalar giving the response type:
#'   "regression" or "classification".
#' @param y Response vector.
#' @param x Data frame of predictors.
#' @param ids Character vector of row identifiers.
#' @param folds List of training indices.
#'
#' @return An object with class c(ytype, "problem").
#'
#' @keywords internal
new_problem <- function(label, ytype, y, x, ids, folds) {
  structure(
    list(
      label = label,
      y = y,
      x = x,
      ids = ids,
      folds = folds
    ),
    class = c(ytype, "problem")
  )
}



#' Construct/extract a machine-learning problem
#'
#' Creates or extracts an object describing a supervised learning problem
#'   and the associated resampling scheme.
#'
#' The problem type is encoded in the S3 class:
#'
#' * `class = c("regression", "problem")`
#' * `class = c("classification", "problem")`
#'
#' A problem consists of:
#'
#' * label - unique problem identifier
#' * y - response vector
#' * x - predictor data frame
#' * ids - unique row identifiers
#' * folds - repeated cross-validation folds, typically generated with
#'   `caret::createMultiFolds()`
#'
#' @param label Character scalar identifying the problem.
#' @param ytype Character scalar giving the response type:
#'   "regression" or "classification".
#' @param y Response vector. Numeric for regression or factor for
#'   classification.
#' @param x Data frame of predictors.
#' @param ids Character vector of row identifiers.
#' @param folds List of training indices, typically produced by
#'   `caret::createMultiFolds()`.
#' @param ... unused.
#'
#' @return An object of class c(ytype, "problem").
#'
#' @export
problem <- function(x, ...) {
  UseMethod("problem")
}

#' @rdname problem
#' @export
problem.default <- function(x, y, label, ytype, ids, folds, ...) {
  validate_problem(
    new_problem(
      label = label,
      ytype = ytype,
      y = y,
      x = x,
      ids = ids,
      folds = folds
    )
  )
}

#' @rdname problem
#' @export
problem.analysis <- function(x, ...) {
  x[["problem"]]
}


#' Return the type of a problem
#'
#' @param x A problem object.
#'
#' @return `"regression"` or `"classification"`.
#'
#' @export
problem_type <- function(x) {
  UseMethod("problem_type")
}


#' @export
problem_type.problem <- function(x) {
  class(x)[1]
}


#' Validate a problem object
#'
#' Internal validator for objects of class "problem".
#'
#' @param x A problem object.
#'
#' @return The validated object, invisibly.
#'
#' @keywords internal
validate_problem <- function(x) {
  stopifnot(inherits(x, "problem"))

  ytype <- problem_type(x)

  if (!ytype %in% c("regression", "classification")) {
    stop(
      "Problem class must begin with 'regression' or 'classification'.",
      call. = FALSE
    )
  }

  if (!is.character(x$label) || length(x$label) != 1) {
    stop("label must be a character scalar.", call. = FALSE)
  }

  if (!is.data.frame(x$x)) {
    stop("x must be a data.frame.", call. = FALSE)
  }

  n <- nrow(x$x)

  if (length(x$y) != n) {
    stop("y must have one value per row of x.", call. = FALSE)
  }

  if (length(x$ids) != n) {
    stop("ids must have one value per row of x.", call. = FALSE)
  }

  if (!is.list(x$folds)) {
    stop("folds must be a list.", call. = FALSE)
  }

  if (inherits(x, "regression") && !is.numeric(x$y)) {
    stop(
      "For regression problems y must be numeric.",
      call. = FALSE
    )
  }

  if (inherits(x, "classification") && !is.factor(x$y)) {
    stop(
      "For classification problems y must be a factor.",
      call. = FALSE
    )
  }

  if ("y" %in% colnames(x)) {
    stop(
      "Do not include a variable called 'y' in the predictors",
      call. = FALSE
    )
  }

  invisible(x)
}

# problem_storage.R

#' Write an analysis problem to disk
#'
#' Saves the problem object to an RData file using the object name `problem`.
#'
#' @param problem A problem object.
#' @param path Output path, usually file.path(dir, "problem.RData").
#'
#' @return The path, invisibly.
#'
#' @keywords internal
write_analysis_problem <- function(problem, path) {
  if (!inherits(problem, "problem")) {
    stop("problem must inherit from class 'problem'.", call. = FALSE)
  }

  out_dir <- dirname(path)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  save(problem, file = path)

  invisible(path)
}


#' Read an analysis problem from disk
#'
#' Reads a problem object from an RData file. The file must contain an object
#' named `problem`.
#'
#' @param path Path to problem.RData.
#'
#' @return A problem object.
#'
#' @keywords internal
read_analysis_problem <- function(path) {
  if (!file.exists(path)) {
    stop("Problem file does not exist.", call. = FALSE)
  }

  env <- new.env(parent = emptyenv())
  object_names <- load(path, envir = env)

  if (!"problem" %in% object_names) {
    stop(
      "Problem file must contain an object named 'problem'.",
      call. = FALSE
    )
  }

  problem <- get("problem", envir = env)

  if (!inherits(problem, "problem")) {
    stop(
      "Object named 'problem' does not inherit from class 'problem'.",
      call. = FALSE
    )
  }

  problem
}


#' Compare lightweight identity of two problem objects
#'
#' This checks whether two problem objects appear to describe the same problem
#' without requiring full object identity.
#'
#' @param x A problem object.
#' @param y A problem object.
#'
#' @return Logical scalar.
#'
#' @keywords internal
identical_problem_identity <- function(x, y) {
  if (!inherits(x, "problem") || !inherits(y, "problem")) {
    return(FALSE)
  }

  identical(x$label, y$label) &&
    identical(problem_type(x), problem_type(y)) &&
    identical(nrow(x$x), nrow(y$x)) &&
    identical(ncol(x$x), ncol(y$x)) &&
    identical(length(x$y), length(y$y)) &&
    identical(length(x$ids), length(y$ids)) &&
    identical(length(x$folds), length(y$folds))
}



#' Print a problem
#'
#' @param x A problem object.
#' @param ... Ignored.
#'
#' @export
print.problem <- function(x, ...) {

  cli::cli_text("{.field Label} : {.val {x$label}}")
  cli::cli_text("{.field Type}  : {.val {problem_type(x)}}")
  cli::cli_text("{.field Rows}  : {.val {nrow(x$x)}}")
  cli::cli_text("{.field Vars}  : {.val {ncol(x$x)}}")
  cli::cli_text("{.field Folds} : {.val {length(x$folds)}}")

  invisible(x)
}


#' Summarise a problem
#'
#' @param object A problem object.
#' @param ... Ignored.
#'
#' @export
summary.problem <- function(object, ...) {
  print(object)
}

#' @export
as.data.frame.problem <- function(x, ..., stringsAsFactors = FALSE) {
  cbind(y = data.frame(y = x$y),
        x$x)
}

#' Convert a problem-class object to rsample format
#' @param x a [problem] object
#' @param ... unused
#' @return a [rsample::rsample-package] rset object
#' @importFrom rsample make_splits
#' @export
as_rsample.problem <- function(x, ...) {
  df <- as.data.frame(x)
  folds <- x$folds
  n <- nrow(df)
  idx <- lapply(
    folds,
    \(f) {
      list(analysis = f,
           assessment = seq(1, n)[-f])
    }
  )
  splits <- lapply(idx, \(ids) rsample::make_splits(ids, data = df))
  rsample::manual_rset(splits, ids = names(folds))
}
