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


#' Construct a machine-learning problem
#'
#' Creates an object describing a supervised learning problem and the
#' associated resampling scheme.
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
#'
#' @return An object of class c(ytype, "problem").
#'
#' @export
problem <- function(label, ytype, y, x, ids, folds) {
  new_problem(
    label = label,
    ytype = ytype,
    y = y,
    x = x,
    ids = ids,
    folds = folds
  ) |>
    validate_problem()
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


#' Print a problem
#'
#' @param x A problem object.
#' @param ... Ignored.
#'
#' @export
print.problem <- function(x, ...) {
  cat("\n")
  cat("Label:", x$label, "\n")
  cat("Type :", problem_type(x), "\n")
  cat("Rows :", nrow(x$x), "\n")
  cat("Vars :", ncol(x$x), "\n")
  cat("Folds:", length(x$folds), "\n")

  invisible(x)
}


#' Summarise a problem
#'
#' @param object A problem object.
#' @param ... Ignored.
#'
#' @export
summary.problem <- function(object, ...) {
  out <- list(
    label = object$label,
    ytype = problem_type(object),
    n = nrow(object$x),
    p = ncol(object$x),
    n_folds = length(object$folds)
  )

  class(out) <- c(
    paste0("summary.", problem_type(object)),
    "summary.problem"
  )

  out
}


#' @export
print.summary.problem <- function(x, ...) {
  cat("Problem summary\n")
  cat("----------------\n")
  cat("Label   :", x$label, "\n")
  cat("Type    :", x$ytype, "\n")
  cat("Rows    :", x$n, "\n")
  cat("Features:", x$p, "\n")
  cat("Folds   :", x$n_folds, "\n")

  invisible(x)
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
