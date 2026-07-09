# Devnotes: a problem is a list containing a particular structure...

new_problem <- function(label, ytype, y, x, ids, folds) {

  structure(
    list(
      label = label,
      ytype = ytype,
      y = y,
      x = x,
      ids = ids,
      folds = folds
    ),
    class = "problem"
  )
}

#' Construct a machine-learning problem
#'
#' Creates an object of class `"problem"` describing a supervised
#' learning problem and the associated resampling scheme.
#'
#' A problem consists of:
#'
#' * `label` - unique problem identifier
#' * `ytype` - target type (`"regression"` or `"classification"`)
#' * `y` - response vector
#' * `x` - predictor data frame
#' * `ids` - unique row identifiers
#' * `folds` - repeated cross-validation folds, typically generated
#'   with [caret::createMultiFolds()]
#'
#' @param label Character scalar identifying the problem.
#' @param ytype Character scalar giving the response type:
#'   `"regression"` or `"classification"`.
#' @param y Response vector. Numeric for regression or factor for
#'   classification.
#' @param x Data frame of predictors.
#' @param ids Character vector of row identifiers.
#' @param folds List of training indices, typically produced by
#'   [caret::createMultiFolds()].
#'
#' @return An object of class `"problem"`.
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


#' Validate a problem object
#'
#' Internal validator for objects of class `"problem"`.
#'
#' @param x A problem object.
#'
#' @return The validated object, invisibly.
#'
#' @keywords internal
validate_problem <- function(x) {

  stopifnot(inherits(x, "problem"))

  if (!is.character(x$label) || length(x$label) != 1) {
    stop("label must be a character scalar.", call. = FALSE)
  }

  if (!x$ytype %in% c("regression", "classification")) {
    stop(
      "ytype must be 'regression' or 'classification'.",
      call. = FALSE
    )
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

  if (x$ytype == "regression" && !is.numeric(x$y)) {
    stop(
      "For regression problems y must be numeric.",
      call. = FALSE
    )
  }

  if (x$ytype == "classification" && !is.factor(x$y)) {
    stop(
      "For classification problems y must be a factor.",
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

  cat("<problem>\n")
  cat("Label:", x$label, "\n")
  cat("Type :", x$ytype, "\n")
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
    ytype = object$ytype,
    n = nrow(object$x),
    p = ncol(object$x),
    n_folds = length(object$folds)
  )

  class(out) <- "summary.problem"
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

