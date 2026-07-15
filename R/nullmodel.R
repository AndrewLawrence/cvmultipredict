#' Null model performance
#'
#' Calculates the performance of a null model for a problem.
#'
#' For regression, the null model predicts the mean of y for every observation
#' and returns the mean squared error.
#'
#' For classification, the null model predicts p(event) at the prevalence and
#' returns the mean squared error (i.e. updated Brier score)
#'
#' @param x A problem object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Numeric scalar.
#'
#' @export
null_model_mse <- function(x, ...) {
  UseMethod("null_model_mse")
}

#' @export
null_model_mse.regression <- function(x, ...) {
  y <- x$y
  y_hat <- mean(y)
  mean((y - y_hat)^2)
}

#' @export
null_model_mse.classification <- function(x, ...) {
  y <- as.numeric(x$y) - 1
  y_hat <- mean(y)
  mean((y - y_hat)^2)
}
