
null_model_mse_split <- function(x, train, test, ...) {
  UseMethod("null_model_mse_split")
}

#' @export
null_model_mse_split.regression <- function(x, train, test, ...) {
  y <- x$y

  y_hat <- mean(y[train])
  mean((y[test] - y_hat)^2)
}

#' @export
null_model_mse_split.classification <- function(x, train, test, event_level = NULL, ...) {
  y <- outcome01(x, event_level = event_level)

  y_hat <- mean(y[train])
  mean((y[test] - y_hat)^2)
}


#' Null model performance
#'
#' Calculates null model mean squared error using either apparent validity
#' or cross-validation folds stored in the problem object.
#'
#' @param x A problem object.
#' @param validity Either "apparent" or "cv".
#' @param combine Function used to combine split-level estimates.
#' @param ... Additional arguments passed to methods.
#'
#' @return Numeric scalar.
#'
#' @export
null_model_mse <- function(
    x,
    validity = c("apparent", "cv"),
    combine = combine_mean,
    ...
) {
  UseMethod("null_model_mse")
}


#' @export
null_model_mse.problem <- function(
    x,
    validity = c("apparent", "cv"),
    combine = combine_mean,
    ...
) {
  estimate_by_validity(
    x = x,
    validity = validity,
    estimator = null_model_mse_split,
    combine = combine,
    ...
  )
}
