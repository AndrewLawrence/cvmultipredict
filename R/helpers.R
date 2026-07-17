#' Arithmetic mean combiner
#'
#' @param x Numeric vector.
#' @param na.rm Logical scalar.
#'
#' @return Numeric scalar.
#'
#' @export
combine_mean <- function(x, na.rm = FALSE) {
  mean(unlist(x), na.rm = na.rm)
}


#' Transformed-scale combiner
#'
#' @param transform Function applied before summarising.
#' @param inverse Function applied after summarising.
#' @param summary Function used to summarise transformed values.
#' @param ... passed to summary
#' @return A function.
#'
#' @export
combine_transformed <- function(transform, inverse, summary = mean, ...) {
  force(transform)
  force(inverse)
  force(summary)

  function(x, ...) {
    inverse(summary(transform(unlist(x)), ...))
  }
}


#' Fisher-z combiner
#'
#' @param x Numeric vector of correlations.
#' @param ... Additional arguments passed to mean.
#'
#' @return Numeric scalar.
#'
#' @export
combine_fisherz <- combine_transformed(
  transform = atanh,
  inverse = tanh,
  summary = mean
)

# Reusable wrapper function:
#   x : a problem object.
#   validity : apparent | cv
#   estimator : a function like null_model_mse
estimate_by_validity <- function(x, validity, estimator, combine, ...) {
  validity <- match.arg(validity, c("apparent", "cv"))

  n <- length(x$y)
  all_idx <- seq_len(n)

  estimates <- switch(
    validity,

    apparent = {
      list(estimator(x, train = all_idx, test = all_idx, ...))
    },

    cv = {
      if (length(x$folds) == 0) {
        stop("Cannot use validity = 'cv' because x$folds is empty.",
             call. = FALSE)
      }

      lapply(
        x$folds,
        function(train) {
          test <- setdiff(all_idx, train)

          if (length(test) == 0) {
            stop("At least one fold has no held-out observations.",
                 call. = FALSE)
          }

          estimator(x, train = train, test = test, ...)
        }
      )
    }
  )

  combine(estimates)
}

# Converts a factor to 0-1 numeric
outcome01 <- function(x, event_level = NULL) {
  stopifnot(inherits(x, "classification"))

  y <- x$y

  if (is.null(event_level)) {
    if (nlevels(y) != 2) {
      stop("Classification null MSE currently requires a two-level factor.",
           call. = FALSE)
    }

    return(as.numeric(y) - 1)
  }

  if (!event_level %in% levels(y)) {
    stop("event_level must be one of levels(x$y).", call. = FALSE)
  }

  as.integer(y == event_level)
}
