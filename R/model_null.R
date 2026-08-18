# null (intercept-only) models for reference metric values


#' @keywords internal
nullpreds_regression <- function(
  y,
  na.rm = FALSE,
  event_level = "second"
) {
  data.frame(.pred = mean(y, na.rm = na.rm),
             y = y,
             label = "null")
}

#' @keywords internal
nullpreds_classification <- function(
  y,
  na.rm = FALSE,
  event_level = "second"
) {
  event_level <- match.arg(event_level, choices = c("first", "second"))
  tgt <- ifelse(event_level == "second", levels(y)[[2]], levels(y)[[1]])

  data.frame(.pred = mean(as.numeric(y == tgt), na.rm = na.rm),
             y = y,
             label = "null")
}


#' nullmetrics
#'
#' Produces the apparent performance metrics obtained from an
#'     "intercept-only" null model for a numeric (regression) or
#'     factor (classification) vector.
#'
#' @param x a vector, either numeric (regression) or factor (classification)
#' @param metric_set a [metric_set][yardstick::metric_set] to apply to x.
#'     Note: default (NULL) uses the default sets associated with
#'     [cvmultipredict].
#' @inheritParams base::mean na.rm
#' @param event_level (for classification) which level of x is the target?
#'     "first" | "second"
#' @param label sets contents of label column in the returned results
#'
#' @return A tibble containing `label`, `.metric`, `.estimator` and
#'     `.estimate` columns.
#'
#' @export
nullmetrics <- function(
  x,
  metric_set = NULL,
  na.rm = FALSE,
  event_level = "second",
  label = "null"
) {
  if (is.factor(x)) {
    p <- nullpreds_classification(x, na.rm = na.rm)
    if ( is.null(metric_set) ) {
      metric_set <- metricset_classification()
    }
  } else {
    p <- nullpreds_regression(x, na.rm = na.rm)
    if ( is.null(metric_set) ) {
      metric_set <- metricset_regression()
    }
  }
  p <- p |>
    metric_set(truth = "y", ".pred", event_level = "second") |>
    suppressWarnings()

  # special handling for rsq (non-traditional)
  sel <- p[[".metric"]] == "rsq"
  if ( any(sel) ) {
    p[[".estimate"]][sel] <- 0.0
  }

  dplyr::mutate(p, label = label, .before = 1L)
}
