
#' pool_results
#'
#' Calculates average performance metrics over cross-validation resamples.
#'     Input `x` should be the return of cvmultipredict.
#'     i.e. a named list of results data.frames - one for each model.
#'
#' @param x [cvmultipredict] result.
#' @inheritParams base::mean na.rm
#'
#' @return A tibble containing columns:
#'
#'  * `model`: label for the model.
#'  * `cvtype`: either `cv` or `app` indicating if estimate is cross-validated
#'         or apparent.
#'  * `.metric`: the [metric][yardstick::metrics].
#'  * `.estimate`: metric value (for CV averaged over resamples)
#'  * `.std_err`: SD of the metric value (`NA` for apparent performance)
#'
#' @importFrom tidyr nest
#' @export
pool_results <- function(x, na.rm = FALSE) {
  if (!inherits(x, "data.frame")) {
    x <- bind_rows(x, .id = "model")
  }
  x |>
    dplyr::mutate(cvtype = factor(
      ifelse(.data$label == "apparent", "app", "cv"),
      levels = c("app", "cv")
    )) |>
    dplyr::select(-.data$label, -.data$.estimator) |>
    tidyr::nest(.by = c(.data$model, .data$cvtype, .data$.metric)) |>
    dplyr::mutate(
      .estimate = purrr::map_dbl(.data$data, ~ mean(.x$.estimate, na.rm = na.rm)),
      .std_err = purrr::map_dbl(.data$data, ~ sd(.x$.estimate, na.rm = na.rm))
    ) |>
    dplyr::select(-.data$data)
}

#' plot_pooled_results
#'
#' a [ggplot][ggplot2::ggplot] linking the Apparent and (mean)
#'     cross-validated performance of each metric.
#'
#' @param x result of [pool_results]
#'
#' @return A [ggplot object][ggplot2::ggplot]
#'
#' @export
plot_pooled_results <- function(x) {
  p <- x |>
    ggplot2::ggplot(ggplot2::aes(y = .data$.estimate,
                                 x = .data$cvtype,
                                 ymin = .data$.estimate - 2 * .data$.std_err,
                                 ymax = .data$.estimate + 2 * .data$.std_err,
                                 group = .data$model,
                                 colour = .data$model)) +
    ggplot2::geom_line() +
    ggplot2::geom_errorbar() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ .data$.metric, scales = "free_y")
  p
}



#' plot_tuning_results
#'
#' a [ggplot][ggplot2::ggplot] showing hyperparameter tuning performance.
#'
#' @param x tuning results, either from [tune_grid][tune::tune_grid],
#'     or logged to disk by a [cvmultipredict] model.
#' @param se - flag: include error bars for +/- 1SD of performance?
#'     SD is calculated over the tuning CV resamples.
#'
#' @return A [ggplot object][ggplot2::ggplot]
#' @importFrom tidyr pivot_longer
#' @export
plot_tuning_results <- function(x, se = FALSE) {
  checkmate::assert_data_frame(x, col.names = "named")
  checkmate::assert_subset(c(".metric", "mean"), colnames(x))
  checkmate::assert_flag(se)

  idx <- which(colnames(x) == ".metric") - 1
  if (idx == 1L) {
    cli::cli_abort("problem with dataset names")
  }
  # log transform certain hyperparameters
  hps_to_log <- c("learn_rate", "loss_reduction")

  for (i in hps_to_log) {
    if (i %in% colnames(x)) {
      x[[i]] <- log10(x[[i]])
    }
  }

  p <- x |>
    tidyr::pivot_longer(cols = 1:idx,
                        names_to = "hyperparameter",
                        values_to = "hyperparameter_value") |>
    ggplot2::ggplot(ggplot2::aes(y = .data$mean,
                                 x = .data$hyperparameter_value,
                                 ymin = .data$mean - .data$std_err,
                                 ymax = .data$mean + .data$std_err))

  if ( se ) {
    p <- p + ggplot2::geom_errorbar()
  }

  if (".iter" %in% colnames(x)) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(colour = .data$.iter))
  } else {
    p <- p + ggplot2::geom_point()
  }

  p <- p +
    ggplot2::facet_wrap( ~ .data$hyperparameter, scales = "free_x")
  p
}
