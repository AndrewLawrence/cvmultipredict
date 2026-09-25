
#' pool_results
#'
#' Calculates average performance metrics over cross-validation resamples.
#'     Input `x` should be the return of cvmultipredict.
#'     i.e. a named list of results data.frames - one for each model.
#'
#' @section Repetition Standard Errors:
#' These require sufficient repeats of cross-validation to estimate the
#' standard deviation over these repeats. Typically at least 10 x k-fold
#' with better `.rep_std_err` estimates at 100 x k-fold.
#'
#' It is a bad idea to combine `na.rm=TRUE` and
#' `use_repetition=TRUE`. In the event that some models are returning `NA` values
#' (e.g. failing to converge) then the per-repetition standard deviation
#' will not cover all the folds for each repetition and the
#' `.rep_std_err` will not have the desired interpretation. `NA`s should either
#' propagate (`na.rm = FALSE`) or the fold-fold `.std_err` be used.
#'
#' @param x [cvmultipredict] result.
#' @inheritParams base::mean na.rm
#' @param use_repetition Return an additional SD estimate (`.rep_std_err`)
#'     calculated for repetitions of k-fold CV rather than over individual folds.
#' @param repetition_regexp A [regular expression] used to extract unique
#'     identifiers of each CV-repetition from cross-validation labels.
#'     For example, the default regexp `"Rep[0-9]+"` matches substrings
#'     like `"Rep001"`, `"Rep01"`, or `"Rep1"`, and so works with labels
#'     formatted like `"Rep001.Fold01"`.
#'
#' @return A tibble containing columns:
#'
#'  * `model`: label for the model.
#'  * `cvtype`: either `cv` or `app` indicating if estimate is cross-validated
#'         or apparent.
#'  * `.metric`: the [metric][yardstick::metrics].
#'  * `.estimate`: metric value (for CV averaged over resamples)
#'  * `.std_err`: For cross-validated values, contains the SD of the
#'      metric value (`NA` for apparent performance).
#'  * `.nreps` : For repeated k-fold, when `use_repetition=TRUE` will
#'      contain the number of repetitions detected.
#'  * `.rep_std_err` : When `use_repetition=TRUE`the standard deviation
#'      of the metric taken after averaging over the k-folds in each
#'      repetition - i.e. a variability measure for the random division
#'      into folds which does not include fold-fold variability.
#' @export
pool_results <- function(x,
                         na.rm = FALSE,
                         use_repetition = FALSE,
                         repetition_regexp = "Rep[0-9]+") {
  check_suggested("tidyr", "Results Pooling")

  if (!inherits(x, "data.frame")) {
    x <- bind_rows(x, .id = "model")
  }

  # retain label until repetition summaries are computed
  x <- x |>
    dplyr::mutate(cvtype = factor(
      ifelse(.data$label == "apparent", "app", "cv"),
      levels = c("app", "cv")
    ))

  out <- x |>
    tidyr::nest(.by = c("model", "cvtype", ".metric")) |>
    dplyr::mutate(
      .estimate = purrr::map_dbl(.data$data, ~ mean(.x$.estimate, na.rm = na.rm)),
      .std_err = purrr::map_dbl(.data$data, ~ sd(.x$.estimate, na.rm = na.rm))
    )

  if (use_repetition) {

    out <- out |>
      dplyr::mutate(
        .nreps = purrr::map_int(.data$data, function(df) {
          df <- dplyr::filter(df, .data$label != "apparent")

          if (nrow(df) == 0) {
            return(NA_integer_)
          }

          reps <- stringr::str_extract(df$label, repetition_regexp)

          dplyr::n_distinct(reps)
        }),

        .rep_std_err = purrr::map_dbl(.data$data, function(df) {
          df <- dplyr::filter(df, .data$label != "apparent")

          if (nrow(df) == 0) {
            return(NA_real_)
          }

          df <- df |>
            dplyr::mutate(.rep = stringr::str_extract(.data$label,
                                                      repetition_regexp)) |>
            dplyr::group_by(.data$.rep) |>
            dplyr::summarise(.estimate = mean(.data$.estimate, na.rm = na.rm),
                             .groups = "drop")

          stats::sd(df$.estimate, na.rm = na.rm)
        })
      )
  }

  out |>
    dplyr::select(-"data")
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
  check_suggested("ggplot2", "Plotting")
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
#' @param highlight_bayes a flag, if `TRUE` then bayesian optimisation
#'     steps get a colour scale.
#' @param highlight_best a flag, if `TRUE` then the "best" hyperparameter
#'     combination is highlighted with a red circle.
#' @param alpha transparency used for plotting.
#' @param best_function a function applied to a numeric vector (including NAs)
#'     that returns the "best" value (usually min or max). Must ignore NA values.
#' @return A [ggplot object][ggplot2::ggplot]
#' @export
plot_tuning_results <- function(x,
                                se = FALSE,
                                highlight_bayes = TRUE,
                                highlight_best = TRUE,
                                alpha = 1.0,
                                best_function = \(x) min(x, na.rm = TRUE)) {
  check_suggested("tidyr", "Plotting")
  check_suggested("ggplot2", "Plotting")
  checkmate::assert_data_frame(x, col.names = "named")
  checkmate::assert_subset(c(".metric", "mean"), colnames(x))
  checkmate::assert_flag(se)

  idx <- which(colnames(x) == ".metric") - 1
  if (idx == 0L) {
    cli::cli_abort("problem with dataset names")
  }
  # log transform certain hyperparameters
  hps_to_log <- c("learn_rate", "loss_reduction")

  for (i in hps_to_log) {
    if (i %in% colnames(x)) {
      x[[i]] <- log10(x[[i]])
    }
  }
  # NA-hack for colour scale if highlight_bayes:
  if (".iter" %in% colnames(x) && highlight_bayes) {
    x$.iter <- ifelse(x$.iter == 0L, NA, x$.iter)
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
    p <- p + ggplot2::geom_errorbar(alpha = alpha)
  }

  if (".iter" %in% colnames(x) && highlight_bayes) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(colour = .data$.iter),
                          alpha = alpha) +
      ggplot2::scale_colour_gradient(na.value = "black",
                                     high = "lightblue",
                                     low = "darkblue")
  } else {
    p <- p + ggplot2::geom_point(alpha = alpha)
  }
  if ( highlight_best ) {
    best_sel <- (x[["mean"]] - best_function(x[["mean"]])) <
      (2 * .Machine$double.eps)
    best <- x[best_sel, ] |>
      tidyr::pivot_longer(cols = 1:idx,
                          names_to = "hyperparameter",
                          values_to = "hyperparameter_value")
    p <- p +
      ggplot2::geom_point(data = best,
                          colour = "red",
                          size = 3,
                          shape = 21)
  }
  p <- p +
    ggplot2::facet_wrap( ~ .data$hyperparameter, scales = "free_x")
  p
}
