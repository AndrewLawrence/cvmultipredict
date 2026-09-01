# dev notes:
#   a "neuralnet" will be an MLP with 2 hidden layers, bias parameters in
#   each layer and tuning over:
#     hidden_layers [h1, h2] (number of neurons in h1 and h2)
#     epochs
#     learn_rate
#     n_selected (selection step using rf importance)
#   as this is again 5 parameters we will use 3^5 = 243 space filling
#     (i.e. entropy_max) hyperparameter combinations + bayesian optimisation
#     as with xgboost.

#' This is a neuralnetwork model 2-layer multilayer perceptron
#' @inheritParams xgboost
#' @param tunevals_hidden_layers tuning values for hidden_layers.
#'     default: 4,8,16,32,64.
#'     *Used for h1 and h2 crossed. s.t. tuning includes h1 > h2 and h2 > h1.*
#' @param tunevals_n_selected tuning values (discrete) for number of variables to
#'     include (filtered by [rf importance][step_selectbyrfimp]).
#' @param tunerange_epochs tuning range for [epochs][dials::epochs].
#'     default: 10 : 1000
#' @param tunerange_learn_rate tuning range for [learn_rate][dials::learn_rate].
#'     default 1e-10 : 0.1
#'
#' @rdname neuralnet
#' @export
neuralnet <- function(x,
                      folder,
                      tuning_grid_n = 243,
                      tunevals_hidden_layers = 2^(2:6),
                      tunevals_n_selected = seq(4, 12, by = 2),
                      tunerange_epochs = c(10L, 1000L),
                      tunerange_learn_rate = c(-10, -1),
                      tuning_bayes_maxit = 50L,
                      tuning_bayes_minit = 10L,
                      tuning_resample_fxn = "vfold_cv",
                      tuning_resample_args = list(v = 10,
                                                  repeats = 1,
                                                  strata = "y"),
                      check_futility = TRUE,
                      metricset = NULL,
                      .RUN = TRUE,
                      ...) {
  UseMethod("neuralnet")
}

#' @rdname neuralnet
#' @export
neuralnet.default <- function(x,
                              folder,
                              tuning_grid_n = 243,
                              tunevals_hidden_layers = 2^(2:6),
                              tunevals_n_selected = seq(4, 12, by = 2),
                              tunerange_epochs = c(10L, 1000L),
                              tunerange_learn_rate = c(-10, -1),
                              tuning_bayes_maxit = 50L,
                              tuning_bayes_minit = 10L,
                              tuning_resample_fxn = "vfold_cv",
                              tuning_resample_args = list(v = 10,
                                                          repeats = 1,
                                                          strata = "y"),
                              check_futility = TRUE,
                              metricset = NULL,
                              .RUN = TRUE,
                              ...) {
  stop("Not implemented for classes apart from analysis")
}

#' @rdname neuralnet
#' @importFrom stats complete.cases predict update
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom recipes recipe add_step all_predictors step_pca
#' @importFrom parsnip boost_tree fit
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang .data
#' @importFrom tune finalize_workflow select_best tune_grid
#' @importFrom tune collect_metrics control_grid tune
#' @importFrom dials epochs learn_rate grid_space_filling finalize
#' @export
neuralnet.regression_analysis <-  function(
  x,
  folder,
  tuning_grid_n = 243,
  tunevals_hidden_layers = 2^(2:6),
  tunevals_n_selected = seq(4, 12, by = 2),
  tunerange_epochs = c(10L, 1000L),
  tunerange_learn_rate = c(-10, -1),
  tuning_bayes_maxit = 50L,
  tuning_bayes_minit = 10L,
  tuning_resample_fxn = "vfold_cv",
  tuning_resample_args = list(v = 10, repeats = 1, strata = "y"),
  check_futility = TRUE,
  metricset = metricset_regression(),
  .RUN = TRUE,
  ...
) {
  check_suggested("kindling", "neural network models")
  checkmate::assert_flag(.RUN)
  checkmate::assert_int(tuning_grid_n, lower = 0L)
  checkmate::assert_int(tuning_bayes_maxit, lower = 0L)
  checkmate::assert_int(tuning_bayes_minit, lower = 0L)
  tunevals_hidden_layers <- unlist(tunevals_hidden_layers)
  tunevals_n_selected <- unlist(tunevals_n_selected)
  tunerange_epochs <- unlist(tunerange_epochs)
  tunerange_learn_rate <- unlist(tunerange_learn_rate)
  checkmate::assert_integer(tunerange_epochs,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_numeric(tunerange_learn_rate,
                            min.len = 1L,
                            max.len = 2L)
  checkmate::assert_numeric(tunevals_hidden_layers,
                            min.len = 1L)
  checkmate::assert_true(
    all(tunevals_hidden_layers == as.integer(tunevals_hidden_layers))
  )
  checkmate::assert_true(
    all(tunevals_n_selected == as.integer(tunevals_n_selected))
  )

  DO_BAYES <- TRUE
  if ( tuning_bayes_maxit == 0L ) {
    DO_BAYES <- FALSE
  }

  # ~ extract from problem --------------------------------------------------
  df <- as.data.frame(problem(x))
  folds <- problem(x)[["folds"]]

  # ~ setup locations -------------------------------------------------------
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  wf_loc <- file.path(folder, "workflow.RData")
  fm_loc <- file.path(folder, "finalmodel.RData")
  fold_locs <- file.path(folder, paste0(names(folds), ".RData"))
  results_loc <- file.path(folder, "results.RData")

  # ~ workflow --------------------------------------------------------------
  if ( file.exists(wf_loc) ) {
    load(wf_loc)
  } else {

    wf <- workflows::workflow()

    rp <- recipes::recipe(y ~ ., data = df) |>
      step_selectbyrfimp(all_predictors(),
                         n_selected = tune_or_fix_discrete(tunevals_n_selected))

    model <- do.call(
      kindling::mlp_kindling,
      list(
        engine = "kindling",
        mode = "regression",
        optimizer = "adam",
        activations = c("relu", "relu"),
        hidden_neurons = tune_or_fix_discrete(tunevals_hidden_layers),
        epochs = tune_or_fix_range(tunerange_epochs),
        learn_rate = tune_or_fix_range(tunerange_learn_rate)
      )
    )

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    param_info <- parameters(
      kindling::hidden_neurons(disc_values = tunevals_hidden_layers),
      dials::epochs(range = tunerange_epochs),
      learn_rate(range = tunerange_learn_rate),
      rf_n_selected(disc_values = tunevals_n_selected)
    ) |>
      finalize(x = df[, -1])

    # Initial tuning grid:
    tuning_grid <- kindling::grid_depth(param_info,
                                        n_hlayer = 2L,
                                        type = "max_entropy",
                                        size = tuning_grid_n)

    # separate control objects for grid and bayes:
    tune_ctrl <- tune::control_grid(event_level = "second", verbose = TRUE)
    tune_ctrl_bayes <- tune::control_bayes(event_level = "second",
                                           verbose = TRUE,
                                           no_improve = tuning_bayes_minit)

    save(wf, tuning_grid, tune_ctrl, tune_ctrl_bayes, param_info, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( file.exists(fm_loc) ) {
    cli::cli_alert_info("neuralnet loading: final model")
    load(fm_loc)
  } else {
    if ( .RUN ) {
      cli::cli_alert_info("neuralnet tuning: final model")

      # make a resampling object:
      resample_args <- append(list(data = df), tuning_resample_args)
      rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                    resample_args)
      # run grid tuning:
      tuning <- tune::tune_grid(wf,
                                resamples = rs,
                                grid = tuning_grid,
                                metrics = metric_set(mse),
                                control = tune_ctrl)
      # Bayesian fine-tuning:
      if ( DO_BAYES ) {
        tuning <- tune::tune_bayes(wf,
                                   resamples = rs,
                                   initial = tuning,
                                   iter = tuning_bayes_maxit,
                                   param_info = param_info,
                                   metrics = metric_set(mse),
                                   control = tune_ctrl_bayes)
      }
      # fix workflow:
      fwf <- tune::finalize_workflow(wf,
                                     parameters = tune::select_best(tuning,
                                                                    metric = "mse"))
      # gather tuning results for analytics
      tuning_results <- tune::collect_metrics(tuning)

      # fit the final model:
      cli::cli_alert_info("neuralnet running: final model")
      m <- fit(fwf, data = df)

      # log to disk:
      save(m, tuning_results, file = fm_loc)
    } else {
      # error:
      msg_norun_mainmissing()
    }
  }
  # extract predictions:
  preds <- predict(m, new_data = df, type = "numeric") |>
    bind_cols(y = df$y) |>
    bind_cols(label = "apparent")

  # futility check - i.e. if the tuned model is
  #               intercept only then don't cross-validate.
  if ( check_futility ) {
    FUTILITY <- futility_check_neuralnet(
      x = preds,
      y = df$y,
      tuning = tuning_results,
      type = "regression"
    )
  } else {
    FUTILITY <- FALSE
  }

  # ~ fold models -----------------------------------------------------------
  if ( !FUTILITY ) {
    lapply(seq.int(length(folds)), function(i) {
      fname <- fold_locs[[i]]
      fids <- folds[[i]]
      flab <- names(folds)[[i]]
      if ( file.exists(fname) ) {

        msg_cv_loading(model = "neuralnet",
                       id = i,
                       nid = length(folds))
        load(fname)
      } else {
        if ( .RUN ) {
          msg_cv_running(model = "neuralnet",
                         id = i,
                         nid = length(folds))

          # make a resampling object:
          f_resample_args <- append(list(data = df[fids, ]), tuning_resample_args)

          f_rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                          f_resample_args)
          # run grid tuning:
          f_tuning <- tune::tune_grid(wf,
                                      resamples = f_rs,
                                      grid = tuning_grid,
                                      metrics = metric_set(mse),
                                      control = tune_ctrl)

          # Bayesian fine-tuning:
          if ( DO_BAYES ) {
            f_tuning <- tune::tune_bayes(wf,
                                         resamples = f_rs,
                                         initial = f_tuning,
                                         iter = tuning_bayes_maxit,
                                         param_info = param_info,
                                         metrics = metric_set(mse),
                                         control = tune_ctrl_bayes)
          }
          # fix workflow:
          f_wf <- tune::finalize_workflow(
            wf,
            parameters = tune::select_best(f_tuning, metric = "mse")
          )

          # gather tuning results for analytics
          f_tuning_results <- tune::collect_metrics(f_tuning)

          # fit the final model:
          m <- fit(f_wf, data = df[fids, ])
          save(m, f_tuning_results, file = fname)
        }
      }
      # collate results:
      if ( exists("m") ) {
        preds <<- dplyr::bind_rows(
          preds,
          predict(m, new_data = df[-fids, ], type = "numeric") |>
            bind_cols(y = df[-fids, "y"]) |>
            bind_cols(label = flab)
        )
      }
    })
  }
  # ~ metrics ---------------------------------------------------------------
  results <- preds |>
    dplyr::group_by(.data$label) |>
    metricset(truth = "y", ".pred", event_level = "second")

  # ~ end -------------------------------------------------------------------
  save(preds, results, file = results_loc)
  invisible(results)
}

#' @rdname neuralnet
#' @export
neuralnet.classification_analysis <-  function(
  x,
  folder,
  tuning_grid_n = 243,
  tunevals_hidden_layers = 2^(2:6),
  tunevals_n_selected = seq(4, 12, by = 2),
  tunerange_epochs = c(10L, 1000L),
  tunerange_learn_rate = c(-10, -1),
  tuning_bayes_maxit = 50L,
  tuning_bayes_minit = 10L,
  tuning_resample_fxn = "vfold_cv",
  tuning_resample_args = list(v = 10, repeats = 1, strata = "y"),
  check_futility = TRUE,
  metricset = metricset_classification(),
  .RUN = TRUE,
  ...
) {
  check_suggested("kindling", "neural network models")
  checkmate::assert_flag(.RUN)
  checkmate::assert_int(tuning_grid_n, lower = 0L)
  checkmate::assert_int(tuning_bayes_maxit, lower = 0L)
  checkmate::assert_int(tuning_bayes_minit, lower = 0L)
  tunevals_hidden_layers <- unlist(tunevals_hidden_layers)
  tunevals_n_selected <- unlist(tunevals_n_selected)
  tunerange_epochs <- unlist(tunerange_epochs)
  tunerange_learn_rate <- unlist(tunerange_learn_rate)
  checkmate::assert_integer(tunerange_epochs,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_numeric(tunerange_learn_rate,
                            min.len = 1L,
                            max.len = 2L)
  checkmate::assert_numeric(tunevals_hidden_layers,
                            min.len = 1L)
  checkmate::assert_true(
    all(tunevals_hidden_layers == as.integer(tunevals_hidden_layers))
  )
  checkmate::assert_true(
    all(tunevals_n_selected == as.integer(tunevals_n_selected))
  )

  DO_BAYES <- TRUE
  if ( tuning_bayes_maxit == 0L ) {
    DO_BAYES <- FALSE
  }

  # ~ extract from problem --------------------------------------------------
  df <- as.data.frame(problem(x))
  folds <- problem(x)[["folds"]]

  # ~ setup locations -------------------------------------------------------
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  wf_loc <- file.path(folder, "workflow.RData")
  fm_loc <- file.path(folder, "finalmodel.RData")
  fold_locs <- file.path(folder, paste0(names(folds), ".RData"))
  results_loc <- file.path(folder, "results.RData")

  # ~ workflow --------------------------------------------------------------
  if ( file.exists(wf_loc) ) {
    load(wf_loc)
  } else {

    wf <- workflows::workflow()

    rp <- recipes::recipe(y ~ ., data = df) |>
      step_selectbyrfimp(all_predictors(),
                         n_selected = tune_or_fix_discrete(tunevals_n_selected))

    model <- do.call(
      kindling::mlp_kindling,
      list(
        engine = "kindling",
        mode = "classification",
        optimizer = "adam",
        activations = c("relu", "relu"),
        hidden_neurons = tune_or_fix_discrete(tunevals_hidden_layers),
        epochs = tune_or_fix_range(tunerange_epochs),
        learn_rate = tune_or_fix_range(tunerange_learn_rate)
      )
    )

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    param_info <- parameters(
      kindling::hidden_neurons(disc_values = tunevals_hidden_layers),
      dials::epochs(range = tunerange_epochs),
      learn_rate(range = tunerange_learn_rate),
      rf_n_selected(disc_values = tunevals_n_selected)
    ) |>
      finalize(x = df[, -1])

    # Initial tuning grid:
    tuning_grid <- kindling::grid_depth(param_info,
                                        n_hlayer = 2L,
                                        type = "max_entropy",
                                        size = tuning_grid_n)

    # separate control objects for grid and bayes:
    tune_ctrl <- tune::control_grid(event_level = "second", verbose = TRUE)
    tune_ctrl_bayes <- tune::control_bayes(event_level = "second",
                                           verbose = TRUE,
                                           no_improve = tuning_bayes_minit)

    save(wf, tuning_grid, tune_ctrl, tune_ctrl_bayes, param_info, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( file.exists(fm_loc) ) {
    cli::cli_alert_info("neuralnet loading: final model")
    load(fm_loc)
  } else {
    if ( .RUN ) {
      cli::cli_alert_info("neuralnet tuning: final model")

      # make a resampling object:
      resample_args <- append(list(data = df), tuning_resample_args)
      rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                    resample_args)
      # run grid tuning:
      tuning <- tune::tune_grid(wf,
                                resamples = rs,
                                grid = tuning_grid,
                                metrics = metric_set(brier_class),
                                control = tune_ctrl)
      # Bayesian fine-tuning:
      if ( DO_BAYES ) {
        tuning <- tune::tune_bayes(wf,
                                   resamples = rs,
                                   initial = tuning,
                                   iter = tuning_bayes_maxit,
                                   param_info = param_info,
                                   metrics = metric_set(brier_class),
                                   control = tune_ctrl_bayes)
      }
      # fix workflow:
      fwf <- tune::finalize_workflow(
        wf,
        parameters = tune::select_best(tuning,
                                       metric = "brier_class")
      )
      # gather tuning results for analytics
      tuning_results <- tune::collect_metrics(tuning)

      # fit the final model:
      cli::cli_alert_info("neuralnet running: final model")
      m <- fit(fwf, data = df)

      # log to disk:
      save(m, tuning_results, file = fm_loc)
    } else {
      # error:
      msg_norun_mainmissing()
    }
  }
  # extract predictions:
  preds <- predict(m, new_data = df, type = "prob") |>
    dplyr::select(-1) |>
    setNames(".pred") |>
    bind_cols(y = df$y) |>
    bind_cols(label = "apparent")

  # futility check - i.e. if the tuned model is
  #               intercept only then don't cross-validate.
  if ( check_futility ) {
    FUTILITY <- futility_check_neuralnet(x = preds,
                                         y = df$y,
                                         tuning = tuning_results,
                                         type = "classification")
  } else {
    FUTILITY <- FALSE
  }

  # ~ fold models -----------------------------------------------------------
  if ( !FUTILITY ) {
    lapply(seq.int(length(folds)), function(i) {
      fname <- fold_locs[[i]]
      fids <- folds[[i]]
      flab <- names(folds)[[i]]
      if ( file.exists(fname) ) {

        msg_cv_loading(model = "neuralnet",
                       id = i,
                       nid = length(folds))
        load(fname)
      } else {
        if ( .RUN ) {

          msg_cv_running(model = "neuralnet",
                         id = i,
                         nid = length(folds))

          # make a resampling object:
          f_resample_args <- append(list(data = df[fids, ]), tuning_resample_args)

          f_rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                          f_resample_args)
          # run grid tuning:
          f_tuning <- tune::tune_grid(wf,
                                      resamples = f_rs,
                                      grid = tuning_grid,
                                      metrics = metric_set(brier_class),
                                      control = tune_ctrl)

          # Bayesian fine-tuning:
          if ( DO_BAYES ) {
            f_tuning <- tune::tune_bayes(wf,
                                         resamples = f_rs,
                                         initial = f_tuning,
                                         iter = tuning_bayes_maxit,
                                         param_info = param_info,
                                         metrics = metric_set(brier_class),
                                         control = tune_ctrl_bayes)
          }
          # fix workflow:
          f_wf <- tune::finalize_workflow(
            wf,
            parameters = tune::select_best(f_tuning, metric = "brier_class")
          )

          # gather tuning results for analytics
          f_tuning_results <- tune::collect_metrics(f_tuning)

          # fit the final model:
          m <- fit(f_wf, data = df[fids, ])
          save(m, f_tuning_results, file = fname)
        }
      }
      # collate results:
      if ( exists("m") ) {
        preds <<- dplyr::bind_rows(
          preds,
          predict(m, new_data = df[-fids, ], type = "prob") |>
            dplyr::select(-1) |>
            setNames(".pred") |>
            bind_cols(y = df[-fids, "y"]) |>
            bind_cols(label = flab)
        )
      }
    })
  }
  # ~ metrics ---------------------------------------------------------------
  results <- preds |>
    dplyr::group_by(.data$label) |>
    metricset(truth = "y", ".pred", event_level = "second")

  # ~ end -------------------------------------------------------------------
  save(preds, results, file = results_loc)
  invisible(results)
}

#' @importFrom dplyr pull filter
#' @importFrom stats var
#' @keywords internal
futility_check_neuralnet <- function(
  x,
  y,
  tuning,
  type = c("classification", "regression")
) {
  # x is a apparent validity predictions data.frame from the "final model"
  #   tuned and fit to the full data.
  #
  # y is the observed values of the data (either a factor or numeric)
  #
  # tuning is a collated tuning dataset from final model (i.e. tuning_results)
  #
  # If futile this function returns: TRUE (otherwise FALSE)
  #
  # For elastic-net if the apparent validity model identified the best
  #   fitting hyperparameters as a 'completely regularised' solution
  #   i.e. all non-intercept coefficients are zero then it's futile
  #   to cross-validate.
  #
  # If the final tuned model is an intercept-only model then
  #   predictions will be constant, so one futility check is for zero
  #   variance.
  #
  # For neuralnet it's very unlikely to produce zero variance predictions.
  #   A better check for xgb is if the holdout error from the finalmodel
  #   tuning_grid is not better than the "null" error of an intercept-only
  #   model.

  type <- match.arg(type)

  # init. as non-futile:
  chk <- FALSE

  # check 1: zero variance
  v <- var(x[[".pred"]])
  if ( v <  .Machine$double.eps ) {
    cli::cli_alert_warning(
      "cross-validation not run: zero-variance final-model preds"
    )
    chk <- TRUE
  }

  # check 2: tuning hold-out error not better than null
  #   note: regression always uses "mse" for tuning, while
  #           classification uses "brier_class"
  #           both of these are minimised,
  #           for futility we test > null
  test_metric <- ifelse(type == "regression", "mse", "brier_class")

  null_perf <- nullmetrics(y) |>
    dplyr::filter(.data$.metric == test_metric) |>
    dplyr::pull(.data$.estimate)

  cv_perf <- tuning |>
    dplyr::filter(.data$.metric == test_metric) |>
    dplyr::pull(.data$mean) |>
    min(na.rm = TRUE)

  if ( cv_perf > null_perf ) {
    cli::cli_alert_warning(
      "cross-validation not run: tuning CV worse than null-model"
    )
    chk <- TRUE
  }
  chk
}
