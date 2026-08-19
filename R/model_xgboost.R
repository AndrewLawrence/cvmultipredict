

#' XGBoost model with grid-tuned hyperparameters followed by Bayesian optimisation
#'
#' Fits Extreme Gradient Boosting (XGBoost) models to the problem.
#'  An XGBoost model is a non-linear ensemble of decision trees which are
#'  built sequentially to improve upon the errors made in the previous trees.
#'  Decision trees can automatically capture non-linear effects and higher order
#'  interactions between features without requiring these be specified in
#'  advance. Uses the [`xgboost`][xgboost::xgboost] engine with tuning wrapped by
#'  [parsnip][parsnip::boost_tree].
#'
#' @section Default Behaviour:
#' Tunes the following over a [space filling grid][dials::grid_space_filling]
#'     with resolution^dimensionality = 3^5 = 243 entries:
#' * ntrees - number of trees in the ensemble
#' * tree_depth - number of splits
#' * learn_rate - (i.e. eta) step-size shrinkage used to prevent overfitting
#' * min_n - threshold number of observations in a potential leaf node for
#'    further partitioning to be considered. Larger == more conservative.
#' * loss_reduction - threshold minimum reduction in the loss function required
#'     to make a further partition on a leaf node. Larger == more conservative.
#'
#' After initial grid tuning, [Bayesian optimisation][tune::tune_bayes]
#'     (Gaussian Process model) is applied to fine tune for 10-50 iterations.
#'
#' The following hyperparameters are fixed / untuned:
#' * `mtry = 1.0` - do not randomly subsample features.
#' * `sample_size = 1.0` - do not randomly subsample observations.
#'
#' All other values remain at the defaults of [xgboost::xgboost]
#'     via [parsnip::xgb_train]
#'
#' @note Hyperparameter ranges are length 2 or length 1 vectors. Use length 1
#'     vectors to suppress tuning for that hyperparameter.
#'     If a length 1 vector with value NA is provided then the
#'     underlying default will be used (passing null)
#'     Length 2 vectors are assumed to specify a range of hyperparameter
#'     values to explore.
#'
#' @param x an analysis object to run xgboost models for.
#' @param folder a folder path ending in a model-name unique label
#'   used to store results.
#' @param tuning_grid_n size of initial [space-filling][dials::grid_space_filling]
#'    tuning grid. Default: 3^5 = 243.
#'    Recommendation: base_spacing ^ dimensionality -
#'    i.e. same number of elements as a hypercube with a (relatively)
#'    low spacing resolution in each dimension. Due to the space_filling grid the
#'    effective spacing within dimension is much greater than the base spacing.
#' @param tunerange_min_n tuning range for min_n. default = c(2L, 10L).
#' @param tunerange_trees tuning range for trees. default = c(1L, 2000L).
#' @param tunerange_loss_reduction tuning range for loss_reduction.
#'     default = c(-3L, 2L). log10 scale.
#' @param tunerange_tree_depth tuning range for tree_depth. default = c(2L, 10L)
#' @param tunerange_learn_rate tuning range for learn_rate. default = c(-5, 1.5).
#'     log10 scale.
#' @param tuning_bayes_maxit maximum number of Bayesian optimisation iterations.
#'     Default: 50L. Set to 0L to suppress bayesian optimisation.
#' @param tuning_bayes_minit number of iterations without improvement before
#'     Bayesian optimisation is terminated.
#'     Default: 10
#' @param metricset used internally to specify the outer-loop evaluation metrics
#'     (not tuning)
#' @param tuning_resample_fxn a string containing a function name from [rsample]
#'   which will be used to resample the data for tuning purposes
#'   (e.g. ["bootstraps"][rsample::bootstraps], ["vfoldcv"][rsample::vfold_cv])
#' @param tuning_resample_args list of arguments to pass to `tuning_resample_fxn`
#' @param check_futility logical. Futility checks on the finalmodel
#'     determine whether cross-validation should be conducted.
#'     If `check_futility = FALSE` then cross-validation is
#'     always conducted.
#' @param ... unused.
#'
#' @seealso [xgboost::xgboost] [parsnip::boost_tree]
#' @export
xgboost <- function(x,
                    folder,
                    tuning_grid_n = 243,
                    tunerange_min_n = c(2L, 10L),
                    tunerange_trees = c(1L, 2000L),
                    tunerange_loss_reduction = c(-3L, 2L),
                    tunerange_tree_depth = c(2L, 10L),
                    tunerange_learn_rate = c(-5, 1.5),
                    tuning_bayes_maxit = 50L,
                    tuning_bayes_minit = 10L,
                    ...) {
  UseMethod("xgboost")
}

#' @rdname xgboost
#' @export
xgboost.default <- function(x,
                            folder,
                            tuning_grid_n = 243,
                            tunerange_min_n = c(2L, 10L),
                            tunerange_trees = c(1L, 2000L),
                            tunerange_loss_reduction = c(-3L, 2L),
                            tunerange_tree_depth = c(2L, 10L),
                            tunerange_learn_rate = c(-5, 1.5),
                            tuning_bayes_maxit = 50L,
                            tuning_bayes_minit = 10L,
                            ...) {
  stop("Not implemented for classes apart from analysis")
}

#' @rdname xgboost
#' @importFrom stats complete.cases predict update
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom recipes recipe add_step all_predictors step_pca
#' @importFrom parsnip boost_tree fit
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang .data
#' @importFrom tune finalize_workflow select_best tune_grid
#' @importFrom tune collect_metrics control_grid tune
#' @importFrom dials min_n trees loss_reduction tree_depth
#' @importFrom dials learn_rate grid_space_filling
#' @importFrom rsample bootstraps
#' @export
xgboost.regression_analysis <-  function(
  x,
  folder,
  tuning_grid_n = 243,
  tunerange_min_n = c(2L, 10L),
  tunerange_trees = c(1L, 2000L),
  tunerange_loss_reduction = c(-3L, 2L),
  tunerange_tree_depth = c(2L, 10L),
  tunerange_learn_rate = c(-5, 1.5),
  tuning_bayes_maxit = 50L,
  tuning_bayes_minit = 10L,
  metricset = metricset_regression(),
  tuning_resample_fxn = "vfold_cv",
  tuning_resample_args = list(v = 10, repeats = 1, strata = "y"),
  check_futility = TRUE,
  ...
) {
  checkmate::assert_int(tuning_grid_n, lower = 0L)
  checkmate::assert_int(tuning_bayes_maxit, lower = 0L)
  checkmate::assert_int(tuning_bayes_minit, lower = 0L)
  tunerange_trees <- unlist(tunerange_trees)
  tunerange_tree_depth <- unlist(tunerange_tree_depth)
  tunerange_min_n <- unlist(tunerange_min_n)
  tunerange_loss_reduction <- unlist(tunerange_loss_reduction)
  tunerange_learn_rate <- unlist(tunerange_learn_rate)
  checkmate::assert_integer(tunerange_min_n,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_integer(tunerange_trees,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_integer(tunerange_tree_depth,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_numeric(tunerange_loss_reduction,
                            min.len = 1L,
                            max.len = 2L)
  checkmate::assert_numeric(tunerange_learn_rate,
                            min.len = 1L,
                            max.len = 2L)

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

    rp <- recipes::recipe(y ~ ., data = df)

    model <- do.call(
      parsnip::boost_tree,
      list(
        engine = "xgboost",
        mode = "regression",
        mtry = 1.0, # fix mtry to be all cols
        trees = tune_or_fix(tunerange_trees),
        tree_depth = tune_or_fix(tunerange_tree_depth),
        learn_rate = tune_or_fix(tunerange_learn_rate),
        min_n = tune_or_fix(tunerange_min_n),
        loss_reduction = tune_or_fix(tunerange_loss_reduction)
      )
    )

    param_info <- parsnip::extract_parameter_set_dials(model)
    if ( length(tunerange_trees) > 1L ) {
      param_info <- update(param_info,
                           trees = trees(range = tunerange_trees))
    }
    if ( length(tunerange_tree_depth) > 1L ) {
      param_info <- update(param_info,
                           tree_depth = tree_depth(range = tunerange_tree_depth))
    }
    if ( length(tunerange_learn_rate) > 1L ) {
      param_info <- update(param_info,
                           learn_rate = learn_rate(range = tunerange_learn_rate))
    }
    if ( length(tunerange_min_n) > 1L ) {
      param_info <- update(param_info, min_n = min_n(range = tunerange_min_n))
    }
    if ( length(tunerange_loss_reduction) > 1L ) {
      param_info <- update(
        param_info,
        loss_reduction = loss_reduction(range = tunerange_loss_reduction)
      )
    }

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    # Initial tuning grid:
    tuning_grid <- grid_space_filling(param_info, size = tuning_grid_n)

    # separate control objects for grid and bayes:
    tune_ctrl <- tune::control_grid(event_level = "second", verbose = TRUE)
    tune_ctrl_bayes <- tune::control_bayes(event_level = "second",
                                           verbose = TRUE,
                                           no_improve = tuning_bayes_minit)

    save(wf, tuning_grid, tune_ctrl, tune_ctrl_bayes, param_info, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( file.exists(fm_loc) ) {
    cli::cli_alert_info("xgboost loading: final model")
    load(fm_loc)
  } else {
    cli::cli_alert_info("xgboost tuning: final model")

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
    cli::cli_alert_info("xgboost running: final model")
    m <- fit(fwf, data = df)

    # log to disk:
    save(m, tuning_results, file = fm_loc)
  }
  # extract predictions:
  preds <- predict(m, new_data = df, type = "numeric") |>
    bind_cols(y = df$y) |>
    bind_cols(label = "apparent")

  # futility check - i.e. if the tuned model is
  #               intercept only then don't cross-validate.
  if ( check_futility ) {
    FUTILITY <- futility_check_xgboost(x = preds,
                                       y = df$y,
                                       tuning = tuning_results,
                                       type = "regression")
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

        msg_cv_loading(model = "xgboost",
                       id = i,
                       nid = length(folds))
        load(fname)
      } else {
        msg_cv_running(model = "xgboost",
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
      # collate results:
      preds <<- dplyr::bind_rows(
        preds,
        predict(m, new_data = df[-fids, ], type = "numeric") |>
          bind_cols(y = df[-fids, "y"]) |>
          bind_cols(label = flab)
      )
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

#' @rdname xgboost
#' @export
xgboost.classification_analysis <-  function(
  x,
  folder,
  tuning_grid_n = 243,
  tunerange_min_n = c(2L, 10L),
  tunerange_trees = c(1L, 2000L),
  tunerange_loss_reduction = c(-3L, 2L),
  tunerange_tree_depth = c(2L, 10L),
  tunerange_learn_rate = c(-5, 1.5),
  tuning_bayes_maxit = 50L,
  tuning_bayes_minit = 10L,
  metricset = metricset_classification(),
  tuning_resample_fxn = "vfold_cv",
  tuning_resample_args = list(v = 10, repeats = 1, strata = "y"),
  check_futility = TRUE,
  ...
) {
  checkmate::assert_int(tuning_grid_n, lower = 0L)
  checkmate::assert_int(tuning_bayes_maxit, lower = 0L)
  checkmate::assert_int(tuning_bayes_minit, lower = 0L)
  tunerange_trees <- unlist(tunerange_trees)
  tunerange_tree_depth <- unlist(tunerange_tree_depth)
  tunerange_min_n <- unlist(tunerange_min_n)
  tunerange_loss_reduction <- unlist(tunerange_loss_reduction)
  tunerange_learn_rate <- unlist(tunerange_learn_rate)
  checkmate::assert_integer(tunerange_min_n,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_integer(tunerange_trees,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_integer(tunerange_tree_depth,
                            min.len = 1L,
                            max.len = 2L,
                            lower = 1L)
  checkmate::assert_numeric(tunerange_loss_reduction,
                            min.len = 1L,
                            max.len = 2L)
  checkmate::assert_numeric(tunerange_learn_rate,
                            min.len = 1L,
                            max.len = 2L)

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

    rp <- recipes::recipe(y ~ ., data = df)

    model <- do.call(
      parsnip::boost_tree,
      list(
        engine = "xgboost",
        mode = "classification",
        mtry = 1.0, # fix mtry to be all cols
        trees = tune_or_fix(tunerange_trees),
        tree_depth = tune_or_fix(tunerange_tree_depth),
        learn_rate = tune_or_fix(tunerange_learn_rate),
        min_n = tune_or_fix(tunerange_min_n),
        loss_reduction = tune_or_fix(tunerange_loss_reduction)
      )
    )

    param_info <- parsnip::extract_parameter_set_dials(model)
    if ( length(tunerange_trees) > 1L ) {
      param_info <- update(param_info,
                           trees = trees(range = tunerange_trees))
    }
    if ( length(tunerange_tree_depth) > 1L ) {
      param_info <- update(param_info,
                           tree_depth = tree_depth(range = tunerange_tree_depth))
    }
    if ( length(tunerange_learn_rate) > 1L ) {
      param_info <- update(param_info,
                           learn_rate = learn_rate(range = tunerange_learn_rate))
    }
    if ( length(tunerange_min_n) > 1L ) {
      param_info <- update(param_info,
                           min_n = min_n(range = tunerange_min_n))
    }
    if ( length(tunerange_loss_reduction) > 1L ) {
      param_info <- update(
        param_info,
        loss_reduction = loss_reduction(range = tunerange_loss_reduction)
      )
    }

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    # Initial tuning grid:
    tuning_grid <- grid_space_filling(param_info, size = tuning_grid_n)

    # separate control objects for grid and bayes:
    tune_ctrl <- tune::control_grid(event_level = "second", verbose = TRUE)
    tune_ctrl_bayes <- tune::control_bayes(event_level = "second",
                                           verbose = TRUE,
                                           no_improve = tuning_bayes_minit)

    save(wf, tuning_grid, tune_ctrl, tune_ctrl_bayes, param_info, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( file.exists(fm_loc) ) {
    cli::cli_alert_info("xgboost loading: final model")
    load(fm_loc)
  } else {
    cli::cli_alert_info("xgboost tuning: final model")

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
      parameters = tune::select_best(tuning, metric = "brier_class")
    )
    # gather tuning results for analytics
    tuning_results <- tune::collect_metrics(tuning)

    # fit the final model:
    cli::cli_alert_info("xgboost running: final model")
    m <- fit(fwf, data = df)

    # log to disk:
    save(m, tuning_results, file = fm_loc)
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
    FUTILITY <- futility_check_xgboost(x = preds,
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

        msg_cv_loading(model = "xgboost",
                       id = i,
                       nid = length(folds))
        load(fname)
      } else {
        msg_cv_running(model = "xgboost",
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
      # collate results:
      preds <<- dplyr::bind_rows(
        preds,
        predict(m, new_data = df[-fids, ], type = "prob") |>
          dplyr::select(-1) |>
          setNames(".pred") |>
          bind_cols(y = df[-fids, "y"]) |>
          bind_cols(label = flab)
      )
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

tune_or_fix <- function(x) {
  if (length(x) == 2) {
    tune()
  } else if (is.na(x)) {
    NULL
  } else {
    x
  }
}

#' @importFrom dplyr pull filter
#' @importFrom stats var
#' @keywords internal
futility_check_xgboost <- function(
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
  # For xgboost it's very unlikely to produce zero variance predictions.
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
