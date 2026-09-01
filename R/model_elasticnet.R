

#' A elastic-net regularised linear model with grid-tuned regularisation.
#'
#' Fits to data a model linear in the predictors but with regularisation to
#'  reduce overfitting and perform variable selection (L1 penalty-type).
#'  Tunes hyperparameters for:
#' 1) **penalty** level (i.e. [lambda][glmnet::glmnet]), and
#' 2) the **mixture** of L2-L1 penalty types (i.e. [alpha][glmnet::glmnet])
#'
#' Default tuning considers:
#' * 3 mixture values of alpha with log-spacing between 0.05 and 1.0, so:
#'    0.05, ~0.224, 1.0
#' * 1000 penalty values of lambda with log10-spacing between the (data-driven)
#'   maximum lambda (using glmnet to estimate this), and a minimum lambda derived
#'   from a ratio parameter (`penalty_minratio`) multiplied by the maximum lambda.
#'
#' @note Tuning uses a custom grid because fitting an elastic-net model produces
#'   all values of the penalty parameter with minimal overhead (due to warm starts).
#'   As a result, tuning over a large number of values of the regularisation penalty
#'   (lambda) is computationally cheap, while tuning lots of values of the mixture
#'   hyperparameter (alpha) is expensive (and often in practice less impactful).
#'   Furthermore, the maximum lambda depends on the value of alpha selected.
#'
#' @param x an analysis object to run elasticnet models for.
#' @param folder a folder path ending in a model-name unique label
#'   used to store results.
#' @param penalty_res the number of penalty values to tune over, values are
#'   determined from the data using the glmnet method.
#' @param mixture_res the number of mixtures to tune over - values will be
#'   log spaced between `mixture_min` and 1.0.
#' @param penalty_minratio see `lambda.min.ratio` argument in glmnet.
#'   Set this to auto to follow glmnet's default behaviour.
#' @param mixture_min the smallest mixture value to consider - this is how close
#'   the model should get to a pure L2 (Ridge) model. For p >> n the pure ridge
#'   is very time-consuming, so the default is 0.05.
#'   Set this to 0.0 to consider the time-consuming pure L2 Ridge when tuning.
#' @param metricset used internally to specify metrics
#' @param tuning_resample_fxn a string containing a function name from [rsample]
#'   which will be used to resample the data for tuning purposes
#'   (e.g. ["bootstraps"][rsample::bootstraps], ["vfoldcv"][rsample::vfold_cv])
#' @param tuning_resample_args list of arguments to pass to `tuning_resample_fxn`
#' @param check_futility logical. Futility checks on the finalmodel
#'     determine whether cross-validation should be conducted.
#'     If `check_futility = FALSE` then cross-validation is
#'     always conducted.
#' @param .RUN used internally to allow results collation without running
#'     missing models.
#' @param ... unused.
#'
#' @seealso [glmnet::glmnet] [parsnip::linear_reg] [parsnip::logistic_reg]
#' @export
elasticnet <- function(x,
                       folder,
                       penalty_res = 1000L,
                       mixture_res = 3L,
                       penalty_minratio = "auto",
                       mixture_min = log(0.05),
                       tuning_resample_fxn = "vfold_cv",
                       tuning_resample_args = list(v = 10,
                                                   repeats = 1,
                                                   strata = "y"),
                       check_futility = TRUE,
                       metricset = NULL,
                       .RUN = TRUE,
                       ...) {
  check_suggested("glmnet", "Elastic-net fitting")
  UseMethod("elasticnet")
}

#' @rdname elasticnet
#' @export
elasticnet.default <- function(x,
                               folder,
                               penalty_res = 1000L,
                               mixture_res = 3L,
                               penalty_minratio = "auto",
                               mixture_min = log(0.05),
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

#' @rdname elasticnet
#' @importFrom stats complete.cases predict
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom recipes recipe add_step all_predictors step_pca
#' @importFrom parsnip linear_reg fit
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang .data
#' @importFrom tune finalize_workflow select_best tune_grid
#' @importFrom tune collect_metrics control_grid tune
#' @export
elasticnet.regression_analysis <-  function(
  x,
  folder,
  penalty_res = 1000L,
  mixture_res = 3L,
  penalty_minratio = "auto",
  mixture_min = 0.05,
  tuning_resample_fxn = "vfold_cv",
  tuning_resample_args = list(v = 10, repeats = 1, strata = "y"),
  check_futility = TRUE,
  metricset = metricset_regression(),
  .RUN = TRUE,
  ...
) {
  checkmate::assert_flag(.RUN)
  checkmate::assert_int(mixture_res, lower = 2L)
  checkmate::assert_int(penalty_res, lower = 2L)
  assert_auto_or_ratio(penalty_minratio)
  checkmate::assert_number(mixture_min, lower = 0.0, upper = 1.0)

  # ~ extract from problem --------------------------------------------------
  df <- as.data.frame(problem(x))
  folds <- problem(x)[["folds"]]

  n <- sum(complete.cases(df))
  p <- ncol(df)

  # ~ options ---------------------------------------------------------------
  if ( penalty_minratio == "auto" ) {
    # from glmnet::glmnet lambda.min.ratio argument:
    penalty_minratio <- ifelse(n < p, 0.01, 1e-4)
  }

  # used in hyperparameter tuning:
  mixture_values <- exp(seq(from = log(mixture_min),
                            to = 0.0,
                            length.out = mixture_res))

  # ~ setup locations -------------------------------------------------------
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  wf_loc <- file.path(folder, "workflow.RData")
  fm_loc <- file.path(folder, "finalmodel.RData")
  fold_locs <- file.path(folder, paste0(names(folds), ".RData"))
  results_loc <- file.path(folder, "results.RData")

  # ~ workflow --------------------------------------------------------------
  if ( file.exists(wf_loc) ) {
    cli::cli_alert_info("elasticnet loading: workflow")
    load(wf_loc)
  } else {
    cli::cli_alert_info("elasticnet generating: workflow")

    wf <- workflows::workflow()

    rp <- recipes::recipe(y ~ ., data = df)

    model <- parsnip::linear_reg(mode = "regression",
                                 engine = "glmnet",
                                 penalty = tune(),
                                 mixture = tune())

    # no additional steps (currently)

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    tuning_grid <- elasticnet_make_grid(y = df$y,
                                        x = df[, colnames(df) != "y"],
                                        alpha_values = mixture_values,
                                        nlambda = penalty_res,
                                        minratio = penalty_minratio)

    tune_ctrl <- tune::control_grid(event_level = "second", verbose = TRUE)

    save(wf, tuning_grid, tune_ctrl, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( file.exists(fm_loc) ) {
    cli::cli_alert_info("elasticnet loading: final model")
    load(fm_loc)
  } else {
    if ( .RUN ) {
      cli::cli_alert_info("elasticnet tuning: final model")
      # make a resampling object:
      resample_args <- append(list(data = df), tuning_resample_args)
      rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                    resample_args)
      # run tuning:
      tuning <- tune::tune_grid(wf,
                                resamples = rs,
                                grid = tuning_grid,
                                metrics = metric_set(mse),
                                control = tune_ctrl)

      fwf <- tune::finalize_workflow(wf,
                                     parameters = tune::select_best(tuning,
                                                                    metric = "mse"))

      tuning_results <- tune::collect_metrics(tuning)

      cli::cli_alert_info("elasticnet running: final model")
      m <- fit(fwf, data = df)
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
    FUTILITY <- futility_check_elasticnet(preds)
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
        msg_cv_loading(model = "elasticnet",
                       id = i,
                       nid = length(folds))
        load(fname)
      } else {
        if ( .RUN ) {
          msg_cv_running(model = "elasticnet",
                         id = i,
                         nid = length(folds))

          # make a resampling object:
          f_resample_args <- append(list(data = df[fids, ]), tuning_resample_args)

          f_rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                          f_resample_args)
          # run tuning:
          f_tuning <- tune::tune_grid(wf,
                                      resamples = f_rs,
                                      grid = tuning_grid,
                                      metrics = metric_set(mse),
                                      control = tune_ctrl)


          f_wf <- tune::finalize_workflow(
            wf,
            parameters = tune::select_best(f_tuning, metric = "mse")
          )

          f_tuning_results <- tune::collect_metrics(f_tuning) #nolint

          m <- fit(f_wf, data = df[fids, ])
          save(m, file = fname)
        }
      }
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

#' @rdname elasticnet
#' @export
elasticnet.classification_analysis <-  function(
  x,
  folder,
  penalty_res = 1000L,
  mixture_res = 3L,
  penalty_minratio = "auto",
  mixture_min = 0.05,
  tuning_resample_fxn = "vfold_cv",
  tuning_resample_args = list(v = 10, repeats = 1, strata = "y"),
  check_futility = TRUE,
  metricset = metricset_classification(),
  .RUN = TRUE,
  ...
) {
  checkmate::assert_flag(.RUN)
  checkmate::assert_int(mixture_res, lower = 2L)
  checkmate::assert_int(penalty_res, lower = 2L)
  assert_auto_or_ratio(penalty_minratio)
  checkmate::assert_number(mixture_min, lower = 0.0, upper = 1.0)

  # ~ extract from problem --------------------------------------------------
  df <- as.data.frame(problem(x))
  folds <- problem(x)[["folds"]]

  n <- sum(complete.cases(df))
  p <- ncol(df)

  # ~ options ---------------------------------------------------------------
  if ( penalty_minratio == "auto" ) {
    # from glmnet::glmnet lambda.min.ratio argument:
    penalty_minratio <- ifelse(n < p, 0.01, 1e-4)
  }

  # used in hyperparameter tuning:
  mixture_values <- exp(seq(from = log(mixture_min),
                            to = 0.0,
                            length.out = mixture_res))

  # ~ setup locations -------------------------------------------------------
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  wf_loc <- file.path(folder, "workflow.RData")
  fm_loc <- file.path(folder, "finalmodel.RData")
  fold_locs <- file.path(folder, paste0(names(folds), ".RData"))
  results_loc <- file.path(folder, "results.RData")

  # ~ workflow --------------------------------------------------------------
  if ( file.exists(wf_loc) ) {
    cli::cli_alert_info("elasticnet loading: workflow")
    load(wf_loc)
  } else {
    cli::cli_alert_info("elasticnet generating: workflow")

    wf <- workflows::workflow()

    rp <- recipes::recipe(y ~ ., data = df)

    model <- parsnip::logistic_reg(mode = "classification",
                                   engine = "glmnet",
                                   penalty = tune(),
                                   mixture = tune())

    # no additional steps (currently)

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    tuning_grid <- elasticnet_make_grid(y = as.numeric(df$y) - 1,
                                        x = df[, colnames(df) != "y"],
                                        alpha_values = mixture_values,
                                        nlambda = penalty_res,
                                        minratio = penalty_minratio)

    tune_ctrl <- tune::control_grid(event_level = "second", verbose = TRUE)

    save(wf, tuning_grid, tune_ctrl, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( file.exists(fm_loc) ) {
    cli::cli_alert_info("elasticnet loading: final model")
    load(fm_loc)
  } else {
    if ( .RUN ) {
      cli::cli_alert_info("elasticnet tuning: final model")

      # make a resampling object:
      resample_args <- append(list(data = df), tuning_resample_args)
      rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                    resample_args)
      # run tuning:
      tuning <- tune::tune_grid(wf,
                                resamples = rs,
                                grid = tuning_grid,
                                metrics = metric_set(brier_class),
                                control = tune_ctrl)

      fwf <- tune::finalize_workflow(
        wf,
        parameters = tune::select_best(tuning,
                                       metric = "brier_class")
      )

      tuning_results <- tune::collect_metrics(tuning)

      cli::cli_alert_info("elasticnet running: final model")
      m <- fit(fwf, data = df)
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
    FUTILITY <- futility_check_elasticnet(preds)
  } else {
    FUTILITY <- FALSE
  }

  # ~ fold models -----------------------------------------------------------
  if ( !FUTILITY ) {
    lapply(seq.int(length(folds)), function(i) {
      fname <- fold_locs[[i]]
      fids <- folds[[i]]
      flab <- names(folds)[[i]]
      if ( !file.exists(fname) && .RUN ) {
        msg_cv_running(model = "elasticnet",
                       id = i,
                       nid = length(folds))
        # make a resampling object:
        f_resample_args <- append(list(data = df[fids, ]), tuning_resample_args)

        f_rs <- do.call(get(tuning_resample_fxn, envir = asNamespace("rsample")),
                        f_resample_args)
        # run tuning:
        f_tuning <- tune::tune_grid(wf,
                                    resamples = f_rs,
                                    grid = tuning_grid,
                                    metrics = metric_set(brier_class),
                                    control = tune_ctrl)

        f_wf <- tune::finalize_workflow(
          wf,
          parameters = tune::select_best(f_tuning,
                                         metric = "brier_class")
        )

        f_tuning_results <- tune::collect_metrics(f_tuning) #nolint

        m <- fit(f_wf, data = df[fids, ])
        save(m, file = fname)
      } else {
        msg_cv_loading(model = "elasticnet",
                       id = i,
                       nid = length(folds))
        load(fname)
      }
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


#' @keywords internal
assert_auto_or_ratio <- function(x) {
  checkmate::assert(
    checkmate::check_choice(x, "auto"),
    checkmate::check_number(x, lower = 1L, upper = 1L)
  )
}

#' @keywords internal
elasticnet_lambda_calculator <- function(y,
                                         x,
                                         alpha,
                                         nlambda,
                                         minratio) {
  check_suggested("glmnet", "Elastic-net fitting")
  glmnet::glmnet(y = as.vector(y),
                 x = as.matrix(x),
                 alpha = alpha,
                 nlambda = nlambda,
                 lambda.min.ratio = minratio)$lambda
}

#' @importFrom tibble tibble
#' @importFrom purrr map map2
#' @keywords internal
elasticnet_make_grid <- function(y, x, alpha_values, nlambda, minratio) {
  res <- purrr::map(
    alpha_values,
    \(.x) {
      elasticnet_lambda_calculator(
        y = y,
        x = x,
        alpha = .x,
        nlambda = nlambda,
        minratio = minratio
      )
    }
  )
  purrr::map2(res,
              alpha_values,
              \(.x, .y) tibble::tibble(mixture = .y, penalty = .x)) |>
    bind_rows()
}

#' @importFrom stats var
#' @keywords internal
futility_check_elasticnet <- function(x) {
  # x is a apparent validity predictions data.frame from the "final model"
  #   tuned and fit to the full data.
  #
  # If futile this function returns: TRUE (otherwise FALSE)
  #
  # For elastic-net if the apparent validity model identified the best
  #   fitting hyperparameters as a 'completely regularised' solution
  #   i.e. all non-intercept coefficients are zero then it's futile
  #   to cross-validate.
  #
  # If the final tuned model is an intercept-only model then
  #   predictions will be constant, so the futility check is for zero
  #   variance.

  v <- var(x[[".pred"]])
  chk <- v < .Machine$double.eps

  if ( chk ) {
    cli::cli_alert_warning(
      "cross-validation not run: zero-variance final-model preds"
    )
  }
  chk
}
