

#' A (general) linear model with minimal (untuned) regularisation.
#'
#' Either no regularisation is applied, or an untuned PCA reduction of the
#'    predictors is used depending on settings for `pca` and `pca_ncomp`
#'
#' @note For pca = "auto" calculations,
#'     n is number of samples, p is number of predictors.
#'
#' @param x an analysis object to run linear models for.
#' @param folder a folder path ending in a model-name unique label
#'   used to store results.
#' @param pca either TRUE (uses PCA to reduce input dimensionality),
#'   FALSE (does not reduce dimensionality) or default: `"auto"`
#'   such that auto applies a PCA step if n < (50 + 10p)
#' @param pca_ncomp number of PCA components, either positive integer <= p
#'   or "auto" to use `floor((n - 50)/10)`.
#' @param model used internally to specify a parsnip model
#' @param metricset used internally to specify metrics
#' @param .RUN used internally to allow results collation without running
#'   missing models.
#' @param ... unused.
#'
#' @seealso  [parsnip::linear_reg] [parsnip::logistic_reg]
#' @export
linear <- function(x,
                   folder,
                   pca = "auto",
                   pca_ncomp = "auto",
                   model = NULL,
                   metricset = NULL,
                   .RUN = TRUE,
                   ...) {
  UseMethod("linear")
}

#' @rdname linear
#' @export
linear.default <- function(x,
                           folder,
                           pca = "auto",
                           pca_ncomp = "auto",
                           model = NULL,
                           metricset = NULL,
                           .RUN = TRUE,
                           ...) {
  stop("Not implemented for classes apart from analysis")
}

#' @rdname linear
#' @importFrom stats complete.cases predict
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom recipes recipe add_step all_predictors step_pca
#' @importFrom parsnip linear_reg fit
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang .data
#' @export
linear.regression_analysis <-  function(x,
                                        folder,
                                        pca = "auto",
                                        pca_ncomp = "auto",
                                        model = parsnip::linear_reg(),
                                        metricset = metricset_regression(),
                                        .RUN = TRUE,
                                        ...) {
  checkmate::assert_flag(.RUN)
  assert_auto_or_flag(pca)
  assert_auto_or_posint(pca_ncomp)

  # ~ extract from problem --------------------------------------------------
  df <- as.data.frame(problem(x))
  folds <- problem(x)[["folds"]]

  n <- sum(complete.cases(df))
  p <- ncol(df)
  pca_auto_check <- p > ((n - 50) / 10)

  # ~ options ---------------------------------------------------------------
  if ( pca == "auto" ) {
    pca <- pca_auto_check
  }
  if ( pca_ncomp == "auto" ) {
    pca_ncomp <- pmax(1L, floor((n - 50) / 10))
  }

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
    if ( pca ) {
      rp <- rp |>
        step_pca(all_predictors(),
                 num_comp = pca_ncomp)
    }

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    save(wf, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( !file.exists(fm_loc) && .RUN ) {
    cli::cli_alert_info("linear running: final model")
    m <- fit(wf, data = df)
    save(m, file = fm_loc)
  } else {
    if ( file.exists(fm_loc) ) {
      load(fm_loc)
    } else {
      # error:
      msg_norun_mainmissing()
    }
  }
  # extract predictions:
  preds <- predict(m, new_data = df, type = "numeric") |>
    bind_cols(y = df$y) |>
    bind_cols(label = "apparent")

  # ~ fold models -----------------------------------------------------------
  lapply(seq.int(length(folds)), function(i) {
    fname <- fold_locs[[i]]
    fids <- folds[[i]]
    flab <- names(folds)[[i]]
    if ( !file.exists(fname) && .RUN ) {
      msg_cv_running(model = "linear",
                     id = i,
                     nid = length(folds))
      m <- fit(wf, data = df[fids, ])
      save(m, file = fname)
    } else {
      msg_cv_loading(model = "linear",
                     id = i,
                     nid = length(folds))
      load(fname)
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

  # ~ metrics ---------------------------------------------------------------
  results <- preds |>
    dplyr::group_by(.data$label) |>
    metricset(truth = "y", estimate = ".pred")

  # ~ end -------------------------------------------------------------------
  save(preds, results, file = results_loc)
  invisible(results)
}

#' @rdname linear
#' @export
linear.classification_analysis <-  function(x,
                                            folder,
                                            pca = "auto",
                                            pca_ncomp = "auto",
                                            model = parsnip::logistic_reg(),
                                            metricset = metricset_classification(),
                                            .RUN = TRUE,
                                            ...) {
  checkmate::assert_flag(.RUN)
  assert_auto_or_flag(pca)
  assert_auto_or_posint(pca_ncomp)

  # ~ extract from problem --------------------------------------------------
  df <- as.data.frame(problem(x))
  folds <- problem(x)[["folds"]]

  n <- sum(complete.cases(df))
  p <- ncol(df)
  pca_auto_check <- p > ((n - 50) / 10)

  # ~ options ---------------------------------------------------------------
  if ( pca == "auto" ) {
    pca <- pca_auto_check
  }
  if ( pca_ncomp == "auto" ) {
    pca_ncomp <- pmax(1L, floor((n - 50) / 10))
  }

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
    if ( pca ) {
      rp <- rp |>
        step_pca(all_predictors(),
                 num_comp = pca_ncomp)
    }

    wf <- wf |>
      workflows::add_recipe(rp) |>
      workflows::add_model(model)

    save(wf, file = wf_loc)
  }

  # ~ final model -----------------------------------------------------------
  if ( !file.exists(fm_loc) && .RUN ) {
    cli::cli_alert_info("linear running: final model")
    m <- fit(wf, data = df)
    save(m, file = fm_loc)
  } else {
    if ( file.exists(fm_loc) ) {
      load(fm_loc)
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


  # ~ fold models -----------------------------------------------------------
  lapply(seq.int(length(folds)), function(i) {
    fname <- fold_locs[[i]]
    fids <- folds[[i]]
    flab <- names(folds)[[i]]
    if ( !file.exists(fname) && .RUN ) {
      msg_cv_running(model = "linear",
                     id = i,
                     nid = length(folds))
      m <- fit(wf, data = df[fids, ])
      save(m, file = fname)
    } else {
      msg_cv_loading(model = "linear",
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


  # ~ metrics ---------------------------------------------------------------
  results <- preds |>
    dplyr::group_by(.data$label) |>
    metricset(truth = "y", ".pred", event_level = "second")

  # ~ end -------------------------------------------------------------------
  save(preds, results, file = results_loc)
  invisible(results)
}

#' @keywords internal
assert_auto_or_flag <- function(x) {
  checkmate::assert(
    checkmate::check_choice(x, "auto"),
    checkmate::check_flag(x)
  )
}

#' @keywords internal
assert_auto_or_posint <- function(x) {
  checkmate::assert(
    checkmate::check_choice(x, "auto"),
    checkmate::check_integerish(x, lower = 1L, len = 1L)
  )
}
