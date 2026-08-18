#' cvmultipredict: Cross-validated prediction utilities
#'
#' Tools for fitting predictive models under cross-validation
#'     and generating out-of-sample predictions.
#'
#' @section Main functions:
#'
#'  * [problem] - define a `problem` from an outcome vector,
#'      a predictor matrix, and a cross-validation resampling scheme
#'  * [manifest] - JSON specification of prediction models and their settings
#'  * [analysis] - link a `problem` to a folder, and `manifest` of
#'      prediction models to fit
#'  * [cvmultipredict] - run prediction models associated with an `analysis`
#'  * [pool_results] - collate cross-validated performance metrics from
#'      `cvmultipredict`
#'  * [plot_pooled_results] - plot performance metrics
#'  * [plot_tuning_results] - plot hyperparameter tuning (for a given model)
#'
#' @docType package
#' @name cvmultipredict-package
"_PACKAGE"
