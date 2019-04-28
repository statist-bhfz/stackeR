#' Models fitting and prediction.
#'
#' Fit list of models on train part and predict on validation part of one resample.
#'
#' @param data data.table with all input data.
#' @param target Target variable name (character).
#' @param models Named list of fit functions from \code{tuneR} package
#' (\code{xgb_fit}, \code{lgb_fit} etc.)
#' @param model_params List of data.table's with tunable model parameters.
#' @param model_args List of unchangeable model parameters.
#' @param preproc_funs List of preprocessing functions (one function per model)
#' which takes data.table \code{data}+\code{split} as input and returns
#' processed data.table with same \code{target} and \code{split} columns.
#'
#' @return data.table with ground truth for validation part of the resample and
#' predictions from all fitted models.
#'
#' @examples
#' # Input data
#' dt <- as.data.table(mtcars)
#'
#' # List of models
#' models <- list("xgboost" = xgb_fit, "catboost" = catboost_fit)
#'
#' # Model parameters (turn off early stopping)
#' xgb_params <- data.table(
#'     max_depth = 6,
#'     eta = 0.025,
#'     colsample_bytree = 0.9,
#'     subsample = 0.8,
#'     gamma = 0,
#'     min_child_weight = 5,
#'     alpha = 0,
#'     lambda = 1
#' )
#' xgb_args <- list(
#'     nrounds = 50,
#'     booster = "gbtree",
#'     eval_metric = "rmse",
#'     objective = "reg:linear",
#'     verbose = 0
#' )
#'
#' catboost_params <- data.table(
#'     iterations = 100,
#'     learning_rate = 0.05,
#'     depth = 8,
#'     loss_function = "RMSE",
#'     eval_metric = "RMSE",
#'     random_seed = 42,
#'     logging_level = "Silent"
#' )
#' catboost_args <- NULL
#'
#' model_params <- list(xgb_params, catboost_params)
#' model_args <- list(xgb_args, catboost_args)
#'
#' # Dumb preprocessing function
#' # Real function will contain imputation, feature engineering etc.
#' # with all statistics computed on train folds and applied to validation fold
#' preproc_fun_example <- function(data) return(data[])
#' # List of preprocessing fuctions for each model
#' preproc_funs <- list(preproc_fun_example, preproc_fun_example)
#'
#' all_models <- all_models_fit(data = dt,
#'                              target = "hp",
#'                              models = models,
#'                              model_params = model_params,
#'                              model_args = model_args,
#'                              preproc_funs = preproc_funs)
#'
#' @details
#'
#'
#' @import data.table
#' @import checkmate
#' @import resampleR
#' @import grideR
#' @export
all_models_fit <- function(data,
                           target,
                           models,
                           model_params,
                           model_args,
                           preproc_funs) {

    assert_data_table(data)
    assert_list(models, types = "function", names = "named")
    assert_list(model_params, types = "list")
    assert_list(model_args)
    assert_list(preproc_funs, types = "function")
    assert_true(
        length(
            unique(
                sapply(list(models, model_params, model_args, preproc_funs),
                       length))) == 1
    )

    res <- lapply(seq_along(models),
                  function(i) {
                      models[[i]](data = data,
                                  target = target,
                                  split = NULL,
                                  preproc_fun = preproc_funs[[i]],
                                  params = model_params[[i]],
                                  args = model_args[[i]],
                                  metrics = NULL,
                                  train_on_all_data = TRUE
                      )
                  })
    names(res) <- names(models)
    return(res)
}
