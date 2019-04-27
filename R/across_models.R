#' Models fitting and prediction.
#'
#' Fit list of models on train part and predict on validation part of one resample.
#'
#' @param data data.table with all input data.
#' @param target Target variable name (character).
#' @param split Indicator variable with 1 corresponds to observations in validation dataset.
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
#' target <- "hp"
#'
#' # data.table with resamples
#' splits <- resampleR::cv_base(dt, "hp")
#' # data.table with tunable model hyperparameters
#'
#' # List of models
#' models <- list("xgboost" = xgb_fit, "catboost" = catboost_fit)
#'
#' # Model parameters
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
#'     nrounds = 500,
#'     early_stopping_rounds = 10,
#'     booster = "gbtree",
#'     eval_metric = "rmse",
#'     objective = "reg:linear",
#'     verbose = 0
#' )
#'
#' catboost_params <- data.table(
#'     iterations = 1000,
#'     learning_rate = 0.05,
#'     depth = 8,
#'     loss_function = "RMSE",
#'     eval_metric = "RMSE",
#'     random_seed = 42,
#'     od_type = 'Iter',
#'     od_wait = 10,
#'     use_best_model = TRUE,
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
#' across_models(data = dt,
#'               target = "hp",
#'               split = splits[, split_1],
#'               models = models,
#'               model_params = model_params,
#'               model_args = model_args,
#'               preproc_funs = preproc_funs)
#'
#' @details
#'
#'
#' @import data.table
#' @import checkmate
#' @import resampleR
#' @import grideR
#' @export
across_models <- function(data,
                          target,
                          split,
                          models,
                          model_params,
                          model_args,
                          preproc_funs) {

    assert_data_table(data)
    assert_integerish(split, len = data[, .N])
    assert_list(models)
    assert_list(model_params)
    assert_list(model_args)
    assert_list(preproc_funs)
    assert_true(
        all(
            c(length(models), length(model_params),
              length(model_args), length(preproc_funs)) ==
            c(length(models), length(model_params),
              length(model_args), length(preproc_funs))
        )
    )

    res <- data[split == 1, .(ground_truth = get(target))] # validation data

    for (i in seq_along(models)) {
        preds <- models[[i]](data = data,
                             target = target,
                             split = split,
                             preproc_fun = preproc_funs[[i]],
                             params = model_params[[i]],
                             args = model_args[[i]],
                             metrics = NULL,
                             return_val_preds = TRUE
                             )
        res[, names(models)[i] := preds[, val_preds]]
    }

    return(res[])
}
