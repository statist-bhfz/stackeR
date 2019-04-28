#' Models stacking using SuperLearner algorithm.
#'
#' Fit arbitrary metamodel on first-level models predictions.
#'
#' @param data data.table with all input data.
#' @param target Target variable name (character).
#' @param splits data.table with train/validation splits.
#' Each column is an indicator variable with 1 corresponds to observations
#' in validation dataset.
#' @param models Named list of fit functions from \code{tuneR} package
#' (\code{xgb_fit}, \code{lgb_fit} etc.)
#' @param model_params List of data.table's with tunable model parameters.
#' @param model_args List of unchangeable model parameters.
#' @param preproc_funs List of preprocessing functions (one function per model)
#' which takes data.table \code{data}+\code{split} as input and returns
#' processed data.table with same \code{target} and \code{split} columns.
#' @param metamodel Function for fitting metamodel.
#' @param metamodel_params List with metamodel parameters.
#' @param metamodel_interface "formula" or "matrix" depending on the metamodel type.
#'
#' @return Object with fitted metamodel.
#'
#' @examples
#' # Input data
#' dt <- as.data.table(mtcars)
#'
#' # data.table with resamples
#' splits <- resampleR::cv_base(dt, "hp")
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
#' metamodel_obj <- metamodel_fit(data = dt,
#'                                target = "hp",
#'                                split = splits,
#'                                models = models,
#'                                model_params = model_params,
#'                                model_args = model_args,
#'                                preproc_funs = preproc_funs,
#'                                metamodel = ranger::ranger,
#'                                metamodel_params = list(num.trees = 3),
#'                                metamodel_interface = "formula"
#'                                )
#' first_level_preds <- across_models(data = dt,
#'                                    target = "hp",
#'                                    split = splits[, split_1],
#'                                    models = models,
#'                                    model_params = model_params,
#'                                    model_args = model_args,
#'                                    preproc_funs = preproc_funs)
#' predict(metamodel_obj, first_level_preds)$predictions
#'
#' @details
#'
#'
#' @import data.table
#' @import checkmate
#' @import resampleR
#' @import grideR
#' @export
metamodel_fit <- function(data,
                          target,
                          splits,
                          models,
                          model_params,
                          model_args,
                          preproc_funs,
                          metamodel = lm,
                          metamodel_params = list(NULL),
                          metamodel_interface = "formula") {

    assert_data_table(data)
    assert_true(splits[, .N] == data[, .N])
    assert_list(models, types = "function", names = "named")
    assert_list(model_params, types = "list")
    assert_list(model_args)
    assert_list(preproc_funs, types = "function")
    assert_function(metamodel)
    assert_list(metamodel_params)
    assert_subset(metamodel_interface, c("formula", "matrix"))
    assert_true(
        length(
            unique(
                sapply(list(models, model_params, model_args, preproc_funs),
                       length))) == 1
    )

    base_models_preds <- lapply(
        splits,
        function(split) across_models(data = data,
                                      target = target,
                                      split = split,
                                      models = models,
                                      model_params = model_params,
                                      model_args = model_args,
                                      preproc_funs = preproc_funs))
    base_models_preds <- rbindlist(base_models_preds, idcol = "split")

    if (metamodel_interface == "formula") {
        metamodel_fit <- do.call(metamodel,
                                 c(list(formula = ground_truth ~ . - split,
                                        data = base_models_preds),
                                   metamodel_params))
    } else if (metamodel_interface == "matrix") {
        x <- as.matrix(base_models_preds[, -c("split", "ground_truth")])
        y <- base_models_preds[, ground_truth]
        metamodel_fit <- do.call(metamodel,
                                 c(list(x = x, y = y),
                                   metamodel_params))
    } else {
        print("Unknown metamodel interface")
        return(NULL)
    }

    return(metamodel_fit)
}
