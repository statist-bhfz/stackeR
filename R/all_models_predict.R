
# используется на тестовых данных для создания фич для метамодели
predict_all_models <- function(data, models_list) {
    assert_data_table(data)
    assert_list(models_list)

    cols_to_drop <- c(
        "totals.transactionRevenue",
        "fullVisitorId",
        "visitStartTime"
    )

    dtest <- xgb.DMatrix(
        data = as.matrix(data[, .SD, .SDcols = -cols_to_drop]),
        label = as.matrix(data[, totals.transactionRevenue])
    )
    pool_test <- catboost.load_pool(data[, .SD, .SDcols = -cols_to_drop],
                                    label = NULL)
    # можно переписать с lapply
    preds <- data.table(
        fullVisitorId = data$fullVisitorId,
        preds_raw_xgb = predict(models_list$xgb, dtest),
        preds_raw_catboost = catboost.predict(models_list$catboost, pool_test)
    )

    # Заполняем отрицательные значения 0
    preds[,
          c("preds_raw_xgb", "preds_raw_catboost") :=
              lapply(.SD, function(x) ifelse(x < 0, 0, x)),
          .SDcols = c("preds_raw_xgb", "preds_raw_catboost")]

    # Трансформация предиктов
    preds <- preds[,
                   lapply(.SD, trans),
                   by = fullVisitorId,
                   .SDcols = c("preds_raw_xgb",
                               "preds_raw_catboost")]
    setnames(preds,
             c("preds_raw_xgb", "preds_raw_catboost"),
             c("sum_logs_xgb", "sum_logs_catboost"))
    return(preds[])
}
