
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stackeR

<!-- badges: start -->

<!-- badges: end -->

The goal of stackeR is to …

## Installation

You can install the released version of stackeR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("stackeR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("statist-bhfz/stackeR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(stackeR)
#> Loading required package: data.table
#> Loading required package: grideR
#> Warning: replacing previous import 'lightgbm::getinfo' by
#> 'xgboost::getinfo' when loading 'grideR'
#> Warning: replacing previous import 'lightgbm::slice' by 'xgboost::slice'
#> when loading 'grideR'
#> Warning: replacing previous import 'lightgbm::setinfo' by
#> 'xgboost::setinfo' when loading 'grideR'
## basic example code
```

``` r
# Input data
dt <- as.data.table(mtcars)
# data.table with resamples
splits <- resampleR::cv_base(dt, "hp")
dt[splits[, split_1] == 1, hp := NA]
# data.table with tunable model hyperparameters
xgb_grid <- CJ(
    max_depth = c(6, 8),
    eta = 0.025,
    colsample_bytree = 0.9,
    subsample = 0.8,
    gamma = 0,
    min_child_weight = c(3, 5),
    alpha = 0,
    lambda = 1
)
# Non-tunable parameters for xgboost
xgb_args <- list(
    nrounds = 50,
    booster = "gbtree",
    eval_metric = "rmse",
    objective = "reg:linear",
    verbose = 0
)
# Dumb preprocessing function
# Real function will contain imputation, feature engineering etc.
# with all statistics computed on train folds and applied to validation fold
preproc_fun_example <- function(data) return(data[])
tmp <- xgb_fit(data = dt,
        target = "hp",
        split = splits[, split_1],
        preproc_fun = preproc_fun_example,
        params = xgb_grid[1, ],
        args = xgb_args,
        metrics = NULL,
        return_val_preds = TRUE)
```
