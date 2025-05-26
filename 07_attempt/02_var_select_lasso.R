# Regression AirBnB Problem ----
# Variable selection

# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(broom)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("data_split/reg_train.rda"))

# create resamples/folds ----
set.seed(9587)
lasso_folds <- reg_train |> 
  vfold_cv(v = 5, repeats = 3, strata = price)

# selection recipe ----
recipe_select <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean) |> 
  step_nzv(all_predictors()) |> 
  step_impute_knn(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_string2factor(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.01) |>  
  step_unknown(all_nominal_predictors()) |>             
  step_novel(all_nominal_predictors()) |> 
  step_YeoJohnson(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |>
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_numeric_predictors())

# check recipe
recipe_select |> 
  prep() |> 
  bake(new_data = NULL)

# model specifications ----
lasso_spec <- 
  linear_reg(
    penalty = tune(), 
    mixture = 1
    ) |> 
  set_mode("regression") |> 
  set_engine("glmnet")

# define workflows ----
lasso_wflow <- 
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(recipe_select)

# hyperparameter tuning values ----
lasso_tuned <- tune_grid(
  lasso_wflow,
  resamples = lasso_folds,
  grid = 15,
  metrics = metric_set(mae)
)

# pull out non-zero coefficients ----
best_lasso <- finalize_workflow(select_best(lasso_tuned, metric = "mae"))

# fit best model/results
var_select_lasso_fit <- fit(best_lasso, data = reg_train)

# tidy up
nonzero_vars <- tidy(lasso_fit) |> filter(estimate != 0) |> pull(term)

# write out variable selection results ----
save(var_select_lasso_fit, file = here("06_attempt/results/var_select_lasso_fit.rda"))
