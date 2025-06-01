# Regression AirBnB Problem ----
# Variable selection

# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("data_split/reg_train.rda"))

# create resamples/folds ----
set.seed(9587)
bt_folds <- reg_train |> 
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
bt_spec <-
  boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(),
    learn_rate = tune(),
    tree_depth = tune()
  ) |> 
  set_mode("regression") |> 
  set_engine("lightgbm")

# define workflows ----
bt_wflow <-
  workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(recipe_select)

# hyperparameter tuning values ----
bt_params <- extract_parameter_set_dials(bt_spec) |> 
  update(
    mtry = mtry(range = c(2, 50)),
    min_n = min_n(range = c(1, 30)),
    trees = trees(range = c(200, 1500)),
    tree_depth = tree_depth(range = c(3, 15)),
    learn_rate = learn_rate(range = c(0.0001, 0.4))
  )

# build tuning grid
bt_grid <- grid_space_filling(bt_params, size = 15)

# fit workflow/model ----
bt_tuned <-
  bt_wflow |> 
  tune_grid(
    resamples = bt_folds,
    grid = bt_grid,
    metrics = metric_set(mae),
    control = control_grid(save_workflow = TRUE)
  )

# extract best model (optimal tuning parameters) ----
optimal_wflow <- extract_workflow(bt_tuned) |> 
  finalize_workflow(select_best(bt_tuned, metric = "mae"))

# fit best model/results ----
var_select_fit_bt <- fit(optimal_wflow, reg_train)

# write out variable selection results ----
save(var_select_fit_bt, file = here("07_attempt/results/7a_var_select_fit_bt.rda"))
