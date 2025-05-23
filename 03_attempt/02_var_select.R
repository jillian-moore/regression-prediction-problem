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
set.seed(987)
bt_folds <- reg_train |> 
  vfold_cv(v = 3, repeats = 1, strata = price)

# basic recipe ----
selection_recipe <- recipe(price ~ ., data = reg_train) |> 
  step_rm(description, amenities, amenities_clean, id) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_nzv(all_predictors()) |> 
  step_normalize(all_predictors())

# check recipe
selection_recipe |> 
  prep() |> 
  bake(new_data = NULL)

# model specifications ----
bt_spec <-
  boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
  ) |> 
  set_mode("regression") |> 
  set_engine("lightgbm")

# define workflows ----
bt_wflow <-
  workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(selection_recipe)

# hyperparameter tuning values ----
bt_params <- extract_parameter_set_dials(bt_spec) |> 
  update(
    mtry = mtry(range = c(2, 15)),
    min_n = min_n(range = c(2, 10)),
    trees = trees(range = c(200, 1000))
  )

# build tuning grid
bt_grid <- grid_random(bt_params, size = 3)

# fit workflow/model ----
bt_tuned <-
  bt_wflow |> 
  tune_grid(
    resamples = bt_folds,
    grid = bt_grid,
    metrics = metric_set(mae),
    control = control_grid(save_workflow = TRUE)
  )

# extract best model (optimal tuning parameters)
optimal_wflow <- extract_workflow(bt_tuned) |> 
  finalize_workflow(select_best(bt_tuned, metric = "mae"))

# fit best model/results
var_select_fit_bt <- fit(optimal_wflow, reg_train)

# write out variable selection results ----
save(var_select_fit_bt, file = here("03_attempt/results/var_select_fit_bt.rda"))
