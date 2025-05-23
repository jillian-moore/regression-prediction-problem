# Regression AirBnB Problem ----
# Tuning for boosted tree model(s)

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(bonsai)
library(future)

# Handle conflicts
tidymodels_prefer()

# set seed
set.seed(8987)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load stuff ----
load(here("data_split/reg_folds.rda"))
load(here("data_split/reg_train.rda"))

# load preprocessing/recipe ----
load(here("06_attempt/recipes/6a_recipe.rda"))

# model specifications ----
bt_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune(),
  mtry = tune(),
  tree_depth = tune()
) |> 
  set_engine("lightgbm") |>
  set_mode("regression")

# update hyperparameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |>
  update(
    trees = trees(c(100, 1500)),
    min_n = min_n(c(2, 40)),
    learn_rate = learn_rate(c(0.005, 0.7)),
    mtry = mtry(c(2, 50)), 
    tree_depth = tree_depth(range = c(3, 15))
  )

# set grid ----
bt_grid <- grid_space_filling(bt_params, size = 30)

# define workflow ----
bt_wflow <-
  workflow() |>
  add_model(bt_spec) |>
  add_recipe(recipe_improved_filtered)

# tune/fit workflow/model ----
tic.clearlog()
tic("bt_spec_bayes")

bt_tune <- tune_bayes(
  bt_wflow,
  resamples = reg_folds,
  initial = 15,
  iter = 20,
  param_info = bt_params,
  metrics = metric_set(mae),
  control = control_bayes(save_workflow = TRUE, verbose = TRUE, no_improve = 15)
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)
bt_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  bt_tune,
  bt_tictoc,
  file = here("06_attempt/results/6a_bt_tune.rda")
)
