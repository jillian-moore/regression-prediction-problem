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
set.seed(8989)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load stuff ----
load(here("data_split/reg_folds.rda"))
load(here("data_split/reg_train.rda"))

# load preprocessing/recipe ----
load(here("04_attempt/recipes/4a_recipe.rda"))

# model specifications ----
bt_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune(),
  mtry = tune()
  ) |> 
  set_engine("lightgbm") |>
  set_mode("regression")

# update hyperparameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |>
  update(
    trees = trees(c(100, 1000)),
    min_n = min_n(c(2, 40)),
    learn_rate = learn_rate(c(0.005, 0.5)),
    mtry = finalize(mtry(), reg_train)
  )

# set grid ----
bt_grid <- grid_random(bt_params, size = 20)

# define workflow ----
bt_wflow <-
  workflow() |>
  add_model(bt_spec) |>
  add_recipe(recipe_improved)

# tune/fit workflow/model ----
tic.clearlog()
tic("bt_spec_bayes")

bt_tune <- tune_bayes(
  bt_wflow,
  resamples = reg_folds,
  initial = 10,
  iter = 20,
  param_info = bt_params,
  metrics = metric_set(mae),
  control = control_bayes(verbose = TRUE, no_improve = 10)
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
  file = here("04_attempt/results/4a_bt_tune.rda")
)
