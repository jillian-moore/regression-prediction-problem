# Regression AirBnB Problem ----
# Tuning for boosted tree model(s)

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(future)

# Handle conflicts
tidymodels_prefer()

# set seed
set.seed(8687)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load stuff ----
load(here("data_split/reg_folds.rda"))
load(here("data_split/reg_train.rda"))

# load preprocessing/recipe ----
load(here("06_attempt/recipes/6a_recipe.rda"))

# model specifications ----
rf_spec <- rand_forest(
  trees = tune(),
  min_n = tune(),
  mtry = tune()
  ) |> 
  set_engine("ranger") |>
  set_mode("regression")

# update hyperparameters ----
rf_params <- extract_parameter_set_dials(rf_spec) |>
  update(
    trees = trees(c(100, 1500)),
    min_n = min_n(c(2, 40)),
    mtry = mtry(c(2, 50))
  )

# set grid ----
rf_grid <- grid_space_filling(rf_params, size = 30)

# define workflow ----
rf_wflow <-
  workflow() |>
  add_model(rf_spec) |>
  add_recipe(recipe_improved_filtered)

# tune/fit workflow/model ----
tic.clearlog()
tic("rf_spec_bayes")

rf_tune <- tune_bayes(
  rf_wflow,
  resamples = reg_folds,
  initial = 15,
  iter = 20,
  param_info = rf_params,
  metrics = metric_set(mae),
  control = control_bayes(save_workflow = TRUE, verbose = TRUE, no_improve = 15)
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)
rf_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  rf_tune,
  rf_tictoc,
  file = here("06_attempt/results/6a_rf_tune.rda")
)
