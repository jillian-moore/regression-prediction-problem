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
set.seed(8697)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load stuff ----
load(here("data_split/reg_folds.rda"))
load(here("data_split/reg_train.rda"))

# load preprocessing/recipe ----
load(here("07_attempt/recipes/7a_recipe.rda"))

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
    mtry = mtry(c(2, 40))
  )

# set grid ----
rf_grid <- grid_space_filling(rf_params, size = 30)

# define workflow ----
rf_wflow <-
  workflow() |>
  add_model(rf_spec) |>
  add_recipe(recipe_filtered)

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
  control = tune::control_bayes(
    no_improve = 15,
    verbose_iter = TRUE,
    save_pred = TRUE, 
    save_workflow = TRUE
  )
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
#  test note
save(
  rf_tune,
  rf_tictoc,
  file = here("07_attempt/results/7a_rf_tune.rda")
)
