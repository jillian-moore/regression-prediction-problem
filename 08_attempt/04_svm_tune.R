# Regression AirBnB Problem ----
# Tune SVM RBF model

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(here)
library(kernlab)
library(future)

# Handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load stuff ----
load(here("data_split/reg_folds.rda"))

# load required objects ----
load(here("07_attempt/recipes/7a_recipe.rda"))

# model specification ----
svm_spec <-
  svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) |>
  set_mode("regression") |>
  set_engine("kernlab")

# set-up tuning grid ----
svm_params <- hardhat::extract_parameter_set_dials(svm_spec)

# define grid
svm_grid <- grid_space_filling(svm_params, size = 20)

# workflow ----
svm_wflow <-
  workflow() |>
  add_model(svm_spec) |>
  add_recipe(recipe_filtered)

# my metrics ----
my_metrics <- metric_set(mae)

# tuning/fitting ----
svm_tune <-
  svm_wflow |>
  tune_grid(
    resamples = reg_folds,
    grid = svm_grid,
    metrics = my_metrics,
    control = stacks::control_stack_grid()
  )

# write out results & workflow ----
save(svm_tune, file = here("08_attempt/results/8a_svm_tune.rda"))
