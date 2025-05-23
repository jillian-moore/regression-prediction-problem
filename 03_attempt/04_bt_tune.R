# Regression AirBnB Problem ----
# Tuning for boosted tree model(s)

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(doParallel)
library(bonsai)
library(future)

# Handle conflicts
tidymodels_prefer()

# set seed
set.seed(3434)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load stuff ----
load(here("data_split/reg_folds.rda"))
load(here("data_split/reg_train.rda"))

# load preprocessing/recipe ----
load(here("03_attempt/recipes/3a_recipe.rda"))

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
    min_n = min_n(c(5, 40)),
    learn_rate = learn_rate(c(0.01, 0.3)),
    mtry = finalize(mtry(), reg_train)
  )

# set grid ----
bt_grid <- grid_random(bt_params, size = 5)

# define workflow ----
bt_wflow <-
  workflow() |>
  add_model(bt_spec) |>
  add_recipe(recipe_improved)

# tune/fit workflow/model ----
tic.clearlog() # clear log
tic("bt_spec") # start clock
bt_tune <-
  tune_grid(
    bt_wflow,
    resamples = reg_folds,
    grid = bt_grid,
    control = control_grid(save_workflow = TRUE, verbose = TRUE),
    metrics = metric_set(mae)
  )
toc(log = TRUE) # extract runtime info

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
  file = here("03_attempt/results/3a_bt_tune.rda")
)
