# Regression AirBnB Problem ----
# Tuning for boosted tree model(s) ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(devtools)
devtools::install_github("catboost/catboost", subdir = "catboost/R-package")

# Handle conflicts
tidymodels_prefer()

# set seed
set.seed(333)

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)

# load resamples ----
load(here("data_split/reg_folds.rda"))

# load preprocessing/recipe ----
load(here("02_recipes/1a_recipe_basic.rda"))

# model specifications ----
bt_spec <- 
  boost_tree(
    trees = tune(),                
    min_n = tune(),              
    learn_rate = tune()
  ) |> 
  set_engine("catboost") |> 
  set_mode("regression")

# update hyperparameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |> 
  update(
    trees = trees(range = c(100, 1000)),
    min_n = min_n(range = c(5, 40)),
    learn_rate = learn_rate(range = c(-4, -0.5))
  )

# set grid ----
bt_grid <- grid_random(bt_params,  size = 20)

# define workflow ----
bt_wflow <- 
  workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(recipe_basic)

# tune/fit workflow/model ----
tic.clearlog() # clear log
tic("bt_spec") # start clock

bt_tune <-
  tune_grid(
    bt_wflow,
    resamples = reg_folds,
    grid = bt_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(mae)
  )

toc(log = TRUE)

# extract runtime info
time_log <- tic.log(format = FALSE)

bt_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  bt_basic_tune,
  bt_basic_tictoc,
  file = here("results/1a_bt_tune.rda")
)

best_params <- select_best(bt_basic_tune, metric = "mae")
