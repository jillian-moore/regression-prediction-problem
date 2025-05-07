# Regression AirBnB Problem ----
# Tuning for boosted tree model(s) ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(doParallel)
library(bonsai)

# Handle conflicts
tidymodels_prefer()

# set seed
set.seed(333)

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = num_cores)

# load resamples ----
load(here("data_split/reg_folds.rda"))

# load preprocessing/recipe ----
load(here("02_recipes/recipes_files/1a_recipe.rda"))

# model specifications ----
bt_spec <-
  parsnip::boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune()
  ) |>
  set_engine("lightgbm") |>
  set_mode("regression")

# update hyperparameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |>
  update(
    trees = trees(range = c(100, 1000)),
    min_n = min_n(range = c(5, 40)),
    tree_depth = tree_depth(range = c(3, 10)),
    learn_rate = learn_rate(range = c(0.001, 0.3))
  )

# set grid ----
bt_grid <- grid_random(bt_params, size = 20)

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
  file = here("results/1a_bt_tune.rda")
)

# get best parameters
best_params <- select_best(bt_tune, metric = "mae")
print(best_params)