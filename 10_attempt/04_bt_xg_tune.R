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
set.seed(8957)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load stuff ----
load(here("data_split/reg_folds.rda"))

# load preprocessing/recipe ----
load(here("10_attempt/recipes/10b_recipe.rda"))

# model specifications ----
bt_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune(),
  mtry = tune(),
  tree_depth = tune(),
  loss_reduction = tune()
) |> 
  set_engine("xgboost") |>
  set_mode("regression")

# update hyperparameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |>
  update(
    trees = trees(range = c(200, 1000)),
    min_n = min_n(range = c(5, 40)),
    learn_rate = learn_rate(range = c(0.01, 0.5)),  
    mtry = mtry(range = c(5, 45)),
    tree_depth = tree_depth(range = c(3, 15)),
    loss_reduction = loss_reduction(range = c(-10, 1.5))
  )

# set grid ----
bt_grid_raw <- grid_space_filling(bt_params, size = 20)

if(max(bt_grid_raw$learn_rate) > 0.3) {
  cat("Grid space filling failed - correcting learn_rate manually...\n")
  
  bt_grid <- bt_grid_raw %>%
    mutate(
      learn_rate = scales::rescale(learn_rate, to = c(0.01, 0.5))
    )
} else {
  bt_grid <- bt_grid_raw
}

# define workflow ----
bt_wflow <-
  workflow() |>
  add_model(bt_spec) |>
  add_recipe(recipe_filtered)

# tune/fit workflow/model ----
tic.clearlog()
tic("bt_tune")

bt_xg_tune <- bt_wflow |>
  tune_grid(
    resamples = reg_folds,
    grid = bt_grid,
    metrics = metric_set(mae),
    control = stacks::control_stack_grid()
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
  bt_xg_tune,
  bt_tictoc,
  file = here("10_attempt/results/10b_bt_xg_tune.rda")
)