# Regression AirBnB Problem ----
# Tuning for boosted tree model(s) across three cities

# load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(bonsai)
library(future)

# handle conflicts
tidymodels_prefer()

# set seed
set.seed(834237)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load objects ----
load(here("13_attempt/data_split/my_metrics.rda"))

# load recipes ----
load(here("13_attempt/recipes/13a_recipe_asheville.rda"))
load(here("13_attempt/recipes/13a_recipe_chicago.rda"))
load(here("13_attempt/recipes/13a_recipe_kauai.rda"))

# load data ----
load(here("13_attempt/data_split/asheville_folds.rda"))
load(here("13_attempt/data_split/chicago_folds.rda"))
load(here("13_attempt/data_split/kauai_folds.rda"))

load(here("13_attempt/data_split/asheville_train.rda"))
load(here("13_attempt/data_split/chicago_train.rda"))
load(here("13_attempt/data_split/kauai_train.rda"))

# model specifications ----
bt_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune(),
  mtry = tune(),
  tree_depth = tune(),
  loss_reduction = tune()
) |> 
  set_engine("lightgbm") |>
  set_mode("regression")

# update hyperparameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |>
  update(
    trees = trees(range = c(200, 1000)),
    min_n = min_n(range = c(5, 40)),
    learn_rate = learn_rate(range = c(0.01, 0.5)),  
    mtry = mtry(range = c(5, 45)),
    tree_depth = tree_depth(range = c(3, 15))
  )

# set grid ----
bt_grid_raw <- grid_space_filling(bt_params, size = 15)

if(max(bt_grid_raw$learn_rate) > 0.3) {
  cat("Grid space filling failed - correcting learn_rate manually...\n")
  
  bt_grid <- bt_grid_raw %>%
    mutate(
      learn_rate = scales::rescale(learn_rate, to = c(0.01, 0.5))
    )
} else {
  bt_grid <- bt_grid_raw
}

# create location-specific configurations ----
locations <- list(
  asheville = list(
    recipe = simple_recipe_asheville,  
    folds = asheville_folds,
    data = asheville_train
  ),
  chicago = list(
    recipe = simple_recipe_chicago,   
    folds = chicago_folds,
    data = chicago_train
  ),
  kauai = list(
    recipe = simple_recipe_kauai,      
    folds = kauai_folds,
    data = kauai_train
  )
)

# function to tune each location ----
tune_location <- function(location_name, location_config) {
  
  # define workflow
  bt_wflow <- workflow() |>
    add_model(bt_spec) |>
    add_recipe(location_config$recipe)
  
  # tune model
  tic(paste(location_name, "bt_gbm_tune"))
  bt_tune_result <- bt_wflow |>
    tune_grid(
      resamples = location_config$folds,
      metrics = my_metrics,
      grid = bt_grid,
      control = stacks::control_stack_grid()
    )
  toc(log = TRUE)
  
  # get timing info
  time_log <- tic.log(format = FALSE)
  latest_time <- time_log[[length(time_log)]]
  
  # extract best parameters and performance
  best_params <- select_best(bt_tune_result, metric = "mae")
  best_performance <- show_best(bt_tune_result, metric = "mae", n = 1)
  
  # create summary
  location_summary <- tibble(
    location = location_name,
    best_mae = best_performance$mean,
    best_mae_se = best_performance$std_err,
    trees = best_params$trees,
    min_n = best_params$min_n,
    learn_rate = best_params$learn_rate,
    mtry = best_params$mtry,
    tree_depth = best_params$tree_depth,
    loss_reduction = best_params$loss_reduction,
    runtime_seconds = latest_time$toc - latest_time$tic,
    n_observations = nrow(location_config$data)
  )
  
  # save individual results
  save(
    bt_tune_result,
    file = here("13_attempt/results", paste0("13a_bt_gbm_tune_", location_name, ".rda"))
  )
  
  return(list(
    summary = location_summary,
    tune_result = bt_tune_result,
    best_params = best_params
  ))
}

# clear timing log before starting
tic.clearlog()

# tune all location ----
all_results <- map2(names(locations), locations, tune_location)
names(all_results) <- names(locations)

# combine all summaries into one dataframe ----
combined_summary <- map_dfr(all_results, ~.x$summary)

# export to CSV ----
write_csv(
  combined_summary, 
  here("13_attempt/results/13a_bt_tuning_summary.csv")
)

# save the combined results object ----
save(
  all_results,
  combined_summary,
  bt_grid,
  file = here("13_attempt/results/13a_bt_combined_results.rda")
)