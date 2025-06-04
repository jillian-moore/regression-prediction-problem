# Regression AirBnB Problem ----
# Variable selection by location

# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)
library(purrr)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("14_attempt/data_split/asheville_train.rda"))
load(here("14_attempt/data_split/chicago_train.rda"))
load(here("14_attempt/data_split/kauai_train.rda"))

# create list of location datasets
location_datasets <- list(
  asheville = asheville_train,
  chicago = chicago_train,
  kauai = kauai_train
)

# function to create recipe for a specific location ----
create_location_recipe <- function(location_data) {
  recipe(price ~ ., data = location_data) |>
    update_role(id, new_role = "ID") |>
    step_zv(all_predictors()) |>
    step_mutate_at(where(is.logical), fn = as.numeric) |>
    step_mutate_at(all_nominal_predictors(), fn = as.factor) |>
    step_mutate(
      reviews_weighted = reviews_per_month * review_scores_rating,
      amenity_density = amenities_count / (accommodates + 1)
    )
}

# perform variable selection for each location ----
perform_location_selection <- function(location_name, location_data) {
  
  set.seed(958347)
  location_folds <- location_data |>
    vfold_cv(v = 5, repeats = 3, strata = price)
  
  location_recipe <- create_location_recipe(location_data)
  
  # model specification
  bt_spec <- boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(),
    learn_rate = tune(),
    tree_depth = tune()
  ) |>
    set_mode("regression") |>
    set_engine("lightgbm")
  
  # define workflow
  bt_wflow <- workflow() |>
    add_model(bt_spec) |>
    add_recipe(location_recipe)
  
  # hyperparameter tuning values
  bt_params <- extract_parameter_set_dials(bt_spec) |>
    update(
      mtry = mtry(range = c(2, min(50, ncol(location_data) - 2))),
      min_n = min_n(range = c(1, 30)),
      trees = trees(range = c(200, 1500)),
      tree_depth = tree_depth(range = c(3, 15)),
      learn_rate = learn_rate(range = c(0.0001, 0.4))
    )
  
  # build tuning grid
  bt_grid <- grid_space_filling(bt_params, size = 15)
  
  # fit workflow/model
  bt_tuned <- bt_wflow |>
    tune_grid(
      resamples = location_folds,
      grid = bt_grid,
      metrics = metric_set(mae),
      control = control_grid(save_workflow = TRUE)
    )
  
  # extract best model
  optimal_wflow <- extract_workflow(bt_tuned) |>
    finalize_workflow(select_best(bt_tuned, metric = "mae"))
  
  # fit best model
  var_select_fit_bt <- fit(optimal_wflow, location_data)
  
  return(var_select_fit_bt)
}

# process asheville
var_select_fit_bt_asheville <- perform_location_selection("asheville", asheville_train)
save(var_select_fit_bt_asheville, file = here("14_attempt/results/14a_var_select_fit_bt_asheville.rda"))

# process chicago
var_select_fit_bt_chicago <- perform_location_selection("chicago", chicago_train)
save(var_select_fit_bt_chicago, file = here("14_attempt/results/14a_var_select_fit_bt_chicago.rda"))

# process kauai
var_select_fit_bt_kauai <- perform_location_selection("kauai", kauai_train)
save(var_select_fit_bt_kauai, file = here("14_attempt/results/14a_var_select_fit_bt_kauai.rda"))