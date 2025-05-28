# Regression AirBnB Problem ----
# Mini dataset prep work

# BT ----

# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("data_split/reg_mini.rda"))

# create resamples/folds ----
set.seed(9587)
bt_folds <- reg_mini |> 
  vfold_cv(v = 5, repeats = 3, strata = price)

# selection recipe ----
recipe_select <- recipe(price ~ ., data = reg_mini) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean) |> 
  step_nzv(all_predictors()) |> 
  step_impute_knn(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_string2factor(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.01) |>  
  step_unknown(all_nominal_predictors()) |>             
  step_novel(all_nominal_predictors()) |> 
  step_YeoJohnson(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |>
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_numeric_predictors())

# check recipe
recipe_select |> 
  prep() |> 
  bake(new_data = NULL)

# model specifications ----
bt_spec <-
  boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(),
    learn_rate = tune(),
    tree_depth = tune()
  ) |> 
  set_mode("regression") |> 
  set_engine("lightgbm")

# define workflows ----
bt_wflow <-
  workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(recipe_select)

# hyperparameter tuning values ----
bt_params <- extract_parameter_set_dials(bt_spec) |> 
  update(
    mtry = mtry(range = c(2, 50)),
    min_n = min_n(range = c(1, 30)),
    trees = trees(range = c(200, 1500)),
    tree_depth = tree_depth(range = c(3, 15)),
    learn_rate = learn_rate(range = c(0.001, 0.4))
  )

# build tuning grid
bt_grid <- grid_space_filling(bt_params, size = 15)

# fit workflow/model ----
bt_tuned <-
  bt_wflow |> 
  tune_grid(
    resamples = bt_folds,
    grid = bt_grid,
    metrics = metric_set(mae),
    control = control_grid(save_workflow = TRUE)
  )

# extract best model (optimal tuning parameters) ----
optimal_wflow <- extract_workflow(bt_tuned) |> 
  finalize_workflow(select_best(bt_tuned, metric = "mae"))

# fit best model/results ----
var_select_fit_bt_mini <- fit(optimal_wflow, reg_mini)

# write out variable selection results ----
save(var_select_fit_bt_mini, file = here("07_attempt/results/7a_var_select_fit_bt_mini.rda"))

# LASSO ----

# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(broom)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("data_split/reg_mini.rda"))

# create resamples/folds ----
set.seed(9587)
lasso_folds <- reg_mini |> 
  vfold_cv(v = 5, repeats = 3, strata = price)

# selection recipe ----
recipe_select <- recipe(price ~ ., data = reg_mini) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean) |> 
  step_nzv(all_predictors()) |> 
  step_impute_knn(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_string2factor(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.01) |>  
  step_unknown(all_nominal_predictors()) |>             
  step_novel(all_nominal_predictors()) |> 
  step_YeoJohnson(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |>
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_numeric_predictors())

# check recipe
recipe_select |> 
  prep() |> 
  bake(new_data = NULL)

# model specifications ----
lasso_spec <- 
  linear_reg(
    penalty = tune(), 
    mixture = 1
  ) |> 
  set_mode("regression") |> 
  set_engine("glmnet")

# define workflows ----
lasso_wflow <- 
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(recipe_select)

# hyperparameter tuning values ----
lasso_tuned <- tune_grid(
  lasso_wflow,
  resamples = lasso_folds,
  grid = 15,
  metrics = metric_set(mae)
)

# pull out non-zero coefficients ----
best_params <- select_best(lasso_tuned, metric = "mae")
best_lasso <- finalize_workflow(lasso_wflow, best_params)

# fit best model/results
var_select_lasso_fit_mini <- fit(best_lasso, data = reg_mini)

# tidy up
nonzero_vars <- tidy(var_select_lasso_fit_mini) |> filter(estimate != 0) |> pull(term)

# write out variable selection results ----
save(var_select_lasso_fit_mini, file = here("07_attempt/results/7a_var_select_lasso_fit_mini.rda"))

# MINI RECIPE ----

# load objects ----
load(here("data_split/reg_mini.rda"))
load(here("07_attempt/results/7a_var_select_fit_bt_mini.rda"))

# get important variables from selection ----
# look at vars
var_select_bt_mini <- var_select_fit_bt_mini |>
  extract_fit_parsnip() |>
  vip::vi()

# keep 95-98% of total importance
threshold <- 0.97
important_vars <- var_select_bt_mini  |> 
  arrange(desc(Importance)) |> 
  mutate(cum_importance = cumsum(Importance)) |> 
  filter(cum_importance <= threshold) |> 
  pull(Variable)

# recipe ----
recipe_select <- recipe(price ~ ., data = reg_mini) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean) |> 
  step_nzv(all_predictors()) |> 
  step_impute_knn(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_string2factor(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.01) |>  
  step_unknown(all_nominal_predictors()) |>             
  step_novel(all_nominal_predictors()) |> 
  step_YeoJohnson(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |>
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_numeric_predictors())

# filter for important vars ----
# identify variables to remove
all_vars <- var_select_bt_mini |> pull(Variable)
vars_to_remove <- setdiff(all_vars, important_vars)

# drop the unimportant ones
recipe_filtered <- recipe_select |>
  step_rm(any_of(!!vars_to_remove))

# prep recipe
recipe_filtered_prep <- 
  prep(
    recipe_filtered,
    training = reg_mini,
    retain = TRUE
  )

# bake recipe on both test and train to make sure dummies match
mini_baked <- bake(recipe_filtered_prep, new_data = NULL)

# save recipe ----
save(recipe_filtered, file = here("07_attempt/recipes/7a_recipe_mini.rda"))

# CATBOOST ----
library(catboost)

# set seed
set.seed(8957)

reg_folds_mini <- reg_mini |>
  vfold_cv(v = 5, repeats = 3, strata = price)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# model specifications ----
bt_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune(),
  mtry = tune(),
  tree_depth = tune()
) |> 
  set_engine("catboost") |>
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

# define workflow ----
bt_wflow <-
  workflow() |>
  add_model(bt_spec) |>
  add_recipe(recipe_filtered)

# tune/fit workflow/model ----
tic.clearlog()
tic("bt_cat_tune")

bt_cat_tune_mini <- bt_wflow |>
  tune_grid(
    resamples = reg_folds_mini,
    grid = bt_grid,
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
  bt_cat_tune_mini,
  bt_tictoc,
  file = here("07_attempt/results/7a_bt_cat_tune_mini.rda")
)

# BOOSTED TREE ----

library(bonsai)
library(tictoc)

# set seed
set.seed(8957)

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# model specifications ----
bt_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune(),
  mtry = tune(),
  tree_depth = tune()
) |> 
  set_engine("lightgbm") |>
  set_mode("regression")

# update hyperparameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |>
  update(
    trees       = trees(range = c(200, 1000)),
    min_n       = min_n(range = c(5, 40)),
    learn_rate  = learn_rate(range = c(-4, 0), trans = scales::log10_trans()),
    mtry        = mtry(range = c(5, 45)),
    tree_depth  = tree_depth(range = c(3, 15))
  )


# set grid ----
bt_grid_raw <- grid_space_filling(bt_params, size = 15)

# if(max(bt_grid_raw$learn_rate) > 0.3) {
#   cat("Grid space filling failed - correcting learn_rate manually...\n")
#   
#   bt_grid <- bt_grid_raw %>%
#     mutate(
#       learn_rate = scales::rescale(learn_rate, to = c(0.01, 0.5))
#     )
# } else {
#   bt_grid <- bt_grid_raw
# }

# define workflow ----
bt_wflow <-
  workflow() |>
  add_model(bt_spec) |>
  add_recipe(recipe_filtered)

# tune/fit workflow/model ----
tic.clearlog()
tic("bt_gbm_tune")

bt_gbm_tune_mini <- bt_wflow |>
  tune_grid(
    resamples = reg_folds_mini,
    grid = bt_grid,
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
  bt_gbm_tune_mini,
  bt_tictoc,
  file = here("07_attempt/results/7a_bt_gbm_tune_mini.rda")
)

bt_gbm_tune_mini |> 
  autoplot()

# SVM ----
# Regression AirBnB Problem ----
# Tune SVM RBF model

# Load package(s) ----
library(here)
library(kernlab)
library(future)

# Handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

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
svm_grid <- grid_space_filling(svm_params, size = 15)

# workflow ----
svm_wflow <-
  workflow() |>
  add_model(svm_spec) |>
  add_recipe(recipe_filtered)

# tuning/fitting ----
svm_tune_mini <-
  svm_wflow |>
  tune_grid(
    resamples = reg_folds_mini,
    grid = svm_grid,
    control = stacks::control_stack_grid()
  )

# write out results & workflow ----
save(svm_tune_mini, file = here("07_attempt/results/7a_svm_tune_mini.rda"))

# ENSEMBLE

library(stacks)

# Load candidate model info ----
load(here("07_attempt/results/7a_bt_gbm_tune_mini.rda"))
load(here("07_attempt/results/7a_svm_tune_mini.rda"))

# create data stack ----
reg_data_stacks <- 
  stacks() |> 
  add_candidates(bt_gbm_tune_mini)  
  #add_candidates(svm_tune_mini)

# fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^seq(-8, -1, length.out = 20), 0.25, 0.5, 1, 1.5, 2)

# blend predictions (tuning step, set seed)
set.seed(9854) # needed bc tuning process happening in background
reg_stack_blend_mini <- reg_data_stacks |> 
  blend_predictions()

# save blended model stack
save(reg_stack_blend_mini, file = here("07_attempt/results/7a_reg_stack_blend_mini.rda"))

# explore the blended model stack ----
reg_stack_blend |> 
  autoplot()

reg_stack_blend |> 
  autoplot(type = "members")

# fit to training set ----
reg_ensemble_mini <- fit_members(reg_stack_blend_mini)

# save trained ensemble model ----
save(reg_ensemble_mini, file = here("07_attempt/results/7a_reg_ensemble_mini.rda"))
