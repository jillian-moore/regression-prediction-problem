# Regression AirBnB Problem ----
# Fit linear regression model

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(here)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# load required objects ----
load(here("data_split/reg_folds.rda"))

# load preprocessing/recipe ----
load(here("07_attempt/recipes/7a_recipe.rda"))

# model specification ----
en_spec <- linear_reg(
  penalty = tune(),     
  mixture = tune()     
  ) |> 
  set_mode("regression") |> 
  set_engine("glmnet")

# workflow ----
en_wflow <- 
  workflow() |> 
  add_model(en_spec) |> 
  add_recipe(recipe_filtered)

# build grid ----
en_grid <- grid_regular(
  penalty(),
  mixture(),
  levels = c(penalty = 10, mixture = 5)
)

# tune resamples ----
en_tune <- en_wflow |> 
  tune_grid(
    resamples = reg_folds,
    grid = en_grid,
    metrics = metric_set(mae),
    control = control_stack_resamples()
  )

# save out
save(en_tune, file = here("07_attempt/results/7a_en_tune.rda"))
