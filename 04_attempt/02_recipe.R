# Regression AirBnB Problem ----
# Setup preprocessing/recipes/feature engineering

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load training data ----
load(here("data_split/reg_train.rda"))

# recipe ----
recipe_improved <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean, host_since, first_review, last_review) |> 
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_string2factor(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.005) |> 
  step_unknown(all_nominal_predictors()) |>             
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_nzv(all_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_corr(all_predictors(), threshold = 0.95) |> 
  step_normalize(all_numeric_predictors())

# check recipe
recipe_improved |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()

# save recipe ----
save(recipe_improved, file = here("04_attempt/recipes/4a_recipe.rda"))