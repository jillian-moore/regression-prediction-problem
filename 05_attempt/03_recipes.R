# Regression AirBnB Problem ----
# Setup preprocessing/recipes/feature engineering

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load objects ----
load(here("data_split/reg_train.rda"))
load(here("05_attempt/results/var_select_fit_bt.rda"))

# get important variables from selection ----
# look at vars
var_select_bt <- var_select_fit_bt |>
  extract_fit_parsnip() |>
  vip::vi()

important_vars <- var_select_bt |> 
  filter(Importance >= 0.005) |> 
  pull(Variable)

save(important_vars, file = here("05_attempt/results/important_vars"))

# recipe ----
recipe_improved <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean) |> 
  # add yeo johnson for last 30 days and anything with many 0 values
  step_impute_median(all_numeric_predictors()) |> # do it with knn
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
# inter accomodates and number rooms
# bathrooms and number of rooms
# bathrooms and beds
# try an ensemble for a bunch of boosted tree

# check recipe and filter for important vars
baked_improved <- recipe_improved |>
  prep() |>
  bake(new_data = NULL) |>
  select(all_of(important_vars))

# save recipe ----
save(recipe_improved, file = here("05_attempt/recipes/5a_recipe.rda"))