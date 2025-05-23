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
load(here("06_attempt/results/var_select_fit_bt.rda"))

# get important variables from selection ----
# look at vars
var_select_bt <- var_select_fit_bt |>
  extract_fit_parsnip() |>
  vip::vi()

important_vars <- var_select_bt |> 
  filter(Importance >= 0.002) |> 
  pull(Variable)

# recipe ----
recipe_improved <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean) |> 
  step_nzv(all_predictors()) |> 
  step_impute_knn(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_YeoJohnson(all_numeric_predictors()) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.005) |> 
  step_unknown(all_nominal_predictors()) |>             
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |>
  step_corr(all_predictors(), threshold = 0.95) |> 
  step_normalize(all_numeric_predictors())

# filter for important vars ----
# identify variables to remove
all_vars <- var_select_bt |> pull(Variable)
vars_to_remove <- setdiff(all_vars, important_vars)

# drop the unimportant ones
recipe_improved_filtered <- recipe_improved |>
  step_rm(any_of(vars_to_remove))

# check recipe
recipe_improved_filtered |> 
  prep() |>
  bake(new_data = NULL)

# save recipe ----
save(recipe_improved_filtered, file = here("06_attempt/recipes/6a_recipe.rda"))