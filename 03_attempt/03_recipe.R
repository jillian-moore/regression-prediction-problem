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
load(here("03_attempt/results/var_select_fit_bt.rda"))

# get important variables from selection ----
# look at vars
var_select_bt <- var_select_fit_bt |>
  extract_fit_parsnip() |>
  vip::vi()

important_vars <- var_select_bt |> 
  filter(Importance >= 0.005) |> 
  pull(Variable)

# recipe ----
recipe_improved <- recipe(price ~ ., data = reg_train) |>
  step_rm(description, amenities, amenities_clean, id) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_mutate(log_number_of_reviews = log(number_of_reviews + 1)) |> 
  step_mutate_at(
    c("host_has_profile_pic", "host_identity_verified", "has_availability"),
    fn = ~as.factor(.)
  ) |> 
  step_string2factor(all_nominal_predictors()) |>       
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_nzv(all_predictors()) |> 
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_numeric_predictors())

# check recipe
recipe_improved |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()

# save recipe ----
save(recipe_improved, file = here("03_attempt/recipes/3a_recipe.rda"))