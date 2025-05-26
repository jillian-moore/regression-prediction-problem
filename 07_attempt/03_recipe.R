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
load(here("07_attempt/results/var_select_fit_bt.rda"))

# get important variables from selection ----
# look at vars
var_select_bt <- var_select_fit_bt |>
  extract_fit_parsnip() |>
  vip::vi()

# keep 95-98% of total importance
important_vars <- var_select_bt |> 
  filter(Importance >= 0.002) |> 
  pull(Variable)

# recipe ----
recipe_select <- recipe(price ~ ., data = reg_train) |>
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
all_vars <- var_select_bt |> pull(Variable)
vars_to_remove <- setdiff(all_vars, important_vars)

# drop the unimportant ones
recipe_filtered <- recipe_select |>
  step_rm(any_of(!!vars_to_remove))

# prep recipe
recipe_filtered_prep <- 
  prep(
    recipe_filtered,
    training = reg_train,
    retain = TRUE
  )

# bake recipe on both test and train to make sure dummies match
train_baked <- bake(recipe_filtered_prep, new_data = NULL)
test_baked  <- bake(recipe_filtered_prep, new_data = reg_test)

# save recipe ----
save(recipe_filtered, file = here("07_attempt/recipes/7a_recipe.rda"))
