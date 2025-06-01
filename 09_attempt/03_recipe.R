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
load(here("data_split/reg_test.rda"))
load(here("09_attempt/results/9a_model_comparison.rda"))
load(here("09_attempt/results/9a_var_select_fit_bt.rda"))

# recipe ----
recipe_for_selection <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean,
          host_about, host_about_clean) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.01) |>  
  step_unknown(all_nominal_predictors()) |>             
  step_novel(all_nominal_predictors()) |> 
  step_YeoJohnson(all_numeric_predictors(), -starts_with("amenity_")) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |>
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_numeric_predictors())

# filter for important vars ----
# identify variables to remove
var_select_bt <- var_select_fit_bt |>
  extract_fit_parsnip() |>
  vip::vi()
all_vars <- var_select_bt |> pull(Variable)
vars_to_remove <- setdiff(all_vars, intersect_vars)

# drop the unimportant ones
recipe_filtered <- recipe_select |>
  step_rm(any_of(!!vars_to_remove))

# check recipe
recipe_filtered_prep <- prep(recipe_filtered)

# bake recipe on both test and train to make sure dummies match
train_baked <- bake(recipe_filtered_prep, new_data = NULL)

required_cols <- recipe_filtered_prep$var_info$variable[recipe_filtered_prep$var_info$role %in% c("predictor", "outcome")]
missing_cols <- setdiff(required_cols, names(reg_test))

# remove extra columns
if(length(missing_cols) > 0) {
  for(col in missing_cols) {
    reg_test[[col]] <- 0  
  }
}

# fix factor levels on test set to match training set
for (col_name in names(reg_test)) {
  if (is.factor(reg_test[[col_name]]) && col_name %in% names(reg_train)) {
    train_levels <- levels(reg_train[[col_name]])
    reg_test[[col_name]] <- factor(reg_test[[col_name]], levels = train_levels)
  }
}

# add missing columns (zeros for dummies)
required_cols <- recipe_filtered_prep$var_info$variable[recipe_filtered_prep$var_info$role %in% c("predictor", "outcome")]
missing_cols <- setdiff(required_cols, names(reg_test))
for (col in missing_cols) {
  reg_test[[col]] <- 0
}

test_baked <- bake(recipe_filtered_prep, new_data = reg_test)

# save recipe ----
save(recipe_filtered, file = here("09_attempt/recipes/9a_recipe.rda"))
