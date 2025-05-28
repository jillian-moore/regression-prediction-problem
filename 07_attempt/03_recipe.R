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
load(here("07_attempt/results/7a_var_select_fit_bt.rda"))
load(here("07_attempt/results/7a_var_select_lasso_fit.rda"))

# get important variables from selection ----
# look at vars
var_select_bt <- var_select_fit_bt |>
  extract_fit_parsnip() |>
  vip::vi()

var_select_lasso <- var_select_lasso_fit |>
  extract_fit_parsnip() |>
  vip::vi()

# keep 95-98% of total importance
threshold <- 0.97
important_bt_vars <- var_select_bt |> 
  arrange(desc(Importance)) |> 
  mutate(cum_importance = cumsum(Importance)) |> 
  filter(cum_importance <= threshold) |> 
  pull(Variable)

# cut variables that lasso did not find important
important_vars <- var_select_lasso |> 
  filter(Variable %in% important_bt_vars, Importance > 0) |> 
  pull(Variable)

# recipe ----
recipe_select <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean, 
          host_about, host_about_clean) |> 
  step_nzv(all_predictors()) |> 
  step_impute_median(all_numeric_predictors()) |> # switched from knn
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

# check recipe
recipe_filtered_prep |> 
  prep() |>
  bake(new_data = NULL)

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
save(recipe_filtered, file = here("07_attempt/recipes/7a_recipe.rda"))
