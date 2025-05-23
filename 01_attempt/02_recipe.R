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

###############################################################################
# basic first attempt recipe
###############################################################################

recipe_basic <- recipe(price ~ ., data = reg_train) |> 
  step_rm(description, amenities, host_about, id) |> 
  # impute missing values with median
  step_impute_median(all_numeric_predictors()) |> 
  # impute missing cat values with mode
  step_impute_mode(all_nominal_predictors()) |> 
  # convert boolean to numeric (0/1)
  step_mutate(instant_bookable = ifelse(instant_bookable == "t", 1, 0)) |> 
  # log-transform number of reviews
  step_mutate(log_number_of_reviews = log(number_of_reviews + 1)) |> 
  step_mutate_at(
    c("host_has_profile_pic", "host_identity_verified", "has_availability"),
    fn = ~as.factor(.)
    ) |> 
  # extract year
  step_date(last_review, features = "year") |> 
  step_date(host_since, features = "year") |>
  step_date(first_review, features = "year") |>
  step_rm(last_review, host_since, first_review) |> 
  step_rename(last_review = last_review_year) |> 
  step_rename(first_review = first_review_year) |>
  step_rename(host_since = host_since_year) |>
  # create dummy variables for categorical features
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  # remove near/zero variance
  step_zv(all_predictors()) |> 
  step_nzv(all_predictors()) |> 
  # remove highly correlated
  step_corr(all_predictors(), threshold = 0.9) |> 
  # normalize
  step_normalize(all_numeric_predictors())  

# check recipe
recipe_basic |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()

# save recipe ----
save(recipe_basic, file = here("02_recipes/recipes_files/1a_recipe.rda"))

