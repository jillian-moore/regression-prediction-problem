# Regression AirBnB Problem ----
# Setup preprocessing/recipes/feature engineering

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# load objects ----
load(here("data_split/reg_train.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores - 1)

# recipe ----
simple_recipe <- recipe(price ~ ., data = reg_train) |>
  update_role(id, new_role = "ID") |>
  step_zv(all_predictors()) |>
  step_mutate_at(where(is.logical), fn = as.numeric) |>
  step_mutate_at(all_nominal_predictors(), fn = as.factor) |>
  step_mutate(
    reviews_weighted = reviews_per_month * review_scores_rating,
    amenity_density = amenity_count / (accommodates + 1)
  ) |>
  step_rm(
    maximum_maximum_nights,
    host_total_listings_count,
    minimum_maximum_nights,
    host_acceptance_rate,
    number_of_reviews,
    review_scores_value,
    minimum_nights_avg_ntm,
    review_scores_cleanliness,
    review_scores_rating,
    room_type,
    availability_365
  )

simple_recipe |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(simple_recipe) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

save(simple_recipe, file = here("12_attempt/recipes/12a_recipe.rda"))