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
load(here("13_attempt/data_split/asheville_train.rda"))
load(here("13_attempt/data_split/chicago_train.rda"))
load(here("13_attempt/data_split/kauai_train.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores - 1)

# recipes ----
# asheville
simple_recipe_asheville <- recipe(price ~ ., data = asheville_train) |>
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

simple_recipe_asheville |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(simple_recipe_asheville) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

# chicago
simple_recipe_chicago <- recipe(price ~ ., data = chicago_train) |>
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

simple_recipe_chicago |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(simple_recipe_chicago) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

# kauai
simple_recipe_kauai <- recipe(price ~ ., data = kauai_train) |>
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

simple_recipe_kauai |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(simple_recipe_kauai) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

save(simple_recipe_asheville, file = here("13_attempt/recipes/13a_recipe_asheville.rda"))
save(simple_recipe_chicago, file = here("13_attempt/recipes/13a_recipe_chicago.rda"))
save(simple_recipe_kauai, file = here("13_attempt/recipes/13a_recipe_kauai.rda"))


