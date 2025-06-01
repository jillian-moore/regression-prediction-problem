# attempt 8 recipe

library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()

load(here("attempt_9/data_split/airbnb_train.rda"))

num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores - 1)

airbnb_recipe <- recipe(price_log10 ~ ., data = airbnb_train) |>
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

airbnb_recipe |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(airbnb_recipe) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

save(airbnb_recipe, file = here("attempt_9/recipes/airbnb_recipe.rda"))

