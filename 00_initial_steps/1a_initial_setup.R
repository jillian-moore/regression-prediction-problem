# Regression AirBnB Problem ----
# Processing reg_training, creating resamples, missingness & factor EDA

# load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# source helper functions ----
source(here("helper_functions.R"))

# handle common conflicts
tidymodels_prefer()

# initial split & resamples ----
reg_train <- read.csv(here("data/train.csv"))
reg_test <- read.csv(here("data/test.csv"))

char_to_factor_list <- reg_train |>
  select(where(is.character)) |>
  select(-description, -host_about, -amenities, -host_response_rate, 
         -host_acceptance_rate, -bathrooms_text, -host_neighbourhood, -neighbourhood_cleansed) |>
  names()

# pre-processing steps that don't fit well in a recipe
reg_train <- read.csv(here("data/train.csv")) |> 
  mutate(
    # parse price
    price = parse_number(price),
    # remove percent sign
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(), 
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    # make id a character object
    id = as.character(id), 
    # calculate bathroom numbers
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf_bath", "0.5"),
    num_bathrooms = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    # make some char vars factors
    across(
      any_of(char_to_factor_list),
      factor
    )
  ) |> 
  select(-bathrooms_text) |> 
  # replace low frequency neighborhoods
  replace_low_freq_neighborhoods() |> 
  # encode property type
  encode_property_type() |> 
  # calculate hosting length
  calculate_hosting_length() |> 
  # categorize neighborhoods
  categorize_neighborhoods_by_affluence() |> 
  categorize_neighborhoods_by_geography()

# resamples: v-fold ----
reg_folds <- reg_train |>
  vfold_cv(v = 5, repeats = 3, strata = price)

# save out ----
save(reg_train, file = here("data_split/reg_train.rda"))
save(reg_test, file = here("data_split/reg_test.rda"))
save(reg_folds, file = here("data_split/reg_folds.rda"))
