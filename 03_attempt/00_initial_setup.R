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

char_to_factor_list <- read.csv(here("data/train.csv")) |>
  select(where(is.character)) |>
  select(
    # perm categorical var
    -description, -host_about, -amenities, 
    # addressed in pre-processing
    -host_response_rate, -host_acceptance_rate, -bathrooms_text, 
    # become dates in pre-processing
    host_since, first_review, last_review, 
    # target var
    -price
    ) |>
  names()

# pre-processing steps that don't fit well in a recipe ----
# training data
reg_train <- read.csv(here("data/train.csv")) |> 
  # replace low frequency neighborhoods
  replace_low_freq_neighborhoods() |> 
  # encode property type
  encode_property_type() |> 
  # calculate hosting length
  calculate_hosting_length() |> 
  # categorize neighborhoods
  categorize_neighborhoods_by_affluence() |> 
  categorize_neighborhoods_by_geography() |> 
  process_airbnb_amenities_simple() |> 
  mutate(
    # parse price
    price = log(as.numeric(parse_number(price))),
    # remove percent sign
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(), 
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    # handle logicals
    host_identity_verified = ifelse(host_identity_verified == TRUE, 1, 0), 
    host_has_profile_pic = ifelse(host_has_profile_pic == TRUE, 1, 0), 
    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0),
    host_about = ifelse(nchar(host_about) > 100, 1, 0),
    # make id a character object
    id = as.character(id), 
    # is there key amenities
    has_pool = as.factor(ifelse(grepl("pool|swimming", tolower(amenities_clean)), 1, 0)),
    has_wifi = as.factor(ifelse(grepl("wifi|internet|broadband|fast wifi|high-speed|high speed", tolower(amenities_clean)), 1, 0)),
    has_balcony = as.factor(ifelse(grepl("balcony|patio|terrace", tolower(amenities_clean)), 1, 0)),
    has_free_parking = as.factor(ifelse(grepl("free parking", tolower(amenities_clean)), 1, 0)),
    # calculate bathroom numbers
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf_bath", "0.5"),
    bathrooms_text = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    # handle dates
    host_since = as.Date(ymd(host_since)),
    first_review = as.Date(ymd(first_review)),
    last_review = as.Date(ymd(last_review)),
    active_host_since = as.numeric(last_review - first_review),
    # remove outliers
    minimum_minimum_nights = pmin(pmax(minimum_minimum_nights, quantile(minimum_minimum_nights, 0.05, na.rm = TRUE)),
                             quantile(minimum_minimum_nights, 0.95, na.rm = TRUE)),
    minimum_maximum_nights = pmin(pmax(minimum_maximum_nights, quantile(minimum_maximum_nights, 0.05, na.rm = TRUE)),
                                  quantile(minimum_maximum_nights, 0.95, na.rm = TRUE)),
    maximum_minimum_nights = pmin(pmax(maximum_minimum_nights, quantile(maximum_minimum_nights, 0.05, na.rm = TRUE)),
                                  quantile(maximum_minimum_nights, 0.95, na.rm = TRUE)),
    maximum_maximum_nights = pmin(pmax(maximum_maximum_nights, quantile(maximum_maximum_nights, 0.05, na.rm = TRUE)),
                                  quantile(maximum_maximum_nights, 0.95, na.rm = TRUE)),
    # make some char vars factors
    across(
      any_of(char_to_factor_list),
      factor
    )
  ) |> 
  select(-host_since, first_review, last_review) |> 
  # fix new var types
  mutate(
    # this one is new numeric
    hosting_length = as.numeric(hosting_length), 
    # these are factors
    host_neighbourhood_affluence = factor(host_neighbourhood_affluence),
    neighbourhood_affluence = factor(neighbourhood_affluence),
    host_neighbourhood_geo = factor(host_neighbourhood_geo),
    neighbourhood_geo = factor(neighbourhood_geo),
    property_type_encoded = factor(property_type_encoded),
    neighbourhood_cleansed = factor(neighbourhood_cleansed)
  )

# testing data
reg_test <- read.csv(here("data/test.csv")) |> 
  # replace low frequency neighborhoods
  replace_low_freq_neighborhoods() |> 
  # encode property type
  encode_property_type() |> 
  # calculate hosting length
  calculate_hosting_length() |> 
  # categorize neighborhoods
  categorize_neighborhoods_by_affluence() |> 
  categorize_neighborhoods_by_geography() |> 
  process_airbnb_amenities_simple() |> 
  mutate(
    # remove percent sign
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(), 
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    # handle logicals
    host_identity_verified = ifelse(host_identity_verified == TRUE, 1, 0), 
    host_has_profile_pic = ifelse(host_has_profile_pic == TRUE, 1, 0), 
    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0),
    host_about = ifelse(nchar(host_about) > 100, 1, 0),
    # make id a character object
    id = as.character(id), 
    # is there key amenities
    has_pool = as.factor(ifelse(grepl("pool|swimming", tolower(amenities_clean)), 1, 0)),
    has_wifi = as.factor(ifelse(grepl("wifi|internet|broadband|fast wifi|high-speed|high speed", tolower(amenities_clean)), 1, 0)),
    has_balcony = as.factor(ifelse(grepl("balcony|patio|terrace", tolower(amenities_clean)), 1, 0)),
    has_free_parking = as.factor(ifelse(grepl("free parking", tolower(amenities_clean)), 1, 0)),
    # calculate bathroom numbers
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf_bath", "0.5"),
    bathrooms_text = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    # handle dates
    host_since = as.Date(ymd(host_since)),
    first_review = as.Date(ymd(first_review)),
    last_review = as.Date(ymd(last_review)),
    active_host_since = as.numeric(last_review - first_review),
    # remove outliers
    minimum_minimum_nights = pmin(pmax(minimum_minimum_nights, quantile(minimum_minimum_nights, 0.05, na.rm = TRUE)),
                                  quantile(minimum_minimum_nights, 0.95, na.rm = TRUE)),
    minimum_maximum_nights = pmin(pmax(minimum_maximum_nights, quantile(minimum_maximum_nights, 0.05, na.rm = TRUE)),
                                  quantile(minimum_maximum_nights, 0.95, na.rm = TRUE)),
    maximum_minimum_nights = pmin(pmax(maximum_minimum_nights, quantile(maximum_minimum_nights, 0.05, na.rm = TRUE)),
                                  quantile(maximum_minimum_nights, 0.95, na.rm = TRUE)),
    maximum_maximum_nights = pmin(pmax(maximum_maximum_nights, quantile(maximum_maximum_nights, 0.05, na.rm = TRUE)),
                                  quantile(maximum_maximum_nights, 0.95, na.rm = TRUE)),
    # make some char vars factors
    across(
      any_of(char_to_factor_list),
      factor
    )
  ) |> 
  select(-host_since, first_review, last_review) |> 
  # fix new var types
  mutate(
    # this one is new numeric
    hosting_length = as.numeric(hosting_length), 
    # these are factors
    host_neighbourhood_affluence = factor(host_neighbourhood_affluence),
    neighbourhood_affluence = factor(neighbourhood_affluence),
    host_neighbourhood_geo = factor(host_neighbourhood_geo),
    neighbourhood_geo = factor(neighbourhood_geo),
    property_type_encoded = factor(property_type_encoded),
    neighbourhood_cleansed = factor(neighbourhood_cleansed)
  )

# resamples: v-fold ----
reg_folds <- reg_train |>
  vfold_cv(v = 5, repeats = 3, strata = price)

# save out ----
save(reg_train, file = here("data_split/reg_train.rda"))
save(reg_test, file = here("data_split/reg_test.rda"))
save(reg_folds, file = here("data_split/reg_folds.rda"))
