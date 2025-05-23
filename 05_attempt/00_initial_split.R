# Regression AirBnB Problem ----
# Pre-processing reg_train, initial split

# load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# source helper functions ----
source(here("helper_functions.R"))

# handle common conflicts
tidymodels_prefer()

# load important vars for later
load(here("05_attempt/results/important_vars"))

# initial split & resamples ----
reg_train <- read.csv(here("data/train.csv"))
reg_test <- read.csv(here("data/test.csv"))

# character to factor list
char_to_factor_list <- read.csv(here("data/train.csv")) |>
  select(where(is.character)) |>
  select(
    # perm categorical var
    -description, -host_about, -amenities, 
    # addressed in pre-processing
    -host_response_rate, -host_acceptance_rate, -bathrooms_text, 
    # become dates in pre-processing
    -host_since, -first_review, -last_review, 
    # target var
    -price
  ) |>
  names()

# PRE-PROCESSING ----
# training data
reg_train <- read.csv(here("data/train.csv")) |> 
  select(
    -host_listings_count, 
    -minimum_maximum_nights, 
    -maximum_maximum_nights,
    -maximum_nights_avg_ntm
  ) |> 
  # replace low frequency neighborhoods
  replace_low_freq_neighborhoods() |> 
  # encode property type
  encode_property_type() |> 
  # categorize neighborhoods
  categorize_neighborhoods_by_affluence() |> 
  categorize_neighborhoods_by_geography() |> 
  # work on description
  add_description_categories() |> 
  # clean amenities
  add_amenities_features() |> 
  add_text_features() |> 
  add_sentiment_features() |> 
  mutate(
    # parse price
    price = log(as.numeric(parse_number(price))),
    # make id a character object
    id = as.character(id), 
    # remove percent sign
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(), 
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    # handle logicals
    host_identity_verified = ifelse(host_identity_verified == TRUE, 1, 0), 
    host_has_profile_pic = ifelse(host_has_profile_pic == TRUE, 1, 0), 
    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0),
    # are there key amenities
    has_pool = as.factor(ifelse(grepl("pool|swimming", tolower(amenities_clean)), 1, 0)),
    has_wifi = as.factor(ifelse(grepl("wifi|internet|broadband|fast wifi|high-speed|high speed", tolower(amenities_clean)), 1, 0)),
    has_balcony = as.factor(ifelse(grepl("balcony|patio|terrace", tolower(amenities_clean)), 1, 0)),
    has_free_parking = as.factor(ifelse(grepl("free parking", tolower(amenities_clean)), 1, 0)),
    has_outdoor_space = as.factor(ifelse(grepl("backyard|garden|yard|outdoor|patio", tolower(amenities_clean)), 1, 0)),
    has_longterm_stays = as.factor(ifelse(grepl("long term|longterm", tolower(amenities_clean)), 1, 0)),
    has_ac = as.factor(ifelse(grepl("air condition|ac|a/c|cooling", tolower(amenities_clean)), 1, 0)),
    has_kitchen = as.factor(ifelse(grepl("kitchen|kitchenette|oven|dishwasher|stove|refrigerator|fridge", tolower(amenities_clean)), 1, 0)),
    # calculate bathroom numbers
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf_bath", "0.5"),
    bathrooms_text = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    # handle dates
    host_since = as.Date(ymd(host_since)),
    first_review = as.Date(ymd(first_review)),
    last_review = as.Date(ymd(last_review)),
    active_host_since = as.numeric(last_review - first_review),
    # log transform stuff from EDA
    host_total_listings_count = log(host_total_listings_count),
    minimum_nights = log(minimum_nights),
    minimum_minimum_nights = log(minimum_minimum_nights),
    maximum_minimum_nights = log(maximum_minimum_nights),
    minimum_nights_avg_ntm = log(minimum_nights_avg_ntm),
    number_of_reviews = log(number_of_reviews),
    number_of_reviews_ltm = log(number_of_reviews_ltm),
    number_of_reviews_l30d = log(number_of_reviews_l30d),
    calculated_host_listings_count = log(calculated_host_listings_count),
    calculated_host_listings_count_entire_homes = log(calculated_host_listings_count_entire_homes),
    calculated_host_listings_count_private_rooms = log(calculated_host_listings_count_private_rooms),
    reviews_per_month = log(reviews_per_month),
    # bin stuff from EDA
    calculated_host_listings_count_shared_rooms = ifelse(calculated_host_listings_count_shared_rooms == 1, 1, 0),
    bedrooms = case_when(
      bedrooms == 0 ~ "0",
      bedrooms == 1 ~ "1",
      bedrooms == 2 ~ "2",
      bedrooms == 3 ~ "3",
      bedrooms == 4 ~ "4",
      bedrooms == 5 ~ "5",
      bedrooms >= 6 ~ "6+",
      TRUE ~ NA_character_
    ),
    beds = case_when(
      beds == 0 ~ "0",
      beds == 1 ~ "1",
      beds == 2 ~ "2",
      beds == 3 ~ "3",
      beds == 4 ~ "4",
      beds == 5 ~ "5",
      beds >= 6 & beds <= 10 ~ "6-10",
      beds > 10 ~ "11+",
      TRUE ~ NA_character_
    ),
    availability_30 = case_when(
      availability_30 == 0 ~ "0",
      availability_30 <= 10 ~ "1–10",
      availability_30 <= 20 ~ "11–20",
      availability_30 <= 30 ~ "21–30",
      TRUE ~ NA_character_
    ),
    availability_60 = case_when(
      availability_60 == 0 ~ "0",
      availability_60 <= 15 ~ "1–15",
      availability_60 <= 30 ~ "16–30",
      availability_60 <= 45 ~ "31–45",
      availability_60 <= 60 ~ "46–60",
      TRUE ~ NA_character_
    ),
    availability_90 = case_when(
      availability_90 == 0 ~ "0",
      availability_90 <= 15 ~ "1–15",
      availability_90 <= 30 ~ "16–30",
      availability_90 <= 60 ~ "31–60",
      availability_90 <= 90 ~ "61–90",
      TRUE ~ NA_character_
    ),
    across(starts_with("review_scores_"), ~ cut(
      .,
      breaks = c(0, 2, 4, 4.3, 4.8, 4.9, 5),
      labels = c("very_low", "low", "mid", "high", "very high", "super high"),
      right = TRUE,
      include.lowest = TRUE
    )),
    # fix old var types
    across(
      any_of(char_to_factor_list),
      factor
    ),
    # fix new var types
    host_neighbourhood_affluence = factor(host_neighbourhood_affluence),
    neighbourhood_affluence = factor(neighbourhood_affluence),
    host_neighbourhood_geo = factor(host_neighbourhood_geo),
    neighbourhood_geo = factor(neighbourhood_geo),
    property_type_encoded = factor(property_type_encoded),
    neighbourhood_cleansed = factor(neighbourhood_cleansed),
    desc_location = factor(desc_location),
    desc_extra_fees = factor(desc_extra_fees),
    desc_parking = factor(desc_parking),
    desc_transport = factor(desc_transport),
    desc_amenities = factor(desc_amenities),
    desc_family_pets = factor(desc_family_pets),
    desc_discount_price = factor(desc_discount_price),
    desc_style_vibe = factor(desc_style_vibe),
    desc_views = factor(desc_views)
  ) |> 
  # addressing -Inf from log values
  mutate(
    across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))
  ) |> 
  select(-first_review, -last_review, -host_since) 

# testing data
reg_test <- read.csv(here("data/test.csv")) |> 
  select(
    -host_listings_count, 
    -minimum_maximum_nights, 
    -maximum_maximum_nights,
    -maximum_nights_avg_ntm
  ) |> 
  # replace low frequency neighborhoods
  replace_low_freq_neighborhoods() |> 
  # encode property type
  encode_property_type() |> 
  # categorize neighborhoods
  categorize_neighborhoods_by_affluence() |> 
  categorize_neighborhoods_by_geography() |> 
  # work on description
  add_description_categories() |> 
  # clean amenities
  add_amenities_features() |> 
  add_text_features() |> 
  add_sentiment_features() |> 
  mutate(
    # make id a character object
    id = as.character(id), 
    # remove percent sign
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(), 
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    # handle logicals
    host_identity_verified = ifelse(host_identity_verified == TRUE, 1, 0), 
    host_has_profile_pic = ifelse(host_has_profile_pic == TRUE, 1, 0), 
    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0),
    # are there key amenities
    has_pool = as.factor(ifelse(grepl("pool|swimming", tolower(amenities_clean)), 1, 0)),
    has_wifi = as.factor(ifelse(grepl("wifi|internet|broadband|fast wifi|high-speed|high speed", tolower(amenities_clean)), 1, 0)),
    has_balcony = as.factor(ifelse(grepl("balcony|patio|terrace", tolower(amenities_clean)), 1, 0)),
    has_free_parking = as.factor(ifelse(grepl("free parking", tolower(amenities_clean)), 1, 0)),
    has_outdoor_space = as.factor(ifelse(grepl("backyard|garden|yard|outdoor|patio", tolower(amenities_clean)), 1, 0)),
    has_longterm_stays = as.factor(ifelse(grepl("long term|longterm", tolower(amenities_clean)), 1, 0)),
    has_ac = as.factor(ifelse(grepl("air condition|ac|a/c|cooling", tolower(amenities_clean)), 1, 0)),
    has_kitchen = as.factor(ifelse(grepl("kitchen|kitchenette|oven|dishwasher|stove|refrigerator|fridge", tolower(amenities_clean)), 1, 0)),
    # calculate bathroom numbers
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf_bath", "0.5"),
    bathrooms_text = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    # handle dates
    host_since = as.Date(ymd(host_since)),
    first_review = as.Date(ymd(first_review)),
    last_review = as.Date(ymd(last_review)),
    active_host_since = as.numeric(last_review - first_review),
    # log transform stuff from EDA
    host_total_listings_count = log(host_total_listings_count),
    minimum_nights = log(minimum_nights),
    minimum_minimum_nights = log(minimum_minimum_nights),
    maximum_minimum_nights = log(maximum_minimum_nights),
    minimum_nights_avg_ntm = log(minimum_nights_avg_ntm),
    number_of_reviews = log(number_of_reviews),
    number_of_reviews_ltm = log(number_of_reviews_ltm),
    number_of_reviews_l30d = log(number_of_reviews_l30d),
    calculated_host_listings_count = log(calculated_host_listings_count),
    calculated_host_listings_count_entire_homes = log(calculated_host_listings_count_entire_homes),
    calculated_host_listings_count_private_rooms = log(calculated_host_listings_count_private_rooms),
    reviews_per_month = log(reviews_per_month),
    # bin stuff from EDA
    calculated_host_listings_count_shared_rooms = ifelse(calculated_host_listings_count_shared_rooms == 1, 1, 0),
    bedrooms = case_when(
      bedrooms == 0 ~ "0",
      bedrooms == 1 ~ "1",
      bedrooms == 2 ~ "2",
      bedrooms == 3 ~ "3",
      bedrooms == 4 ~ "4",
      bedrooms == 5 ~ "5",
      bedrooms >= 6 ~ "6+",
      TRUE ~ NA_character_
    ),
    beds = case_when(
      beds == 0 ~ "0",
      beds == 1 ~ "1",
      beds == 2 ~ "2",
      beds == 3 ~ "3",
      beds == 4 ~ "4",
      beds == 5 ~ "5",
      beds >= 6 & beds <= 10 ~ "6-10",
      beds > 10 ~ "11+",
      TRUE ~ NA_character_
    ),
    availability_30 = case_when(
      availability_30 == 0 ~ "0",
      availability_30 <= 10 ~ "1–10",
      availability_30 <= 20 ~ "11–20",
      availability_30 <= 30 ~ "21–30",
      TRUE ~ NA_character_
    ),
    availability_60 = case_when(
      availability_60 == 0 ~ "0",
      availability_60 <= 15 ~ "1–15",
      availability_60 <= 30 ~ "16–30",
      availability_60 <= 45 ~ "31–45",
      availability_60 <= 60 ~ "46–60",
      TRUE ~ NA_character_
    ),
    availability_90 = case_when(
      availability_90 == 0 ~ "0",
      availability_90 <= 15 ~ "1–15",
      availability_90 <= 30 ~ "16–30",
      availability_90 <= 60 ~ "31–60",
      availability_90 <= 90 ~ "61–90",
      TRUE ~ NA_character_
    ),
    across(starts_with("review_scores_"), ~ cut(
      .,
      breaks = c(0, 2, 4, 4.3, 4.8, 4.9, 5),
      labels = c("very_low", "low", "mid", "high", "very_high", "super high"),
      right = TRUE,
      include.lowest = TRUE
    )),
    # fix old var types
    across(
      any_of(char_to_factor_list),
      factor
    ),
    # fix new var types
    host_neighbourhood_affluence = factor(host_neighbourhood_affluence),
    neighbourhood_affluence = factor(neighbourhood_affluence),
    host_neighbourhood_geo = factor(host_neighbourhood_geo),
    neighbourhood_geo = factor(neighbourhood_geo),
    property_type_encoded = factor(property_type_encoded),
    neighbourhood_cleansed = factor(neighbourhood_cleansed),
    desc_location = factor(desc_location),
    desc_extra_fees = factor(desc_extra_fees),
    desc_parking = factor(desc_parking),
    desc_transport = factor(desc_transport),
    desc_amenities = factor(desc_amenities),
    desc_family_pets = factor(desc_family_pets),
    desc_discount_price = factor(desc_discount_price),
    desc_style_vibe = factor(desc_style_vibe),
    desc_views = factor(desc_views)
  ) |> 
  # addressing -Inf from log values
  mutate(
    across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))
  ) |> 
  select(-first_review, -last_review, -host_since)

# resamples: v-fold ----
reg_folds <- reg_train |>
  vfold_cv(v = 5, repeats = 3, strata = price)

# save out ----
save(reg_train, file = here("data_split/reg_train.rda"))
save(reg_test, file = here("data_split/reg_test.rda"))
save(reg_folds, file = here("data_split/reg_folds.rda"))