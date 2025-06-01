# Regression AirBnB Problem ----
# Pre-processing, initial split

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
# training data ----
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
  add_frequent_amenities_features() |> 
  add_sentiment_features() |> 
  mutate(
    # parse price
    price = as.numeric(parse_number(price)),
    price = if_else(price > 1090, 1090, price), # 1090 is 5*IQR
    price = log(price),
    # parse host verifications
    host_verifications <- parse_verifications_to_factor(host_verifications),
    has_phone = if_else(
      str_detect(as.factor(host_verifications), "'phone'"), 1, 0
      ),
    # price v. neighborhood median
    # price_vs_neighborhood_median = price - ave(price, neighbourhood_cleansed, FUN = median, na.rm = TRUE),
    # make id a character object
    id = as.character(id), 
    # remove percent sign
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(), 
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    # is the airbnb in Kauai
    in_kauai = as.factor(ifelse(listing_location == "kauai", 1, 0)),
    # handle logicals
    host_identity_verified = ifelse(host_identity_verified == TRUE, 1, 0), 
    host_has_profile_pic = ifelse(host_has_profile_pic == TRUE, 1, 0), 
    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0),
    # is there * amenity (found important based on past runs)
    has_pool = as.factor(ifelse(grepl("pool|swimming", tolower(amenities_clean)), 1, 0)),
    has_kitchen = as.factor(ifelse(grepl("kitchen", tolower(amenities_clean)), 1, 0)),
    has_building_staff = as.factor(ifelse(grepl("building staff", tolower(amenities_clean)), 1, 0)),
    has_waterfront = as.factor(ifelse(grepl("waterfront", tolower(amenities_clean)), 1, 0)),
    has_hot_tub = as.factor(ifelse(grepl("hot tub", tolower(amenities_clean)), 1, 0)),
    has_gas_stove = as.factor(ifelse(grepl("gas stove", tolower(amenities_clean)), 1, 0)),
    has_elevator = as.factor(ifelse(grepl("elevator", tolower(amenities_clean)), 1, 0)),
    has_dishes = as.factor(ifelse(grepl("dishes and silverware", tolower(amenities_clean)), 1, 0)),
    # calculate bathroom numbers
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf_bath", "0.5"),
    bathrooms_text = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    # handle dates
    host_since = as.Date(ymd(host_since)),
    first_review = as.Date(ymd(first_review)),
    last_review = as.Date(ymd(last_review)),
    active_host_since = as.numeric(last_review - first_review),
    existing_host_since = as.numeric(as.Date("2025-03-20") - host_since),
    # NEW: enhanced temporal features
    days_since_last_review = as.numeric(as.Date("2025-03-20") - last_review),
    host_experience_years = existing_host_since / 365,
    review_frequency = ifelse(active_host_since > 0, 
                              number_of_reviews / pmax(active_host_since/365, 0.1), 0),
    # NEW: missing value indicators (created before imputation)
    missing_host_response_rate = as.integer(is.na(host_response_rate)),
    missing_host_acceptance_rate = as.integer(is.na(host_acceptance_rate)),
    missing_reviews = as.integer(is.na(last_review)),
    missing_bathrooms = as.integer(is.na(bathrooms_text)),
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
    # NEW: improved availability binning
    availability_30 = case_when(
      availability_30 == 0 ~ "0",
      availability_30 <= 5 ~ "1-5",
      availability_30 <= 15 ~ "6-15",
      availability_30 <= 25 ~ "16-25",
      availability_30 <= 30 ~ "26-30",
      TRUE ~ NA_character_
    ),
    availability_60 = case_when(
      availability_60 == 0 ~ "0",
      availability_60 <= 10 ~ "1-10",
      availability_60 <= 30 ~ "11-30",
      availability_60 <= 50 ~ "31-50",
      availability_60 <= 60 ~ "51-60",
      TRUE ~ NA_character_
    ),
    availability_90 = case_when(
      availability_90 == 0 ~ "0",
      availability_90 <= 15 ~ "1-15",
      availability_90 <= 45 ~ "16-45",
      availability_90 <= 75 ~ "46-75",
      availability_90 <= 90 ~ "76-90",
      TRUE ~ NA_character_
    ),
    # NEW: review recency binning
    review_recency = case_when(
      is.na(days_since_last_review) ~ "never_reviewed",
      days_since_last_review <= 30 ~ "recent",
      days_since_last_review <= 90 ~ "moderate", 
      days_since_last_review <= 365 ~ "old",
      TRUE ~ "very_old"
    ),
    # NEW: number of reviews binning
    reviews_binned = case_when(
      number_of_reviews == 0 ~ "0",
      number_of_reviews <= 5 ~ "1-5",
      number_of_reviews <= 15 ~ "6-15", 
      number_of_reviews <= 50 ~ "16-50",
      number_of_reviews <= 100 ~ "51-100",
      TRUE ~ "100+"
    ),
    # NEW: value and efficiency features
    # price_per_person = ifelse(accommodates > 0, exp(price) / accommodates, NA),
    bathrooms_per_bedroom = ifelse(as.numeric(bedrooms) > 0 & !is.na(bathrooms_text), 
                                   bathrooms_text / as.numeric(bedrooms), bathrooms_text),
    beds_per_person = ifelse(accommodates > 0 & !is.na(as.numeric(beds)), 
                             as.numeric(beds) / accommodates, NA),
    # NEW: host location match
    host_neighbourhood_match = as.integer(
      !is.na(host_neighbourhood) & !is.na(neighbourhood_cleansed) &
        host_neighbourhood == neighbourhood_cleansed
    ),
    across(starts_with("review_scores_"), ~ as.numeric(.)),
    review_scores_rating_bin = cut(
      review_scores_rating,
      breaks = c(0, 2, 4, 4.3, 4.8, 4.9, 5),
      labels = c("very_low", "low", "mid", "high", "very high", "super high"),
      right = TRUE,
      include.lowest = TRUE
    ),
    # safe use here
    quality_quantity_interaction = paste(
      ifelse(is.na(review_scores_rating), "no_rating", 
             cut(review_scores_rating, breaks = 3, labels = c("low", "med", "high"))),
      reviews_binned, sep = "_"
    ),
    # fix old var types
    across(
      any_of(char_to_factor_list),
      factor
    ),
    across(where(is.logical), ~ as.integer(.)),
    # fix new var types
    review_recency = factor(review_recency),
    reviews_binned = factor(reviews_binned),
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
    desc_views = factor(desc_views),
    # NEW: interaction features (after all base features are created)
    # key business-logic interactions for SVM especially
    location_property_interaction = paste(neighbourhood_affluence, property_type_encoded, sep = "_"),
    capacity_bedrooms_interaction = paste(accommodates, bedrooms, sep = "_"),
    quality_quantity_interaction = paste(
      ifelse(is.na(review_scores_rating), "no_rating", 
             cut(review_scores_rating, breaks = 3, labels = c("low", "med", "high"))),
      reviews_binned, sep = "_"
    ),
    availability_booking_interaction = paste(availability_30, instant_bookable, sep = "_"),
    host_quality_interaction = paste(
      ifelse(host_response_rate >= 95, "responsive", "less_responsive"),
      ifelse(host_acceptance_rate >= 80, "accepting", "selective"), 
      sep = "_"
    ),
    # convert interactions to factors
    location_property_interaction = factor(location_property_interaction),
    capacity_bedrooms_interaction = factor(capacity_bedrooms_interaction),
    quality_quantity_interaction = factor(quality_quantity_interaction),
    availability_booking_interaction = factor(availability_booking_interaction),
    host_quality_interaction = factor(host_quality_interaction)
  ) |> 
  select(-first_review, -last_review, -host_since)

# testing data ----
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
  add_frequent_amenities_features() |> 
  add_sentiment_features() |> 
  mutate(
    # parse host verifications
    host_verifications <- parse_verifications_to_factor(host_verifications),
    has_phone = if_else(
      str_detect(as.factor(host_verifications), "'phone'"), 1, 0
    ),
    # make id a character object
    id = as.character(id), 
    # remove percent sign
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(), 
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    # is the airbnb in Kauai
    in_kauai = as.factor(ifelse(listing_location == "kauai", 1, 0)),
    # handle logicals
    host_identity_verified = ifelse(host_identity_verified == TRUE, 1, 0), 
    host_has_profile_pic = ifelse(host_has_profile_pic == TRUE, 1, 0), 
    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0),
    # is there * amenity (found important based on past runs)
    has_pool = as.factor(ifelse(grepl("pool|swimming", tolower(amenities_clean)), 1, 0)),
    has_kitchen = as.factor(ifelse(grepl("kitchen", tolower(amenities_clean)), 1, 0)),
    has_building_staff = as.factor(ifelse(grepl("building staff", tolower(amenities_clean)), 1, 0)),
    has_waterfront = as.factor(ifelse(grepl("waterfront", tolower(amenities_clean)), 1, 0)),
    has_hot_tub = as.factor(ifelse(grepl("hot tub", tolower(amenities_clean)), 1, 0)),
    has_gas_stove = as.factor(ifelse(grepl("gas stove", tolower(amenities_clean)), 1, 0)),
    has_elevator = as.factor(ifelse(grepl("elevator", tolower(amenities_clean)), 1, 0)),
    has_dishes = as.factor(ifelse(grepl("dishes and silverware", tolower(amenities_clean)), 1, 0)),
    # calculate bathroom numbers
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf_bath", "0.5"),
    bathrooms_text = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    # handle dates
    host_since = as.Date(ymd(host_since)),
    first_review = as.Date(ymd(first_review)),
    last_review = as.Date(ymd(last_review)),
    active_host_since = as.numeric(last_review - first_review),
    existing_host_since = as.numeric(as.Date("2025-03-20") - host_since),
    # NEW: enhanced temporal features
    days_since_last_review = as.numeric(as.Date("2025-03-20") - last_review),
    host_experience_years = existing_host_since / 365,
    review_frequency = ifelse(active_host_since > 0, 
                              number_of_reviews / pmax(active_host_since/365, 0.1), 0),
    # NEW: missing value indicators (created before imputation)
    missing_host_response_rate = as.integer(is.na(host_response_rate)),
    missing_host_acceptance_rate = as.integer(is.na(host_acceptance_rate)),
    missing_reviews = as.integer(is.na(last_review)),
    missing_bathrooms = as.integer(is.na(bathrooms_text)),
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
    # NEW: improved availability binning
    availability_30 = case_when(
      availability_30 == 0 ~ "0",
      availability_30 <= 5 ~ "1-5",
      availability_30 <= 15 ~ "6-15",
      availability_30 <= 25 ~ "16-25",
      availability_30 <= 30 ~ "26-30",
      TRUE ~ NA_character_
    ),
    availability_60 = case_when(
      availability_60 == 0 ~ "0",
      availability_60 <= 10 ~ "1-10",
      availability_60 <= 30 ~ "11-30",
      availability_60 <= 50 ~ "31-50",
      availability_60 <= 60 ~ "51-60",
      TRUE ~ NA_character_
    ),
    availability_90 = case_when(
      availability_90 == 0 ~ "0",
      availability_90 <= 15 ~ "1-15",
      availability_90 <= 45 ~ "16-45",
      availability_90 <= 75 ~ "46-75",
      availability_90 <= 90 ~ "76-90",
      TRUE ~ NA_character_
    ),
    # NEW: review recency binning
    review_recency = case_when(
      is.na(days_since_last_review) ~ "never_reviewed",
      days_since_last_review <= 30 ~ "recent",
      days_since_last_review <= 90 ~ "moderate", 
      days_since_last_review <= 365 ~ "old",
      TRUE ~ "very_old"
    ),
    # NEW: number of reviews binning
    reviews_binned = case_when(
      number_of_reviews == 0 ~ "0",
      number_of_reviews <= 5 ~ "1-5",
      number_of_reviews <= 15 ~ "6-15", 
      number_of_reviews <= 50 ~ "16-50",
      number_of_reviews <= 100 ~ "51-100",
      TRUE ~ "100+"
    ),
    # NEW: value and efficiency features
    # price_per_person = ifelse(accommodates > 0, exp(price) / accommodates, NA),
    bathrooms_per_bedroom = ifelse(as.numeric(bedrooms) > 0 & !is.na(bathrooms_text), 
                                   bathrooms_text / as.numeric(bedrooms), bathrooms_text),
    beds_per_person = ifelse(accommodates > 0 & !is.na(as.numeric(beds)), 
                             as.numeric(beds) / accommodates, NA),
    # NEW: host location match
    host_neighbourhood_match = as.integer(
      !is.na(host_neighbourhood) & !is.na(neighbourhood_cleansed) &
        host_neighbourhood == neighbourhood_cleansed
    ),
    across(starts_with("review_scores_"), ~ as.numeric(.)),
    review_scores_rating_bin = cut(
      review_scores_rating,
      breaks = c(0, 2, 4, 4.3, 4.8, 4.9, 5),
      labels = c("very_low", "low", "mid", "high", "very high", "super high"),
      right = TRUE,
      include.lowest = TRUE
    ),
    # safe use here
    quality_quantity_interaction = paste(
      ifelse(is.na(review_scores_rating), "no_rating", 
             cut(review_scores_rating, breaks = 3, labels = c("low", "med", "high"))),
      reviews_binned, sep = "_"
    ),
    # fix old var types
    across(
      any_of(char_to_factor_list),
      factor
    ),
    across(where(is.logical), ~ as.integer(.)),
    # fix new var types
    review_recency = factor(review_recency),
    reviews_binned = factor(reviews_binned),
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
    desc_views = factor(desc_views),
    # NEW: interaction features (after all base features are created)
    # key business-logic interactions for SVM especially
    location_property_interaction = paste(neighbourhood_affluence, property_type_encoded, sep = "_"),
    capacity_bedrooms_interaction = paste(accommodates, bedrooms, sep = "_"),
    quality_quantity_interaction = paste(
      ifelse(is.na(review_scores_rating), "no_rating", 
             cut(review_scores_rating, breaks = 3, labels = c("low", "med", "high"))),
      reviews_binned, sep = "_"
    ),
    availability_booking_interaction = paste(availability_30, instant_bookable, sep = "_"),
    host_quality_interaction = paste(
      ifelse(host_response_rate >= 95, "responsive", "less_responsive"),
      ifelse(host_acceptance_rate >= 80, "accepting", "selective"), 
      sep = "_"
    ),
    # convert interactions to factors
    location_property_interaction = factor(location_property_interaction),
    capacity_bedrooms_interaction = factor(capacity_bedrooms_interaction),
    quality_quantity_interaction = factor(quality_quantity_interaction),
    availability_booking_interaction = factor(availability_booking_interaction),
    host_quality_interaction = factor(host_quality_interaction)
  ) |> 
  select(-first_review, -last_review, -host_since)

# resamples: v-fold ----
reg_folds <- reg_train |>
  vfold_cv(v = 5, repeats = 3, strata = price)

# save out ----
save(reg_train, file = here("data_split/reg_train.rda"))
save(reg_test, file = here("data_split/reg_test.rda"))
save(reg_folds, file = here("data_split/reg_folds.rda"))