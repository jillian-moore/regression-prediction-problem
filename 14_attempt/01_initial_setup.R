# Regression AirBnB Problem ----
# Pre-processing, initial split

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(stringr)
library(lubridate)

# handle common conflicts
tidymodels_prefer()

# source functions ----
source("helper_functions.R")

# PRE_PROCESSING ----
# training data
reg_train <- read.csv(here("data/train.csv")) |>
  clean_names() |> 
  add_text_features() |> 
  add_sentiment_features() |> 
  add_amenities_features() |> 
  add_frequent_amenities_features() |> 
  mutate(
    # make id a character object
    id = as.character(id),
    # parse price
    price = parse_number(price),
    price = log10(price),
    # remove percent sign and handle warnings
    host_response_rate = na_if(host_response_rate, "N/A") |> str_remove("%") |> as.numeric() / 100,
    host_acceptance_rate = na_if(host_acceptance_rate, "N/A") |> str_remove("%") |> as.numeric() / 100,
    host_response_rate = if_else(is.na(host_response_rate), median(host_response_rate, na.rm = TRUE), host_response_rate),
    host_acceptance_rate = if_else(is.na(host_acceptance_rate), median(host_acceptance_rate, na.rm = TRUE), host_acceptance_rate),
    # calculate bathroom numbers and handle warnings
    bathrooms_text = str_replace_all(bathrooms_text, regex("half[- ]?bath", ignore_case = TRUE), "0.5"),
    num_bathrooms = suppressWarnings(as.numeric(str_remove_all(bathrooms_text, "[A-Za-z ]"))),
    num_bathrooms = if_else(num_bathrooms > 5, 5, num_bathrooms),
    num_bathrooms = if_else(is.na(num_bathrooms), median(num_bathrooms, na.rm = TRUE), num_bathrooms),
    # natural language processing
    description_length = if_else(is.na(description), 0L, nchar(description)),
    host_about_length = if_else(is.na(host_about), 0L, nchar(host_about)),
    # handle dates
    host_since = ymd(host_since),
    first_review = ymd(first_review),
    host_tenure_days = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    days_since_first_review = as.numeric(Sys.Date() - as.Date(first_review)),
    # host listings
    host_many_listings = if_else(calculated_host_listings_count > 3, 1, 0),
    is_single_property_host = if_else(calculated_host_listings_count == 1, 1, 0),
    # binary host description
    has_host_about = if_else(is.na(host_about), 0, 1),
    has_description = if_else(is.na(description), 0, 1),
    # missing info
    missing_response_rate = is.na(host_response_rate),
    missing_acceptance_rate = is.na(host_acceptance_rate),
    missing_num_bathrooms = is.na(num_bathrooms),
    missing_review_score = is.na(review_scores_rating),
    missing_reviews_per_month = is.na(reviews_per_month),
    reviews_per_month = if_else(is.na(reviews_per_month), 0, reviews_per_month),
    review_scores_rating = if_else(is.na(review_scores_rating), median(review_scores_rating, na.rm = TRUE), review_scores_rating)
  ) |>
  select(-bathrooms_text, -amenities, -description, -host_about, -host_since, 
         -first_review, -last_review, -description_clean, -host_about_clean)

char_to_factor <- reg_train |>
  select(where(is.character)) |>
  select(-id) |>
  names()

reg_train <-reg_train |>
  mutate(across(all_of(char_to_factor), factor))

# testing data
reg_test <- read.csv(here("data/test.csv")) |>
  clean_names() |> 
  add_text_features() |> 
  add_sentiment_features() |> 
  add_amenities_features() |> 
  add_frequent_amenities_features() |> 
  mutate(
    # make id a character object
    id = as.character(id),
    # remove percent sign and handle warnings
    host_response_rate = na_if(host_response_rate, "N/A") |> str_remove("%") |> as.numeric() / 100,
    host_acceptance_rate = na_if(host_acceptance_rate, "N/A") |> str_remove("%") |> as.numeric() / 100,
    host_response_rate = if_else(is.na(host_response_rate), median(host_response_rate, na.rm = TRUE), host_response_rate),
    host_acceptance_rate = if_else(is.na(host_acceptance_rate), median(host_acceptance_rate, na.rm = TRUE), host_acceptance_rate),
    # calculate bathroom numbers and handle warnings
    bathrooms_text = str_replace_all(bathrooms_text, regex("half[- ]?bath", ignore_case = TRUE), "0.5"),
    num_bathrooms = suppressWarnings(as.numeric(str_remove_all(bathrooms_text, "[A-Za-z ]"))),
    num_bathrooms = if_else(num_bathrooms > 5, 5, num_bathrooms),
    num_bathrooms = if_else(is.na(num_bathrooms), median(num_bathrooms, na.rm = TRUE), num_bathrooms),
    # natural language processing
    description_length = if_else(is.na(description), 0L, nchar(description)),
    host_about_length = if_else(is.na(host_about), 0L, nchar(host_about)),
    # handle dates
    host_since = ymd(host_since),
    first_review = ymd(first_review),
    host_tenure_days = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    days_since_first_review = as.numeric(Sys.Date() - as.Date(first_review)),
    # host listings
    host_many_listings = if_else(calculated_host_listings_count > 3, 1, 0),
    is_single_property_host = if_else(calculated_host_listings_count == 1, 1, 0),
    # binary host description
    has_host_about = if_else(is.na(host_about), 0, 1),
    has_description = if_else(is.na(description), 0, 1),
    # missing info
    missing_response_rate = is.na(host_response_rate),
    missing_acceptance_rate = is.na(host_acceptance_rate),
    missing_num_bathrooms = is.na(num_bathrooms),
    missing_review_score = is.na(review_scores_rating),
    missing_reviews_per_month = is.na(reviews_per_month),
    reviews_per_month = if_else(is.na(reviews_per_month), 0, reviews_per_month),
    review_scores_rating = if_else(is.na(review_scores_rating), median(review_scores_rating, na.rm = TRUE), review_scores_rating)
  ) |>
  select(-bathrooms_text, -amenities, -description, -host_about, -host_since, 
         -first_review, -last_review, -description_clean, -host_about_clean)

char_to_factor <- reg_test |>
  select(where(is.character)) |>
  select(-id) |>
  names()

reg_test <-reg_test |>
  mutate(across(all_of(char_to_factor), factor))

# split by listing location ----
# training data
asheville_train <- reg_train |> 
  filter(str_detect(tolower(listing_location), "asheville")) |> 
  # from var selection
  mutate(
    reviews_weighted = reviews_per_month * review_scores_rating,
    amenity_density = amenities_count / (accommodates + 1)) |>
  mutate(
    has_shared_patio = str_detect(amenities_clean, regex("shared patio or balcony", ignore_case = TRUE)),
    has_elevator = str_detect(amenities_clean, regex("elevator", ignore_case = TRUE)),
    has_lock = str_detect(amenities_clean, regex("lock on bedroom door", ignore_case = TRUE)),
    has_hair_dryer = str_detect(amenities_clean, regex("hair dryer", ignore_case = TRUE)),
    has_priv_entrance = str_detect(amenities_clean, regex("private entrance", ignore_case = TRUE)),
    has_free_parking = str_detect(amenities_clean, regex("free street parking", ignore_case = TRUE))
  ) |> 
  select(
    everything(),
    -c(
      instant_bookable, 
      beds, 
      host_has_profile_pic, 	
      property_type, 	
      calculated_host_listings_count, 	
      host_about_length, 
      description_length, 	
      calculated_host_listings_count_entire_homes, 
      neighbourhood_cleansed, 
      maximum_nights_avg_ntm, 
      number_of_reviews_l30d,
      number_of_reviews,
      amenities_clean,
      matches("^amenity_"),
      -amenity_density,
      -amenities_count
    )
  )

chicago_train <- reg_train |> 
  filter(str_detect(tolower(listing_location), "chicago")) |> 
  mutate(
    reviews_weighted = reviews_per_month * review_scores_rating,
    amenity_density = amenities_count / (accommodates + 1)
  ) |>
  mutate(
    has_skyline = str_detect(amenities_clean, regex("city skyline view", ignore_case = TRUE)),
    has_elevator = str_detect(amenities_clean, regex("elevator", ignore_case = TRUE)),
    has_lock = str_detect(amenities_clean, regex("lock on bedroom door", ignore_case = TRUE)),
    has_dishwasher = str_detect(amenities_clean, regex("dishwasher", ignore_case = TRUE)),
    has_building_staff = str_detect(amenities_clean, regex("building staff", ignore_case = TRUE)),
    has_freezer = str_detect(amenities_clean, regex("freezer", ignore_case = TRUE))
  ) |> 
  select(
    everything(),
    -c(
      host_has_profile_pic, 
      missing_review_score, 
      has_description, 
      host_location, 	
      has_host_about, 
      host_many_listings, 	
      is_single_property_host, 	
      host_verifications, 
      host_identity_verified, 
      maximum_maximum_nights, 
      instant_bookable,
      amenities_clean,
      matches("^amenity_"),
      -amenity_density,
      -amenities_count
    )
  )

kauai_train <- reg_train |> 
  filter(str_detect(tolower(listing_location), "kauai")) |> 
  mutate(
    reviews_weighted = reviews_per_month * review_scores_rating,
    amenity_density = amenities_count / (accommodates + 1)
  ) |>
  mutate(
    has_single_level = str_detect(amenities_clean, regex("single level home", ignore_case = TRUE)),
    has_dishes = str_detect(amenities_clean, regex("has dishes and silverware", ignore_case = TRUE)),
    has_waterfront = str_detect(amenities_clean, regex("waterfront", ignore_case = TRUE)),
    has_dishwasher = str_detect(amenities_clean, regex("dishwasher", ignore_case = TRUE)),
    has_first_aid = str_detect(amenities_clean, regex("first aid kit", ignore_case = TRUE))
  ) |> 
  select(
    everything(),
    -c(
      has_description, 
      missing_review_score, 
      room_type, 
      host_response_time, 
      host_has_profile_pic,
      host_many_listings, 
      is_single_property_host, 
      instant_bookable, 	
      host_verifications, 
      host_response_rate, 
      has_host_about,
      amenities_clean,
      matches("^amenity_"),
      -amenity_density,
      -amenities_count
    )
  )

# testing data
asheville_test <- reg_test |> 
  filter(str_detect(tolower(listing_location), "asheville")) |> 
  # from var selection
  mutate(
    reviews_weighted = reviews_per_month * review_scores_rating,
    amenity_density = amenities_count / (accommodates + 1)) |>
  mutate(
    has_shared_patio = str_detect(amenities_clean, regex("shared patio or balcony", ignore_case = TRUE)),
    has_elevator = str_detect(amenities_clean, regex("elevator", ignore_case = TRUE)),
    has_lock = str_detect(amenities_clean, regex("lock on bedroom door", ignore_case = TRUE)),
    has_hair_dryer = str_detect(amenities_clean, regex("hair dryer", ignore_case = TRUE)),
    has_priv_entrance = str_detect(amenities_clean, regex("private entrance", ignore_case = TRUE)),
    has_free_parking = str_detect(amenities_clean, regex("free street parking", ignore_case = TRUE))
  ) |> 
  select(
    everything(),
    -c(
      instant_bookable, 
      beds, 
      host_has_profile_pic, 	
      property_type, 	
      calculated_host_listings_count, 	
      host_about_length, 
      description_length, 	
      calculated_host_listings_count_entire_homes, 
      neighbourhood_cleansed, 
      maximum_nights_avg_ntm, 
      number_of_reviews_l30d,
      number_of_reviews,
      amenities_clean,
      matches("^amenity_"),
      -amenity_density,
      -amenities_count
    )
  )

chicago_test <- reg_test |> 
  filter(str_detect(tolower(listing_location), "chicago")) |> 
  mutate(
    reviews_weighted = reviews_per_month * review_scores_rating,
    amenity_density = amenities_count / (accommodates + 1)
  ) |>
  mutate(
    has_skyline = str_detect(amenities_clean, regex("city skyline view", ignore_case = TRUE)),
    has_elevator = str_detect(amenities_clean, regex("elevator", ignore_case = TRUE)),
    has_lock = str_detect(amenities_clean, regex("lock on bedroom door", ignore_case = TRUE)),
    has_dishwasher = str_detect(amenities_clean, regex("dishwasher", ignore_case = TRUE)),
    has_building_staff = str_detect(amenities_clean, regex("building staff", ignore_case = TRUE)),
    has_freezer = str_detect(amenities_clean, regex("freezer", ignore_case = TRUE))
  ) |> 
  select(
    everything(),
    -c(
      host_has_profile_pic, 
      missing_review_score, 
      has_description, 
      host_location, 	
      has_host_about, 
      host_many_listings, 	
      is_single_property_host, 	
      host_verifications, 
      host_identity_verified, 
      maximum_maximum_nights, 
      instant_bookable,
      amenities_clean,
      matches("^amenity_"),
      -amenity_density,
      -amenities_count
    )
  )

kauai_test <- reg_test |> 
  filter(str_detect(tolower(listing_location), "kauai")) |> 
  mutate(
    reviews_weighted = reviews_per_month * review_scores_rating,
    amenity_density = amenities_count / (accommodates + 1)
  ) |>
  mutate(
    has_single_level = str_detect(amenities_clean, regex("single level home", ignore_case = TRUE)),
    has_dishes = str_detect(amenities_clean, regex("has dishes and silverware", ignore_case = TRUE)),
    has_waterfront = str_detect(amenities_clean, regex("waterfront", ignore_case = TRUE)),
    has_dishwasher = str_detect(amenities_clean, regex("dishwasher", ignore_case = TRUE)),
    has_first_aid = str_detect(amenities_clean, regex("first aid kit", ignore_case = TRUE))
  ) |> 
  select(
    everything(),
    -c(
      has_description, 
      missing_review_score, 
      room_type, 
      host_response_time, 
      host_has_profile_pic,
      host_many_listings, 
      is_single_property_host, 
      instant_bookable, 	
      host_verifications, 
      host_response_rate, 
      has_host_about,
      amenities_clean,
      matches("^amenity_"),
      -amenity_density,
      -amenities_count
    )
  )

# create separate folds for each location ----
set.seed(58493756)

asheville_folds <- asheville_train |> 
  vfold_cv(v = 6, repeats = 4, strata = price)

chicago_folds <- chicago_train |> 
  vfold_cv(v = 6, repeats = 4, strata = price)

kauai_folds <- kauai_train |> 
  vfold_cv(v = 6, repeats = 4, strata = price)

# my metrics
my_metrics <- metric_set(mae)

# save out
save(my_metrics, file = here("14_attempt/data_split/my_metrics.rda"))

save(asheville_train, file = here("14_attempt/data_split/asheville_train.rda"))
save(chicago_train, file = here("14_attempt/data_split/chicago_train.rda"))
save(kauai_train, file = here("14_attempt/data_split/kauai_train.rda"))

save(asheville_test, file = here("14_attempt/data_split/asheville_test.rda"))
save(chicago_test, file = here("14_attempt/data_split/chicago_test.rda"))
save(kauai_test, file = here("14_attempt/data_split/kauai_test.rda"))

save(asheville_folds, file = here("14_attempt/data_split/asheville_folds.rda"))
save(chicago_folds, file = here("14_attempt/data_split/chicago_folds.rda"))
save(kauai_folds, file = here("14_attempt/data_split/kauai_folds.rda"))
