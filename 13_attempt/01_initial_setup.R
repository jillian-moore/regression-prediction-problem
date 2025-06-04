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

# PRE_PROCESSING ----
# training data
reg_train <- read.csv(here("data/train.csv")) |>
  clean_names() |> 
  mutate(
    # make id a character object
    id = as.character(id),
    # parse price
    price = parse_number(price),
    price = if_else(price > 2500, 2500, price),
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
    host_tenure_days = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    first_review = ymd(first_review),
    # has reviews
    has_reviews = !is.na(first_review),
    # checking amenities
    has_wifi = str_detect(amenities, regex("wifi", ignore_case = TRUE)),
    has_kitchen = str_detect(amenities, regex("kitchen", ignore_case = TRUE)),
    amenity_count = if_else(is.na(amenities), 0L, str_count(amenities, ",") + 1),
    # missing info
    missing_response_rate = is.na(host_response_rate),
    missing_acceptance_rate = is.na(host_acceptance_rate),
    missing_num_bathrooms = is.na(num_bathrooms),
    missing_review_score = is.na(review_scores_rating),
    missing_reviews_per_month = is.na(reviews_per_month),
    reviews_per_month = if_else(is.na(reviews_per_month), 0, reviews_per_month),
    review_scores_rating = if_else(is.na(review_scores_rating), median(review_scores_rating, na.rm = TRUE), review_scores_rating)
  ) |>
  select(-bathrooms_text, -amenities, -description, -host_about, -host_since, -first_review, -last_review)

char_to_factor <- reg_train |>
  select(where(is.character)) |>
  select(-id) |>
  names()

reg_train <-reg_train |>
  mutate(across(all_of(char_to_factor), factor))

# testing data
reg_test <- read.csv(here("data/test.csv")) |>
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
    host_tenure_days = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    first_review = ymd(first_review),
    # has reviews
    has_reviews = !is.na(first_review),
    # checking amenities
    has_wifi = str_detect(amenities, regex("wifi", ignore_case = TRUE)),
    has_kitchen = str_detect(amenities, regex("kitchen", ignore_case = TRUE)),
    amenity_count = if_else(is.na(amenities), 0L, str_count(amenities, ",") + 1),
    # missing info
    missing_response_rate = is.na(host_response_rate),
    missing_acceptance_rate = is.na(host_acceptance_rate),
    missing_num_bathrooms = is.na(num_bathrooms),
    missing_review_score = is.na(review_scores_rating),
    missing_reviews_per_month = is.na(reviews_per_month),
    reviews_per_month = if_else(is.na(reviews_per_month), 0, reviews_per_month),
    review_scores_rating = if_else(is.na(review_scores_rating), median(review_scores_rating, na.rm = TRUE), review_scores_rating)
  ) |>
  select(-bathrooms_text, -amenities, -description, -host_about, -host_since, -first_review, -last_review)

char_to_factor <- reg_test |>
  select(where(is.character)) |>
  select(-id) |>
  names()

reg_test <-reg_test |>
  mutate(across(all_of(char_to_factor), factor))

# split by listing location ----
# training data
asheville_train <- reg_train |> 
  filter(str_detect(tolower(listing_location), "asheville"))

chicago_train <- reg_train |> 
  filter(str_detect(tolower(listing_location), "chicago"))

kauai_train <- reg_train |> 
  filter(str_detect(tolower(listing_location), "kauai"))

# testing data
asheville_test <- reg_test |> 
  filter(str_detect(tolower(listing_location), "asheville"))

chicago_test <- reg_test |> 
  filter(str_detect(tolower(listing_location), "chicago"))

kauai_test <- reg_test |> 
  filter(str_detect(tolower(listing_location), "kauai"))

# create separate folds for each location ----
set.seed(58493756)

asheville_folds <- asheville_data |> 
  vfold_cv(v = 6, repeats = 4, strata = price)

chicago_folds <- chicago_data |> 
  vfold_cv(v = 6, repeats = 4, strata = price)

kauai_folds <- kauai_data |> 
  vfold_cv(v = 6, repeats = 4, strata = price)

# my metrics
my_metrics <- metric_set(mae)

# save out
save(my_metrics, file = here("13_attempt/data_split/my_metrics.rda"))

save(asheville_train, file = here("13_attempt/data_split/asheville_train.rda"))
save(chicago_train, file = here("13_attempt/data_split/chicago_train.rda"))
save(kauai_train, file = here("13_attempt/data_split/kauai_train.rda"))

save(asheville_test, file = here("13_attempt/data_split/asheville_test.rda"))
save(chicago_test, file = here("13_attempt/data_split/chicago_test.rda"))
save(kauai_test, file = here("13_attempt/data_split/kauai_test.rda"))

save(asheville_folds, file = here("13_attempt/data_split/asheville_folds.rda"))
save(chicago_folds, file = here("13_attempt/data_split/chicago_folds.rda"))
save(kauai_folds, file = here("13_attempt/data_split/kauai_folds.rda"))
