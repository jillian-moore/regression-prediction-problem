# Load packages
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(stringr)
library(lubridate)

tidymodels_prefer()

# Load and clean data ----
reg_train <- read_csv(here("data/train.csv")) |>
  clean_names()

library(tidyverse)
library(lubridate)

airbnb_train <- reg_train |>
  mutate(
    id = as.character(id),
    price = parse_number(price),
    price = if_else(price > 2500, 2500, price),
    price_log10 = log10(price),
    host_response_rate = na_if(host_response_rate, "N/A") |> str_remove("%") |> as.numeric() / 100,
    host_acceptance_rate = na_if(host_acceptance_rate, "N/A") |> str_remove("%") |> as.numeric() / 100,
    bathrooms_text = str_replace_all(bathrooms_text, regex("half[- ]?bath", ignore_case = TRUE), "0.5"),
    num_bathrooms = suppressWarnings(as.numeric(str_remove_all(bathrooms_text, "[A-Za-z ]"))),
    num_bathrooms = if_else(num_bathrooms > 5, 5, num_bathrooms),
    description_length = if_else(is.na(description), 0L, nchar(description)),
    host_about_length = if_else(is.na(host_about), 0L, nchar(host_about)),
    host_since = ymd(host_since),
    host_tenure_days = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    first_review = ymd(first_review),
    has_reviews = !is.na(first_review),
    has_wifi = str_detect(amenities, regex("wifi", ignore_case = TRUE)),
    has_kitchen = str_detect(amenities, regex("kitchen", ignore_case = TRUE)),
    amenity_count = if_else(is.na(amenities), 0L, str_count(amenities, ",") + 1),
    missing_response_rate = is.na(host_response_rate),
    missing_acceptance_rate = is.na(host_acceptance_rate),
    missing_num_bathrooms = is.na(num_bathrooms),
    missing_review_score = is.na(review_scores_rating),
    missing_reviews_per_month = is.na(reviews_per_month),
    host_response_rate = if_else(is.na(host_response_rate), median(host_response_rate, na.rm = TRUE), host_response_rate),
    host_acceptance_rate = if_else(is.na(host_acceptance_rate), median(host_acceptance_rate, na.rm = TRUE), host_acceptance_rate),
    num_bathrooms = if_else(is.na(num_bathrooms), median(num_bathrooms, na.rm = TRUE), num_bathrooms),
    reviews_per_month = if_else(is.na(reviews_per_month), 0, reviews_per_month),
    review_scores_rating = if_else(is.na(review_scores_rating), median(review_scores_rating, na.rm = TRUE), review_scores_rating)
  ) |>
  select(-price, -bathrooms_text, -amenities, -description, -host_about, -host_since, -first_review, -last_review)

char_to_factor <- airbnb_train |>
  select(where(is.character)) |>
  select(-id) |>
  names()

airbnb_train <- airbnb_train |>
  mutate(across(all_of(char_to_factor), factor))


# Cross-validation folds
set.seed(2310724)
airbnb_folds <- vfold_cv(airbnb_train, v = 6, repeats = 4, strata = price_log10)

# Metric
my_metrics <- metric_set(mae)

# Save outputs for Attempt 6
save(airbnb_train, file = here("attempt_10/data_split/airbnb_train.rda"))
save(airbnb_folds, file = here("attempt_10/data_split/airbnb_folds.rda"))
save(my_metrics, file = here("attempt_10/data_split/my_metrics.rda"))


