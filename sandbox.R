# quick peek at data

# load packages
library(tidyverse)
library(here)

# notes for future
# look into random forest for ensemble, on own

# load data
reg_train <- read_csv(here("data/train.csv"))

reg_train |> 
  skimr::skim_without_charts()

reg_train |> 
  mutate(price = parse_number(price)) |> 
  count(price)

# only keep amenities in over 250 airbnbs

# to factor: listing_location, host_since, host_location, host_response_time, host_neighbourhood, host_verifications,
# host_has_profile_pic, host_identity_verified, neighbourhood_cleansed, property_type, room_type, bathrooms_text
# has_availability, first_review, last_review, instant_bookable

# to num: host_response_rate, host_acceptance_rate
# perm cat: description, id, host_about, amenities

# check for outliers
summary_stats <- reg_train |>
  select(where(is.numeric)) |>
  summarise(across(everything(), list(median = ~median(.x, na.rm = TRUE),
                                      max = ~max(.x, na.rm = TRUE)))) |>
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(median|max)$"
  ) |>
  pivot_wider(names_from = stat, values_from = value)

summary_stats <- summary_stats |>
  mutate(across(c(median, max), ~round(.x, 0)))

# check price for outliers
reg_train <- read.csv(here("data/train.csv"))

reg_train <- reg_train |> 
  mutate(price = as.numeric(parse_number(price)))

# identify outliers in price using IQR method
Q1 <- quantile(reg_train$price, 0.25, na.rm = TRUE)
Q3 <- quantile(reg_train$price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - (5 * IQR)
upper_bound <- Q3 + (5 * IQR)

# filter out rows with extreme price outliers
reg_train_clean <- reg_train |> 
  filter(price <= 50)

reg_train_clean$price
summary(reg_train$price)
