# Regression AirBnB Problem ----
# EDA of key variables

# load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load data ----
reg_train <- read.csv(here("data/train.csv"))
reg_test <- read.csv(here("data/test.csv"))

# function to look at density ----
plot_all_numeric_densities <- function(df) {

  numeric_cols <- df %>% select(where(is.numeric)) %>% names()
  
  plots <- map(numeric_cols, function(col) {
    ggplot(df, aes(x = .data[[col]])) +
      geom_density() 
  })
  
  names(plots) <- numeric_cols
  return(plots)
}

# call the function
density_plots <- plot_all_numeric_densities(reg_train)

# by column ----
# extreme host_total_listings_count skew towards one
density_plots$host_total_listings_count
# extreme host_listings_count seems to be the same thing as host_total_listings_count but smaller scale
density_plots$host_listings_count
# trails to zero around 10 bedrooms, most have 1-5 bedrooms
density_plots$bedrooms
# trails to zero around 15 beds, most have 1-10
density_plots$beds
# trails to zero around 25, huge skew towards stays a few days long, bump around 2 weeks
density_plots$minimum_nights
# actually okay
density_plots$maximum_nights
# trails to zero around 25, huge skew towards stays a few days long, bump around 2 weeks
density_plots$minimum_minimum_nights
# trails to zero around 25, huge skew towards stays a few days long, bump around 2 weeks
density_plots$maximum_minimum_nights
# okay this is like weird decimal numbers, would remove
density_plots$minimum_maximum_nights
# okay this is like weird decimal numbers, would remove
density_plots$maximum_maximum_nights
# trails to zero around 25, huge skew towards stays a few days long, bump around 2 weeks
density_plots$minimum_nights_avg_ntm
# okay this is like weird decimal numbers, would remove
density_plots$maximum_nights_avg_ntm
# valley around 15, most available very soon or closer to 30 days away
density_plots$availability_30
# valley around 25, most available very soon or closer to 55 days away
density_plots$availability_60
# valley around 15, most available very soon with steady increase towards 75
density_plots$availability_90
# more variable, valley around 110 with steady increase towards 350
density_plots$availability_365
# skew left, reaches 0 around 270
density_plots$number_of_reviews
# skew left, reaches 0 around 100
density_plots$number_of_reviews_ltm
# more variable, reaches 0 around 15
density_plots$number_of_reviews_l30d
# huge right skew towards 4+
density_plots$review_scores_rating
# huge skew towards 4+
density_plots$review_scores_accuracy
# huge skew towards 4+
density_plots$review_scores_cleanliness
# huge skew towards 4+
density_plots$review_scores_checkin
# huge skew towards 4+
density_plots$review_scores_communication
# huge skew towards 4+
density_plots$review_scores_location
# still skew towards right but a bit more variable, small spike at 4
density_plots$review_scores_value
# huge skew left random small spikes until almost 450
density_plots$calculated_host_listings_count
# huge skew left random small spikes until almost 450
density_plots$calculated_host_listings_count_entire_homes
# huge skew left, fewer small spikes tho
density_plots$calculated_host_listings_count_private_rooms
# weird skew, would remove or make binary
density_plots$calculated_host_listings_count_shared_rooms
# huge skew left, reachoes 0 around 10
density_plots$reviews_per_month

