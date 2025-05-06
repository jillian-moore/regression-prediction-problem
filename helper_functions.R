# Regression AirBnB Problem ----
# Helper functions

# load package(s)
library(tidyverse)

# extract number of bathrooms from text
extract_bathroom_number <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}

# replace low frequency neighborhoods
replace_low_freq_neighborhoods <- function(df, min_count = 7) {
  counts <- table(df$neighbourhood_cleansed)
  low_freq <- names(counts[counts < min_count])
  df$neighbourhood_cleansed <- ifelse(df$neighbourhood_cleansed %in% low_freq, 
                                      "Others", 
                                      df$neighbourhood_cleansed)
  return(df)
}

# categorize neighborhoods by affluence
categorize_neighborhoods_by_affluence <- function(df) {
  affluent <- c("Gold Coast", "Streeterville", "Near North Side", "Lincoln Park", 
                "Lake View East", "West Loop", "South Loop", "Loop", "River North", 
                "Old Town", "Roscoe Village", "West Town", "Hyde Park", "The Loop", 
                "Rush & Division", "Sheffield & DePaul", "Wrigleyville", "Bucktown", 
                "Magnificent Mile", "Fulton River District", "Greektown", 
                "University Village", "Printer's Row")
  
  moderately_affluent <- c("Lakeview", "Bucktown", "Wicker Park", "West Ridge", 
                           "Portage Park", "North Center", "Andersonville", 
                           "Irving Park", "Uptown", "West Town", "Edgewater", 
                           "Ravenswood", "Ukrainian Village", "Little Italy",
                           "Lincoln Square", "Jefferson Park", "Avondale", 
                           "Old Irving Park", "North Park", "Sheridan Park", 
                           "Montclare", "Bridgeport", "Irving Park", 
                           "Logan Square", "Humboldt Park", "Pilsen")
  
  middle_income <- c("Pilsen", "Douglas", "South Shore", "Greater Grand Crossing", 
                     "West Lawn", "Hermosa", "Albany Park", "Brighton Park", 
                     "McKinley Park", "Washington Park", "Auburn Gresham", 
                     "Chicago Lawn", "Englewood", "Forest Glen", "Washington Heights", 
                     "Belmont Cragin", "Norwood Park", "Ashburn", "South Lawndale", 
                     "Heart of Italy", "East Side", "Lower West Side", 
                     "East Garfield Park", "West Englewood", "Tri-Taylor", 
                     "Garfield Ridge", "Clearwater Beach", "Logan Square", 
                     "Andersonville Glen")
  
  lower_income <- c("Back of the Yards", "Humboldt Park", "East Garfield Park", 
                    "Austin", "Riverdale", "Pullman", "Roseland", "Archer Heights", 
                    "Bridgeport", "West Woodlawn", "West Chatham", "Little Village", 
                    "West Pullman", "Gage Park", "Grand Boulevard", "New City", 
                    "Woodlawn", "South Chicago", "Cabrini-Green", "Belmont Central", 
                    "West Chesterfield", "O'Hare", "South Shore", "Stoney Island Park", 
                    "Garfield Park", "Calumet Heights", "Ashburn", "Pulaski Park", 
                    "Englewood", "East Albany Park", "Legends South", "West Lawn", 
                    "North Beach")
  
  df$host_neighbourhood_affluence <- case_when(
    df$host_neighbourhood %in% affluent ~ "Affluent",
    df$host_neighbourhood %in% moderately_affluent ~ "Moderately Affluent",
    df$host_neighbourhood %in% middle_income ~ "Middle-Income",
    df$host_neighbourhood %in% lower_income ~ "Lower-Income",
    TRUE ~ "Others"
  )
  
  df$neighbourhood_affluence <- case_when(
    df$neighbourhood_cleansed %in% affluent ~ "Affluent",
    df$neighbourhood_cleansed %in% moderately_affluent ~ "Moderately Affluent",
    df$neighbourhood_cleansed %in% middle_income ~ "Middle-Income",
    df$neighbourhood_cleansed %in% lower_income ~ "Lower-Income",
    TRUE ~ "Others"
  )
  
  return(df)
}

# categorize neighborhoods by geography
categorize_neighborhoods_by_geography <- function(df) {
  north_side <- c("Lakeview", "Bucktown", "Wicker Park", "West Ridge", "Portage Park", 
                  "North Center", "Andersonville", "Irving Park", "Uptown", 
                  "Edgewater", "Ravenswood", "Ukrainian Village", "Lincoln Square", 
                  "Jefferson Park", "Avondale", "Old Irving Park", "North Park", 
                  "Sheridan Park", "Montclare", "Albany Park", "Lincoln Park", 
                  "Rush & Division", "Sheffield & DePaul", "Wrigleyville", 
                  "Logan Square", "Andersonville Glen", "Rogers Park", 
                  "West Rogers Park", "Rogers Edge", "West Ridge", "North Edgebrook", 
                  "Nortown", "West Andersonville", "Bowmanville", "West DePaul", 
                  "Boystown", "Lincoln Square", "Kenwood", "North Lawndale", 
                  "Morgan Park", "East Garfield Park")
  
  south_side <- c("Hyde Park", "Pilsen", "Douglas", "South Shore", 
                  "Greater Grand Crossing", "West Lawn", "Hermosa", "Washington Park", 
                  "Auburn Gresham", "Chicago Lawn", "Englewood", "Forest Glen", 
                  "Washington Heights", "Belmont Cragin", "Norwood Park", "Ashburn", 
                  "South Lawndale", "Heart of Italy", "East Side", "Lower West Side", 
                  "East Garfield Park", "West Englewood", "Tri-Taylor", 
                  "Garfield Ridge", "Clearwater Beach", "South Chicago", 
                  "Back of the Yards", "Pullman", "Roseland", "Archer Heights", 
                  "West Woodlawn", "West Chatham", "Little Village", "West Pullman", 
                  "Gage Park", "Grand Boulevard", "New City", "Woodlawn", 
                  "Cabrini-Green", "Belmont Central", "West Chesterfield", 
                  "South Shore", "Stoney Island Park", "Garfield Park", 
                  "Calumet Heights", "Ashburn", "Pulaski Park", "Englewood", 
                  "East Albany Park", "Legends South", "West Lawn", "North Beach", 
                  "Bridgeport", "Bronzeville", "Near South Side", "South Commons", 
                  "Dearborn Park", "Printing House Row", "Armour Square", 
                  "Hegewisch", "Chatham", "Pullman", "Washington Heights")
  
  central <- c("Gold Coast", "Streeterville", "Near North Side", "Lake View East", 
               "West Loop", "South Loop", "Loop", "River North", "Old Town", 
               "Roscoe Village", "West Town", "The Loop", "Grand Crossing", 
               "New Eastside", "Magnificent Mile", "Michigan Avenue", "Goose Island", 
               "River West", "Dearborn Homes", "West Garfield Park", "East Village", 
               "Greektown", "Printer's Row", "Fulton River District")
  
  df$host_neighbourhood_geo <- case_when(
    df$host_neighbourhood %in% north_side ~ "North",
    df$host_neighbourhood %in% south_side ~ "South",
    df$host_neighbourhood %in% central ~ "Central",
    TRUE ~ "Others"
  )
  
  df$neighbourhood_geo <- case_when(
    df$neighbourhood_cleansed %in% north_side ~ "North",
    df$neighbourhood_cleansed %in% south_side ~ "South",
    df$neighbourhood_cleansed %in% central ~ "Central",
    TRUE ~ "Others"
  )
  
  return(df)
}

# encode property type
encode_property_type <- function(df) {
  df$property_type_encoded <- ifelse(grepl("room", df$property_type, ignore.case = TRUE), 
                                     "room", "More than a room")
  return(df)
}

# calculate hosting length
calculate_hosting_length <- function(df, reference_date = "2025-05-01") {
  ref_date <- as.Date(reference_date)
  df$host_since <- as.Date(df$host_since)
  df$hosting_length <- as.numeric(ref_date - df$host_since)
  return(df)
}