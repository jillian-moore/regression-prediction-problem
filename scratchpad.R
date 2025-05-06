load(here("data_split/train.rda"))
     
# Convert boolean values to numeric
train$instant_bookable <- ifelse(train$instant_bookable == "t", 1, 0)

# Remove percentage signs and convert to numeric
train$host_response_rate <- as.numeric(gsub("%", "", train$host_response_rate))
train$host_acceptance_rate <- as.numeric(gsub("%", "", train$host_acceptance_rate))

# Impute missing values with median
impute_median <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

numeric_cols <- c("review_scores_rating", "review_scores_accuracy", 
                  "review_scores_cleanliness", "review_scores_checkin", 
                  "review_scores_communication", "review_scores_location", 
                  "review_scores_value", "beds", "reviews_per_month",
                  "host_response_rate", "host_acceptance_rate")


for (col in numeric_cols) {
  train[[col]] <- impute_median(train[[col]])
}

# Calculate hosting length from host_since date
reference_date <- as.Date("2022-12-31")
train$host_since <- as.Date(train$host_since)
train$hosting_length <- as.numeric(reference_date - train$host_since)


# Create dummy variables for room_type
train <- train %>%
  mutate(
    room_type_Entire_home = ifelse(room_type == "Entire home/apt", 1, 0),
    room_type_Hotel_room = ifelse(room_type == "Hotel room", 1, 0),
    room_type_Shared_room = ifelse(room_type == "Shared room", 1, 0),
    room_type_Private_room = ifelse(room_type == "Private room", 1, 0)
  )

# Extract number of bathrooms from bathrooms_text
extract_bathroom_number <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}

train$number_of_bathrooms <- extract_bathroom_number(train$bathrooms_text)
train$number_of_bathrooms <- impute_median(train$number_of_bathrooms)

# Log-transform number of reviews
train$log_number_of_reviews <- log(train$number_of_reviews + 1)


replace_low_freq_neighborhoods <- function(df, min_count = 7) {
  counts <- table(df$neighbourhood_cleansed)
  low_freq <- names(counts[counts < min_count])
  df$neighbourhood_cleansed <- ifelse(df$neighbourhood_cleansed %in% low_freq, 
                                      "Others", 
                                      df$neighbourhood_cleansed)
  return(df)
}

train <- replace_low_freq_neighborhoods(train)

train$property_type_encoded <- ifelse(grepl("room", train$property_type, ignore.case = TRUE), 
                                      "room", "More than a room")

# Categorize neighborhoods by affluence and geography
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

train <- categorize_neighborhoods_by_affluence(train)
train <- categorize_neighborhoods_by_geography(train)

# Create dummy variables for categorical features
categorical_features <- c('host_response_time', 'host_verifications', 
                          'host_has_profile_pic', 'host_identity_verified', 
                          'has_availability', 'instant_bookable', 
                          'property_type_encoded', 'host_neighbourhood_affluence',
                          'host_neighbourhood_geo', 'neighbourhood_affluence', 
                          'neighbourhood_geo')

# Function to create dummy variables
create_dummies <- function(data, cat_vars) {
  result <- data
  for (var in cat_vars) {
    dummies <- model.matrix(~ 0 + factor(data[[var]]))
    colnames(dummies) <- paste0(var, "_", gsub(" ", "_", colnames(dummies)))
    result <- cbind(result, dummies)
  }
  return(result)
}

train_with_dummies <- create_dummies(train, categorical_features)

# Drop columns that are no longer needed
cols_to_drop <- c('property_type', 'host_neighbourhood', 'neighbourhood_cleansed', 
                  'number_of_reviews', 'first_review', 'last_review', 
                  'bathrooms_text', 'host_since', 'host_location', 'host_id', 
                  'id', 'host_response_time', 'host_verifications', 
                  'host_has_profile_pic', 'host_identity_verified', 
                  'has_availability', 'property_type_encoded', 
                  'host_neighbourhood_affluence', 'host_neighbourhood_geo', 
                  'neighbourhood_affluence', 'neighbourhood_geo', 'room_type')

train_clean <- train_with_dummies %>% select(-one_of(cols_to_drop))

# Ensure same columns in train and test (except target variable)
target_col <- "price"  # Assuming 'price' is the target variable for regression
predictors <- setdiff(names(train_clean), target_col)

# Finalize datasets
X_train <- train_clean[, predictors]
y_train <- train_clean[[target_col]]
