# Regression AirBnB Problem ----
# Helper functions

# load package(s)
library(tidyverse)
library(stringr)
library(textstem)
library(tidytext)
library(textdata)

# extract number of bathrooms from text ----
extract_bathroom_number <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}

# replace low frequency neighborhoods ----
replace_low_freq_neighborhoods <- function(df, min_count = 7) {
  counts <- table(df$neighbourhood_cleansed)
  low_freq <- names(counts[counts < min_count])
  df$neighbourhood_cleansed <- ifelse(df$neighbourhood_cleansed %in% low_freq, 
                                      "Others", 
                                      df$neighbourhood_cleansed)
  return(df)
}

# categorize neighborhoods by affluence ----
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

# categorize neighborhoods by geography ----
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

# encode property type ----
encode_property_type <- function(df) {
  df$property_type_encoded <- ifelse(grepl("room", df$property_type, ignore.case = TRUE), 
                                     "room", "More than a room")
  return(df)
}

# NLP ----
# parse JSON amenities properly ----
parse_json_amenities <- function(amenities_text) {
  if (is.na(amenities_text) || amenities_text == "" || amenities_text == "[]") {
    return(character(0))
  }
  
  tryCatch({

    amenities_text %>%
      str_remove_all("^\\[|\\]$") %>% 
      str_split(",") %>%
      unlist() %>%
      str_trim() %>%  
      str_remove_all('^"|"$') %>%  
      str_to_lower() %>% 
      .[. != ""]  
  }, error = function(e) {
    character(0)
  })
}

# add new columns ----
add_amenities_features <- function(data, amenities_col = "amenities") {

  amenities_parsed <- map(data[[amenities_col]], parse_json_amenities)
  
  amenities_count <- map_int(amenities_parsed, length)
  
  amenities_clean <- map_chr(amenities_parsed, ~ paste(.x, collapse = " | "))
  
  data %>%
    mutate(
      amenities_clean = amenities_clean,
      amenities_count = amenities_count,
      amenities_parsed = amenities_parsed
    )
}

# get frequent amenities ----
add_frequent_amenities_features <- function(df, threshold = 250) {
  
  if (!"amenities_parsed" %in% names(df)) {
    df <- add_amenities_features(df)
  }
  
  all_amenities <- df %>%
    filter(map_lgl(amenities_parsed, ~ length(.x) > 0)) %>%
    unnest(amenities_parsed) %>%
    count(amenities_parsed, sort = TRUE, name = "amenity_count")
  
  frequent_amenities <- all_amenities %>%
    filter(amenity_count >= threshold) %>%
    pull(amenities_parsed)
  
  amenity_col_names <- paste0("amenity_", make.names(frequent_amenities))
  amenity_col_names <- make.unique(amenity_col_names)
  
  df %>%
    rowwise() %>%
    mutate(
      amenity_flags = list({
        current_amenities <- amenities_parsed
        flags <- map_lgl(frequent_amenities, ~ .x %in% current_amenities)
        set_names(as.integer(flags), amenity_col_names)
      })
    ) %>%
    unnest_wider(amenity_flags) %>%
    ungroup() %>%
    select(-amenities_parsed)  
}

# simple process of description and host about ----
add_text_features <- function(df) {
  df %>%
    mutate(
      description_clean = tolower(description) |> str_replace_all("[^a-z\\s]", "") |> str_squish(),
      host_about_clean = tolower(host_about) |> str_replace_all("[^a-z\\s]", "") |> str_squish(),
      description_word_count = str_count(description_clean, "\\w+"),
      host_about_word_count = str_count(host_about_clean, "\\w+"),
      description_char_count = nchar(description),
      host_about_char_count = nchar(host_about)
    )
}

# simple process of sentiment features ----
add_sentiment_features <- function(df) {
  afinn <- get_sentiments("afinn")
  
  get_sentiment <- function(text) {
    if (is.na(text) || text == "") return(0)
    
    tibble(text = text) %>%
      unnest_tokens(word, text) %>%
      inner_join(afinn, by = "word") %>%
      summarise(sentiment = sum(value, na.rm = TRUE)) %>%
      pull(sentiment)
  }
  
  df %>%
    rowwise() %>%
    mutate(
      description_sentiment = get_sentiment(description_clean),
      host_about_sentiment = get_sentiment(host_about_clean)
    ) %>%
    ungroup()
}

# description categories ----
# detect presence of any keyword
detect_keywords <- function(text, keywords) {
  pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")
  str_detect(tolower(text), pattern)
}

# add category features function
add_description_categories <- function(df) {
  df %>%
    mutate(
      desc_location = detect_keywords(description, c("downtown", "downtown chicago", "wrigley field", "ideally located", "dining", "grocery store")),
      desc_transport = detect_keywords(description, c("train", "trains", "bus stop", "public transit", "public transportation", "metra", "cta", "airport", "line", 
                                                      "transportation", "walking distance", "walk", "convenience", "train station")),
      desc_views = detect_keywords(description, c("views", "view", "skyscraper", "lake", "lake michigan", "water views")),
      desc_amenities = detect_keywords(description, c("fully-equipped", "jacuzzi", "sauna", "pool table", "patio", "basement", "garden", "rooftop deck", "balcony", "balconies", "deck")),
      desc_family_pets = detect_keywords(description, c("children", "child", "pet", "dog", "cat", "family")),
      desc_discount_price = detect_keywords(description, c("discount", "discounts", "money saving", "price tag", "value")),
      desc_extra_fees = detect_keywords(description, c("fee", "fees")),
      desc_style_vibe = detect_keywords(description, c("luxury", "trendy", "renovated", "quiet", "updated", "stylish", "artsy", "popular", "historical", 
                                                       "comfortable", "peaceful", "cozy", "spacious")),
      desc_parking = detect_keywords(description, c("parking", "free parking", "designated parking", "parking garage"))
    )
}

# process amenities complex ----
process_airbnb_amenities <- function(data, 
                                     amenities_col = "amenities",
                                     extract_binary = TRUE,
                                     create_tfidf = TRUE,
                                     top_n_features = 50) {
  
  processed_data <- data
  
  # clean the amenities field
  processed_data <- processed_data |> 
    mutate(
      amenities_clean = sapply(!!sym(amenities_col), function(x) {
        if(is.na(x) || x == "") return("")
        
        # remove JSON array syntax and quotes
        clean_text <- gsub('\\["', '', x)
        clean_text <- gsub('"\\]', '', clean_text)
        clean_text <- gsub('\\"', '', clean_text)
        # split by commas
        amenities_vector <- strsplit(clean_text, '", "')[[1]]
        # return as a single string with spaces
        paste(amenities_vector, collapse = " ")
      })
    )
  
  # count total amenities
  processed_data <- processed_data |> 
    mutate(amenities_count = str_count(amenities_clean, "\\w+"))
  
  # extract binary features for common amenities
  if(extract_binary) {
    amenity_patterns <- list(
      has_wifi = "wifi|internet|broadband",
      has_kitchen = "kitchen|kitchenette",
      has_ac = "air condition|ac|a/c|cooling",
      has_heating = "heating|heater",
      has_washer = "washer|laundry|washing machine",
      has_dryer = "dryer",
      has_parking = "parking|garage",
      has_pool = "pool|swimming",
      has_tv = "tv|television",
      has_workspace = "workspace|desk|office|work",
      has_dishwasher = "dishwasher",
      has_microwave = "microwave",
      has_coffee = "coffee",
      has_refrigerator = "refrigerator|fridge",
      has_oven = "oven",
      has_stove = "stove",
      has_balcony = "balcony|patio|terrace",
      has_gym = "gym|fitness",
      has_elevator = "elevator|lift",
      has_hair_dryer = "hair dryer|hairdryer",
      has_iron = "iron",
      has_essentials = "essentials",
      has_free_parking = "free parking",
      has_dedicated_workspace = "dedicated workspace",
      has_self_checkin = "self check|self-check|keypad|lockbox",
      has_outdoor_space = "backyard|garden|yard|outdoor|patio",
      has_longterm_stays = "long term|longterm",
      has_high_speed_wifi = "fast wifi|high-speed|high speed"
    )
    
    # create binary features for each amenity pattern
    for (amenity_name in names(amenity_patterns)) {
      pattern <- amenity_patterns[[amenity_name]]
      processed_data[[amenity_name]] <- as.integer(grepl(pattern, tolower(processed_data$amenities_clean)))
    }
  }
  
  # advanced TF-IDF features
  
  if(create_tfidf) {
    # process text function
    process_text <- function(text_column) {
      text_df <- tibble::tibble(text = text_column) |> 
        mutate(id = row_number())
      
      # tokenize
      tokens <- text_df |>
        unnest_tokens(word, text) |>
        # Remove stop words
        anti_join(stop_words, by = "word") |>
        # Remove numbers
        filter(!str_detect(word, "^[0-9]+$")) |>
        # Lemmatize
        mutate(word = lemmatize_words(word))
      
      return(tokens)
    }
    
    # process amenities tokens
    amenities_tokens <- process_text(processed_data$amenities_clean)
    
    # create TF-IDF features for amenities
    amenities_tfidf <- amenities_tokens |>
      count(id, word) |>
      bind_tf_idf(word, id, n) |>
      arrange(desc(tf_idf))
    
    # get top words by TF-IDF for amenities
    top_amen_words <- amenities_tfidf |>
      group_by(word) |>
      summarize(total_tfidf = sum(tf_idf)) |>
      top_n(top_n_features, total_tfidf) |>
      pull(word)
    
    # create document-term matrix for top amenity words
    amen_dtm <- amenities_tfidf |>
      filter(word %in% top_amen_words) |>
      select(id, word, tf_idf) |>
      pivot_wider(names_from = word, 
                  values_from = tf_idf, 
                  values_fill = 0,
                  names_prefix = "amen_")
    
    # join TF-IDF features with the processed data
    if(nrow(amen_dtm) > 0) {
      # create row ID to ensure correct join
      processed_data$row_id <- 1:nrow(processed_data)
      amen_dtm$row_id <- amen_dtm$id
      
      # join the features
      processed_data <- processed_data |>
        left_join(amen_dtm |> select(-id), by = "row_id") |>
        select(-row_id)  # Remove the temporary ID column
      
      # replace NA values with 0 for the new columns
      na_cols <- grep("^amen_", names(processed_data), value = TRUE)
      processed_data[na_cols][is.na(processed_data[na_cols])] <- 0
    }
  }
  
  # return the processed data
  return(processed_data)
}

# process description complex ----
process_airbnb_description <- function(data, 
                                       description_col = "description",
                                       include_sentiment = TRUE,
                                       create_tfidf = TRUE,
                                       extract_keywords = TRUE,
                                       top_n_features = 100) {
  
  # make a copy of the input data
  processed_data <- data
  
  # clean the description field
  
  # handle missing descriptions
  processed_data[[description_col]][is.na(processed_data[[description_col]])] <- ""
  
  # extract basic features
  
  # create simple text metrics
  processed_data <- processed_data |>
    mutate(
      description_length = nchar(!!sym(description_col)),
      description_word_count = str_count(!!sym(description_col), "\\w+"),
      description_sentence_count = str_count(!!sym(description_col), "[.!?]+"),
      description_caps_ratio = str_count(!!sym(description_col), "[A-Z]") / 
        (str_count(!!sym(description_col), "[a-zA-Z]") + 0.001),
      description_exclamation_count = str_count(!!sym(description_col), "!"),
      description_question_count = str_count(!!sym(description_col), "\\?")
    )
  
  # extract common keywords if requested
  if(extract_keywords) {
    # define important keywords to extract
    keyword_patterns <- list(
      mentions_location = "location|located|close to|near|nearby|walking distance",
      mentions_view = "view|overlook|scene|scenic|panoramic",
      mentions_new = "new|brand new|newly|recent|renovated|remodeled",
      mentions_modern = "modern|contemporary|updated|stylish",
      mentions_cozy = "cozy|comfortable|comfy|snug|relax",
      mentions_luxury = "luxury|luxurious|elegant|upscale|premium",
      mentions_clean = "clean|spotless|tidy|immaculate",
      mentions_spacious = "spacious|roomy|large|big|huge",
      mentions_quiet = "quiet|peaceful|serene|tranquil",
      mentions_safe = "safe|secure|security|gated",
      mentions_family = "family|kid|child|children|baby",
      mentions_business = "business|work|office|corporate|professional",
      mentions_transport = "transport|subway|metro|bus|train|airport",
      mentions_restaurants = "restaurant|dining|cafe|food|eat",
      mentions_shopping = "shop|shopping|store|mall|boutique",
      mentions_beach = "beach|ocean|sea|coast|shore",
      mentions_downtown = "downtown|city center|heart of the city"
    )
    
    # create binary features for each keyword pattern
    for (keyword_name in names(keyword_patterns)) {
      pattern <- keyword_patterns[[keyword_name]]
      processed_data[[keyword_name]] <- as.integer(grepl(pattern, 
                                                         tolower(processed_data[[description_col]])))
    }
  }
  
  # process text for advanced features
  
  # tokenize and process function
  process_text <- function(text_column) {
    text_df <- tibble::tibble(text = text_column) |>
      mutate(id = row_number())
    
    # tokenize
    tokens <- text_df |>
      unnest_tokens(word, text) |>
      # Remove stop words
      anti_join(stop_words, by = "word") |>
      # Remove numbers
      filter(!str_detect(word, "^[0-9]+$")) |>
      # Lemmatize
      mutate(word = lemmatize_words(word))
    
    return(tokens)
  }
  
  # process description
  description_tokens <- process_text(processed_data[[description_col]])
  
  # sentiment analysis
  
  if(include_sentiment) {
    # get sentiment scores for description
    desc_sentiment <- description_tokens |>
      inner_join(get_sentiments("bing"), by = "word") |>
      count(id, sentiment) |>
      pivot_wider(names_from = sentiment, 
                  values_from = n, 
                  values_fill = 0) |>
      mutate(
        desc_sentiment_score = positive - negative,
        desc_sentiment_ratio = ifelse(negative == 0, positive, positive / negative)
      )
    
    # handle cases where a listing has no sentiment words
    missing_ids <- setdiff(1:nrow(processed_data), desc_sentiment$id)
    if(length(missing_ids) > 0) {
      missing_df <- tibble(
        id = missing_ids,
        positive = 0,
        negative = 0,
        desc_sentiment_score = 0,
        desc_sentiment_ratio = 1  # neutral ratio
      )
      desc_sentiment <- bind_rows(desc_sentiment, missing_df)
    }
    
    # sort by ID to match the original data
    desc_sentiment <- desc_sentiment |> arrange(id)
    
    # create row ID to ensure correct join
    processed_data$row_id <- 1:nrow(processed_data)
    desc_sentiment$row_id <- desc_sentiment$id
    
    # join the sentiment features
    processed_data <- processed_data |>
      left_join(desc_sentiment |> select(-id), by = "row_id")
    
    # replace NA values with 0/1 for the new columns
    if("positive" %in% names(processed_data) && "negative" %in% names(processed_data)) {
      processed_data$positive[is.na(processed_data$positive)] <- 0
      processed_data$negative[is.na(processed_data$negative)] <- 0
      processed_data$desc_sentiment_score[is.na(processed_data$desc_sentiment_score)] <- 0
      processed_data$desc_sentiment_ratio[is.na(processed_data$desc_sentiment_ratio)] <- 1
    }
  }
  
  # TF-IDF features
  
  if(create_tfidf) {
    # create TF-IDF features for description
    description_tfidf <- description_tokens |>
      count(id, word) |>
      bind_tf_idf(word, id, n) |>
      arrange(desc(tf_idf))
    
    # get top words by TF-IDF for descriptions
    top_desc_words <- description_tfidf |>
      group_by(word) |>
      summarize(total_tfidf = sum(tf_idf)) |>
      top_n(top_n_features, total_tfidf) |>
      pull(word)
    
    # create document-term matrix for top description words
    desc_dtm <- description_tfidf |>
      filter(word %in% top_desc_words) |>
      select(id, word, tf_idf) |>
      pivot_wider(names_from = word, 
                  values_from = tf_idf, 
                  values_fill = 0,
                  names_prefix = "desc_")
    
    # join TF-IDF features with the processed data
    if(nrow(desc_dtm) > 0) {
      # row ID should already exist from sentiment analysis
      if(!"row_id" %in% names(processed_data)) {
        processed_data$row_id <- 1:nrow(processed_data)
      }
      desc_dtm$row_id <- desc_dtm$id
      
      # join the features
      processed_data <- processed_data |>
        left_join(desc_dtm |> select(-id), by = "row_id")
      
      # replace NA values with 0 for the new columns
      na_cols <- grep("^desc_", names(processed_data), value = TRUE)
      processed_data[na_cols][is.na(processed_data[na_cols])] <- 0
    }
  }
  
  # remove the temporary ID column
  if("row_id" %in% names(processed_data)) {
    processed_data <- processed_data |> select(-row_id)
  }
  
  # return the processed data
  return(processed_data)
}
