# Regression AirBnB Problem ----
# Tuning for CatBoost model
# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(rlang)
library(future)
library(catboost)

# Get our custom CatBoost integration
source(here("10_attempt/catboost_model.R"))
load(here("data_split/reg_train.rda"))
debug_catboost_call()
debug_cv_issue()

# Method 1: Convert specific columns to factors
# Replace 'column_name' with your actual categorical column names
# Function to automatically convert appropriate columns to factors
convert_to_factors <- function(data, 
                               char_to_factor = TRUE, 
                               binary_to_factor = TRUE,
                               max_unique_numeric = 5,
                               exclude_cols = NULL,
                               include_cols = NULL) {
  
  # Load required library
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required")
  }
  
  library(dplyr)
  
  # Get column names to work with
  if (!is.null(include_cols)) {
    # If specific columns are specified, only work with those
    cols_to_check <- intersect(include_cols, names(data))
  } else {
    # Otherwise check all columns except excluded ones
    cols_to_check <- setdiff(names(data), exclude_cols)
  }
  
  # Initialize list to store columns to convert
  cols_to_convert <- character(0)
  
  for (col in cols_to_check) {
    col_data <- data[[col]]
    
    # Skip if already a factor
    if (is.factor(col_data)) {
      next
    }
    
    # Convert character columns to factors
    if (is.character(col_data) && char_to_factor) {
      cols_to_convert <- c(cols_to_convert, col)
      next
    }
    
    # Convert logical columns to factors  
    if (is.logical(col_data)) {
      cols_to_convert <- c(cols_to_convert, col)
      next
    }
    
    # Handle numeric columns
    if (is.numeric(col_data)) {
      unique_vals <- unique(col_data[!is.na(col_data)])
      n_unique <- length(unique_vals)
      
      # Convert binary numeric columns (0/1 or any 2 unique values)
      if (binary_to_factor && n_unique == 2) {
        cols_to_convert <- c(cols_to_convert, col)
        next
      }
      
      # Convert numeric columns with few unique values (likely categorical)
      if (n_unique <= max_unique_numeric && n_unique > 2) {
        # Check if values look like categories (integers, small range)
        if (all(unique_vals == round(unique_vals)) && max(unique_vals) - min(unique_vals) < 20) {
          cols_to_convert <- c(cols_to_convert, col)
        }
      }
    }
  }
  
  # Convert the identified columns to factors
  if (length(cols_to_convert) > 0) {
    cat("Converting to factors:", paste(cols_to_convert, collapse = ", "), "\n")
    
    data <- data %>%
      mutate(across(all_of(cols_to_convert), as.factor))
  } else {
    cat("No columns identified for conversion to factors.\n")
  }
  
  return(data)
}

# Helper function for more specific conversions
convert_specific_to_factors <- function(data, cols) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required")
  }
  
  library(dplyr)
  
  # Check if all specified columns exist
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    warning("Columns not found in data: ", paste(missing_cols, collapse = ", "))
    cols <- intersect(cols, names(data))
  }
  
  if (length(cols) > 0) {
    cat("Converting specified columns to factors:", paste(cols, collapse = ", "), "\n")
    data <- data %>%
      mutate(across(all_of(cols), as.factor))
  }
  
  return(data)
}
reg_converted <- convert_to_factors(reg_train)

# Handle conflicts
tidymodels_prefer()

# Verify CatBoost registration
cat("Checking CatBoost registration...\n")
tryCatch({
  model_env <- get_model_env()
  if ("catboost_reg" %in% model_env$models) {
    cat("✓ CatBoost successfully registered!\n")
  } else {
    stop("✗ CatBoost registration failed. Check catboost_model.R file.")
  }
}, error = function(e) {
  cat("Could not verify registration, but proceeding...\n")
})

# Check if constructor function exists
if (exists("catboost_reg", mode = "function")) {
  cat("✓ catboost_reg() constructor function available\n")
} else {
  cat("✗ catboost_reg() constructor function not found\n")
}

# Set seed
set.seed(8957)

# Parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# Load data ----
load(here("data_split/reg_folds.rda"))

# Load preprocessing/recipe ----
load(here("10_attempt/recipes/10b_recipe.rda"))

# Model specifications ----
# First try a simple fixed model to test basic functionality
cat("Creating model specification...\n")

# Test with fixed parameters first
catboost_spec_fixed <- catboost_reg(
  trees = 50,           # Fixed value for testing
  learn_rate = 0.1      # Fixed value for testing
) %>%
  set_mode("regression") %>% 
  set_engine("catboost", verbose = FALSE)

cat("Fixed model spec created successfully\n")

# For tuning, create a separate spec
catboost_spec <- catboost_reg() %>%  # Empty constructor first
  set_mode("regression") %>% 
  set_engine("catboost", verbose = FALSE)

# Add tunable parameters using update
catboost_spec$args$trees <- tune()
catboost_spec$args$learn_rate <- tune()

cat("Tunable model spec created\n")

# Set grid ----
catboost_grid <- grid_regular(
  trees(range = c(200, 1000)),
  learn_rate(range = c(0.01, 0.3)),
  levels = 3
)

# Optional: Check and correct grid if needed
# (This section was referencing undefined variables, so I've made it optional)
# if(exists("bt_grid_raw") && max(bt_grid_raw$learn_rate) > 0.3) {
#   cat("Grid space filling failed - correcting learn_rate manually...\n")
#   
#   catboost_grid <- bt_grid_raw %>%
#     mutate(
#       learn_rate = scales::rescale(learn_rate, to = c(0.01, 0.5))
#     )
# }

# Define workflow ----
# Test with fixed parameters first
catboost_wflow_fixed <- workflow() %>% 
  add_model(catboost_spec_fixed) %>% 
  add_recipe(recipe_filtered)

# Workflow for tuning
catboost_wflow <- workflow() %>% 
  add_model(catboost_spec) %>% 
  add_recipe(recipe_filtered)

# Test the workflow on a small subset first
cat("Testing workflow on small sample...\n")
tryCatch({
  # Get a small sample for testing
  if (exists("reg_folds")) {
    test_fold <- reg_folds$splits[[1]]
    train_data <- analysis(test_fold)
    test_data <- assessment(test_fold)
    
    # Take a small sample
    small_train <- train_data[1:min(100, nrow(train_data)), ]
    small_test <- test_data[1:min(50, nrow(test_data)), ]
    
    cat("Sample data dimensions - train:", dim(small_train), "test:", dim(small_test), "\n")
    
    # Test basic fitting with FIXED parameters (no tuning)
    cat("Testing fixed parameter workflow...\n")
    test_fit <- catboost_wflow_fixed %>%
      fit(small_train)
    
    cat("Model fitted successfully\n")
    
    # Test prediction
    cat("Testing predictions...\n")
    test_pred <- predict(test_fit, small_test)
    
    cat("✓ Basic workflow test passed!\n")
    cat("✓ Predictions shape:", dim(test_pred), "\n")
    cat("✓ First few predictions:", head(test_pred$.pred, 3), "\n")
    
  } else {
    cat("No folds object found, skipping test\n")
  }
}, error = function(e) {
  cat("✗ Workflow test failed:", e$message, "\n")
  cat("This suggests there are issues with the model implementation\n")
  
  # Print more debugging info
  cat("Error details:\n")
  print(e)
  cat("Traceback:\n")
  traceback()
})

# Tune/fit workflow/model ----
tic.clearlog()
tic("catboost_tune")

# Add error handling for the tuning process
catboost_tune <- tryCatch({
  tune_grid(
    catboost_wflow,
    resamples = reg_folds,
    grid = catboost_grid,  # Fixed variable name
    metrics = metric_set(mae, rmse),  # Start with basic metrics
    control = control_grid(
      save_pred = TRUE,
      parallel_over = "everything",  # Enable parallel processing
      verbose = TRUE,  # Add verbose output for debugging
      allow_par = TRUE,
      extract = NULL  # Don't extract additional info to avoid issues
    )
  )
}, error = function(e) {
  cat("Tuning failed with error:", e$message, "\n")
  # Save the error for debugging
  assign(".Last.tune.error", e, envir = .GlobalEnv)
  stop("Tuning process failed. Check .Last.tune.error for details.")
})

toc(log = TRUE)

# Extract timing information
time_log <- tic.log(format = FALSE)
catboost_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# Show best results
cat("Best CatBoost results:\n")
show_best(catboost_tune, metric = "mae", n = 5)

# Select best parameters
best_catboost <- select_best(catboost_tune, metric = "mae")
cat("\nBest parameters:\n")
print(best_catboost)

# Finalize workflow with best parameters
final_catboost_wflow <- finalize_workflow(catboost_wflow, best_catboost)

# Optional: Fit final model on full training set
# final_catboost_fit <- fit(final_catboost_wflow, data = train_data)

# Write out results (fitted/trained workflows & runtime info) ----
save(
  catboost_tune,           # Fixed variable name
  catboost_tictoc,         # Fixed variable name
  best_catboost,           # Added best parameters
  final_catboost_wflow,    # Added finalized workflow
  file = here("10_attempt/results/10a_catboost_tune.rda")
)

# Clean up parallel processing
plan(sequential)

# Optional: Print summary
cat("\nTuning completed successfully!\n")
cat("Runtime:", round(catboost_tictoc$runtime, 2), "seconds\n")
cat("Results saved to: 10_attempt/results/10a_catboost_tune.rda\n")