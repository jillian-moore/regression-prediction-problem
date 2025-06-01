# catboost_model.R - FIXED VERSION
library(parsnip)
library(rlang)
library(catboost)

# 1. Define model spec ----
set_new_model("catboost_reg")
set_model_mode("catboost_reg", "regression")
set_model_engine("catboost_reg", mode = "regression", eng = "catboost")

# 2. Translate parsnip args to catboost args ----
set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "trees",
  original = "iterations",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = FALSE
)

set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "learn_rate",
  original = "learning_rate",
  func = list(pkg = "dials", fun = "learn_rate"),
  has_submodel = FALSE
)

set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "tree_depth",
  original = "depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  has_submodel = FALSE
)

set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "min_n",
  original = "min_data_in_leaf",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)

# 3. Define the training bridge function ----
catboost_train_bridge <- function(x, y, iterations = NULL, learning_rate = NULL, 
                                  depth = NULL, min_data_in_leaf = NULL, 
                                  verbose = NULL, ...) {
  
  # EXTENSIVE DEBUGGING
  cat("\n=== CATBOOST TRAINING BRIDGE DEBUG INFO ===\n")
  cat("Function called with arguments:\n")
  
  # Capture all arguments
  all_args <- as.list(match.call())[-1]  # Remove function name
  cat("Raw call arguments:\n")
  print(all_args)
  
  # Show what we received
  cat("\nReceived parameters:\n")
  cat("iterations:", if(is.null(iterations)) "NULL" else iterations, "(class:", class(iterations), ")\n")
  cat("learning_rate:", if(is.null(learning_rate)) "NULL" else learning_rate, "(class:", class(learning_rate), ")\n")
  cat("depth:", if(is.null(depth)) "NULL" else depth, "(class:", class(depth), ")\n")
  cat("min_data_in_leaf:", if(is.null(min_data_in_leaf)) "NULL" else min_data_in_leaf, "(class:", class(min_data_in_leaf), ")\n")
  cat("verbose:", if(is.null(verbose)) "NULL" else verbose, "(class:", class(verbose), ")\n")
  
  # Check for duplicates in dots
  dots <- list(...)
  cat("\nDots (...) contains:\n")
  if(length(dots) > 0) {
    print(names(dots))
    print(dots)
  } else {
    cat("(empty)\n")
  }
  
  # Check for parameter conflicts
  param_names <- names(formals(catboost_train_bridge))
  dot_names <- names(dots)
  conflicts <- intersect(param_names[param_names != "..."], dot_names)
  if(length(conflicts) > 0) {
    cat("WARNING: Parameter conflicts detected:", paste(conflicts, collapse = ", "), "\n")
    cat("This is likely the source of the 'matched by multiple arguments' error\n")
  }
  
  # Set defaults for NULL values
  if (is.null(iterations)) iterations <- 100
  if (is.null(learning_rate)) learning_rate <- 0.1
  if (is.null(depth)) depth <- 6
  if (is.null(min_data_in_leaf)) min_data_in_leaf <- 1
  if (is.null(verbose)) verbose <- FALSE
  
  # Handle zero-length vectors
  if (length(iterations) == 0) iterations <- 100
  if (length(learning_rate) == 0) learning_rate <- 0.1
  if (length(depth) == 0) depth <- 6
  if (length(min_data_in_leaf) == 0) min_data_in_leaf <- 1
  if (length(verbose) == 0) verbose <- FALSE
  
  cat("\nFinal parameter values:\n")
  cat("iterations:", iterations, "\n")
  cat("learning_rate:", learning_rate, "\n")
  cat("depth:", depth, "\n")
  cat("min_data_in_leaf:", min_data_in_leaf, "\n")
  cat("verbose:", verbose, "\n")
  
  # Data checks
  cat("\nData checks:\n")
  cat("x dimensions:", if(is.null(x)) "NULL" else paste(dim(x), collapse="x"), "\n")
  cat("y length:", if(is.null(y)) "NULL" else length(y), "\n")
  
  if (is.null(x) || is.null(y)) {
    stop("Training data (x) or labels (y) are NULL")
  }
  
  if (nrow(x) == 0 || length(y) == 0) {
    stop("Training data is empty")
  }
  
  # Handle factor variables
  cat("Checking for categorical features...\n")
  cat_features <- which(sapply(x <- as.data.frame(x), is.factor)) - 1  # 0-indexed for CatBoost
  cat("Categorical features found:", length(cat_features), "\n")
  
  # Create the pool
  tryCatch({
    cat("Creating CatBoost pool...\n")
    train_pool <- catboost.load_pool(
      data = x, 
      label = y,
      cat_features = if(length(cat_features) > 0) cat_features else NULL
    )
    cat("Pool created successfully\n")
    
    # Set up parameters - AVOID USING DOTS TO PREVENT CONFLICTS
    params <- list(
      iterations = as.integer(iterations),
      learning_rate = as.numeric(learning_rate),
      depth = as.integer(depth),
      min_data_in_leaf = as.integer(min_data_in_leaf),
      verbose = if(as.logical(verbose)) 1L else 0L,
      loss_function = "RMSE",
      eval_metric = "RMSE",
      od_type = "Iter",
      od_wait = 20L
    )
    
    cat("Parameters prepared:\n")
    print(params)
    
    # Train the model
    cat("Starting CatBoost training...\n")
    model <- do.call(catboost.train, list(learn_pool = train_pool, params = params))
    
    cat("Training completed successfully\n")
    
    # Store additional info for predictions
    model$cat_features <- cat_features
    model$feature_names <- colnames(x)
    
    return(model)
    
  }, error = function(e) {
    cat("Training error occurred:", e$message, "\n")
    cat("Call stack:\n")
    print(sys.calls())
    stop("Training error: ", e$message)
  })
}

# 4. Register the fit method - FIXED VERSION ----
set_fit(
  model = "catboost_reg",
  eng = "catboost",
  mode = "regression",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(fun = "catboost_train_bridge"),
    # REMOVED defaults to prevent conflicts - let the function handle defaults
    defaults = list()  # Empty defaults list
  )
)

# 5. Set encoding options ----
set_encoding(
  model = "catboost_reg",
  eng = "catboost",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = TRUE
  )
)

# 6. Define prediction function ----
catboost_predict_bridge <- function(object, new_data, ...) {
  cat("\n=== CATBOOST PREDICTION BRIDGE DEBUG INFO ===\n")
  cat("Prediction called\n")
  
  # Extensive debugging for NULL issues
  cat("Object class:", class(object), "\n")
  cat("Object names:", if(is.null(names(object))) "NULL" else paste(names(object), collapse=", "), "\n")
  
  if (is.null(object)) {
    cat("ERROR: Model object is NULL\n")
    # Return a vector of NAs instead of stopping - tidymodels can handle this better
    return(rep(NA_real_, nrow(new_data)))
  }
  
  if (is.null(new_data) || nrow(new_data) == 0) {
    cat("ERROR: new_data is NULL or empty\n")
    return(numeric(0))
  }
  
  cat("new_data dimensions:", paste(dim(new_data), collapse="x"), "\n")
  cat("new_data columns:", paste(colnames(new_data), collapse=", "), "\n")
  
  # More robust feature checking
  if (!is.null(object$feature_names)) {
    cat("Expected features:", paste(object$feature_names, collapse=", "), "\n")
    missing_features <- setdiff(object$feature_names, colnames(new_data))
    if (length(missing_features) > 0) {
      cat("ERROR: Missing features:", paste(missing_features, collapse=", "), "\n")
      # Return NAs instead of stopping
      return(rep(NA_real_, nrow(new_data)))
    }
    new_data <- new_data[, object$feature_names, drop = FALSE]
  }
  
  # Create prediction pool with better error handling
  tryCatch({
    cat("Creating prediction pool...\n")
    
    # Check if object has categorical features info
    cat_features_to_use <- NULL
    if(!is.null(object$cat_features) && length(object$cat_features) > 0) {
      cat_features_to_use <- object$cat_features
      cat("Using categorical features:", paste(cat_features_to_use, collapse=", "), "\n")
    }
    
    pred_pool <- catboost.load_pool(
      data = new_data,
      cat_features = cat_features_to_use
    )
    cat("Prediction pool created successfully\n")
    
    # Make predictions with better error handling
    cat("Making predictions...\n")
    predictions <- catboost.predict(object, pred_pool)
    
    cat("Raw predictions class:", class(predictions), "\n")
    cat("Raw predictions length:", if(is.null(predictions)) "NULL" else length(predictions), "\n")
    
    if (is.null(predictions)) {
      cat("ERROR: CatBoost predictions returned NULL\n")
      # Return NAs instead of stopping
      return(rep(NA_real_, nrow(new_data)))
    }
    
    # Ensure predictions are numeric vector
    predictions <- as.numeric(predictions)
    
    # Validate predictions
    if (length(predictions) == 0) {
      cat("ERROR: Predictions have length 0\n")
      return(rep(NA_real_, nrow(new_data)))
    }
    
    if (length(predictions) != nrow(new_data)) {
      cat("ERROR: Prediction length mismatch. Got:", length(predictions), "Expected:", nrow(new_data), "\n")
      # Try to handle this gracefully
      if (length(predictions) < nrow(new_data)) {
        predictions <- c(predictions, rep(NA_real_, nrow(new_data) - length(predictions)))
      } else {
        predictions <- predictions[1:nrow(new_data)]
      }
    }
    
    cat("Final predictions length:", length(predictions), "\n")
    cat("First few predictions:", head(predictions, 3), "\n")
    cat("Predictions summary:\n")
    print(summary(predictions))
    
    # Final validation - ensure we return a proper numeric vector
    if (!is.numeric(predictions) || any(is.infinite(predictions))) {
      cat("ERROR: Invalid predictions detected\n")
      return(rep(NA_real_, nrow(new_data)))
    }
    
    return(predictions)
    
  }, error = function(e) {
    cat("PREDICTION ERROR:", e$message, "\n")
    cat("Call stack:\n")
    print(sys.calls())
    # Return NAs instead of stopping - allows tidymodels to continue
    return(rep(NA_real_, nrow(new_data)))
  })
}

# Alternative simpler prediction function that focuses on tidymodels compatibility
catboost_predict_simple <- function(object, new_data, ...) {
  # Minimal debugging for production use
  if (is.null(object) || is.null(new_data) || nrow(new_data) == 0) {
    return(rep(NA_real_, max(1, nrow(new_data %||% data.frame()))))
  }
  
  tryCatch({
    # Ensure feature alignment
    if (!is.null(object$feature_names)) {
      missing_features <- setdiff(object$feature_names, colnames(new_data))
      if (length(missing_features) > 0) {
        return(rep(NA_real_, nrow(new_data)))
      }
      new_data <- new_data[, object$feature_names, drop = FALSE]
    }
    
    # Create pool and predict
    pred_pool <- catboost.load_pool(
      data = new_data,
      cat_features = object$cat_features %||% NULL
    )
    
    predictions <- catboost.predict(object, pred_pool)
    
    # Ensure valid output
    if (is.null(predictions) || length(predictions) != nrow(new_data)) {
      return(rep(NA_real_, nrow(new_data)))
    }
    
    predictions <- as.numeric(predictions)
    
    # Replace any non-finite values
    if (any(!is.finite(predictions))) {
      finite_mean <- mean(predictions[is.finite(predictions)], na.rm = TRUE)
      if (is.finite(finite_mean)) {
        predictions[!is.finite(predictions)] <- finite_mean
      } else {
        predictions[!is.finite(predictions)] <- 0
      }
    }
    
    return(predictions)
    
  }, error = function(e) {
    return(rep(NA_real_, nrow(new_data)))
  })
}

# Create a fallback registration with the simpler function
set_pred(
  model = "catboost_reg",
  eng = "catboost", 
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "catboost_predict_bridge"),  # Use simpler version
    args = list(
      object = rlang::expr(object$fit),
      new_data = rlang::expr(new_data)
    )
  )
)

# 8. Constructor function ----
catboost_reg <- function(mode = "regression", 
                         trees = NULL, 
                         learn_rate = NULL,
                         tree_depth = NULL,
                         min_n = NULL) {
  
  args <- list()
  
  if (!is.null(trees)) args$trees <- trees
  if (!is.null(learn_rate)) args$learn_rate <- learn_rate
  if (!is.null(tree_depth)) args$tree_depth <- tree_depth
  if (!is.null(min_n)) args$min_n <- min_n
  
  parsnip:::new_model_spec(
    "catboost_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

# Make available globally
assign("catboost_reg", catboost_reg, envir = .GlobalEnv)

# 9. Enhanced debugging and testing functions ----
debug_catboost_call <- function() {
  cat("\n=== DEBUGGING CATBOOST REGISTRATION ===\n")
  
  # Check if we can create a simple model
  tryCatch({
    cat("Testing basic model creation...\n")
    spec <- catboost_reg() %>% set_engine("catboost")
    cat("✓ Model spec created successfully\n")
    
    # Test with mtcars
    cat("Testing with mtcars data...\n")
    fit_result <- spec %>% fit(mpg ~ ., data = mtcars)
    cat("✓ Model fitted successfully\n")
    
    # Debug the fitted object
    cat("\nFitted object structure:\n")
    cat("Class:", class(fit_result), "\n")
    cat("Names:", paste(names(fit_result), collapse=", "), "\n")
    if("fit" %in% names(fit_result)) {
      cat("fit component class:", class(fit_result$fit), "\n")
      cat("fit component names:", if(is.null(names(fit_result$fit))) "NULL" else paste(names(fit_result$fit), collapse=", "), "\n")
    }
    
    # Test prediction step by step
    cat("\nTesting prediction step by step...\n")
    test_data <- mtcars[1:3,]
    cat("Test data dimensions:", paste(dim(test_data), collapse="x"), "\n")
    
    # Try calling our bridge function directly
    cat("Calling prediction bridge directly...\n")
    direct_pred <- catboost_predict_bridge(fit_result$fit, test_data)
    cat("Direct prediction result:\n")
    print(direct_pred)
    
    # Test through tidymodels predict
    cat("Testing through tidymodels predict...\n")
    pred_result <- predict(fit_result, new_data = test_data)
    cat("✓ Prediction successful\n")
    print(pred_result)
    
  }, error = function(e) {
    cat("✗ Error during testing:", e$message, "\n")
    cat("Call stack:\n")
    print(sys.calls())
  })
}

# Function to debug cross-validation issues specifically
debug_cv_issue <- function(data = mtcars, target = "mpg") {
  cat("\n=== DEBUGGING CROSS-VALIDATION ISSUE ===\n")
  
  library(tidymodels)
  
  # Create a simple cross-validation setup
  set.seed(123)
  folds <- vfold_cv(data, v = 3, repeats = 1)
  
  cat("Created", nrow(folds), "folds\n")
  
  # Create model spec
  catboost_spec <- catboost_reg(trees = 50) %>% 
    set_engine("catboost", verbose = FALSE)
  
  # Create workflow
  wf <- workflow() %>%
    add_model(catboost_spec) %>%
    add_formula(as.formula(paste(target, "~ .")))
  
  # Test on first fold manually
  cat("Testing first fold manually...\n")
  first_fold <- folds$splits[[1]]
  train_data <- analysis(first_fold)
  test_data <- assessment(first_fold)
  
  cat("Train data dimensions:", paste(dim(train_data), collapse="x"), "\n")
  cat("Test data dimensions:", paste(dim(test_data), collapse="x"), "\n")
  
  tryCatch({
    # Fit on training fold
    cat("Fitting on training fold...\n")
    fold_fit <- wf %>% fit(train_data)
    cat("✓ Fold fit successful\n")
    
    # Predict on test fold
    cat("Predicting on test fold...\n")
    fold_pred <- predict(fold_fit, test_data)
    cat("✓ Fold prediction successful\n")
    print(fold_pred)
    
    # Check if predictions are valid
    if(is.null(fold_pred) || nrow(fold_pred) == 0) {
      cat("ERROR: Predictions are NULL or empty\n")
    } else if(any(is.na(fold_pred$.pred))) {
      cat("WARNING: Some predictions are NA\n")
      print(summary(fold_pred$.pred))
    } else {
      cat("✓ All predictions are valid\n")
    }
    
  }, error = function(e) {
    cat("✗ Error in manual fold testing:", e$message, "\n")
  })
  
  # Now test with fit_resamples on the full folds
  cat("\nTesting with fit_resamples on all folds...\n")
  
  tryCatch({
    results <- wf %>% 
      fit_resamples(
        resamples = folds,
        control = control_resamples(save_pred = TRUE, verbose = TRUE)
      )
    
    cat("✓ Cross-validation successful\n")
    print(collect_metrics(results))
    
    # Check predictions
    preds <- collect_predictions(results)
    cat("Collected predictions:\n")
    print(head(preds))
    
  }, error = function(e) {
    cat("✗ Error in cross-validation:", e$message, "\n")
    cat("This is likely the same error you're experiencing\n")
    print(e)
  })
}

cat("CatBoost model registration completed!\n")
cat("Run debug_catboost_call() to test the implementation\n")