# Regression AirBnB Problem ----
# Variable selection
# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)
library(finetune)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("data_split/reg_train.rda"))

# create resamples/folds ----
set.seed(9557)
bt_folds <- reg_train |> 
  vfold_cv(v = 5, repeats = 3, strata = price)

# improved selection recipe ----
recipe_select <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean,
          host_about, host_about_clean) |>
  step_impute_knn(
    c("accommodates", "bathrooms_text", "host_response_rate",
      "host_acceptance_rate", "number_of_reviews"),
    neighbors = 5
  ) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_string2factor(all_nominal_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.001) |>  
  step_unknown(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# check recipe
recipe_prepped <- recipe_select |> 
  prep()

recipe_baked <- recipe_prepped |> 
  bake(new_data = NULL)

# improved model specifications with regularization ----
bt_spec <- boost_tree(
  trees = tune(),
  mtry = tune(),
  min_n = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  loss_reduction = tune()  
) |> 
  set_mode("regression") |> 
  set_engine("lightgbm", 
             feature_fraction = 0.8,    
             lambda_l1 = tune(),       
             lambda_l2 = tune(),        
             verbose = -1              
  )

# define workflows ----
bt_wflow <- workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(recipe_select)

# hyperparameter tuning values ----
bt_params <- extract_parameter_set_dials(bt_spec) |>
  update(
    mtry = mtry(range = c(10, 100)),          
    min_n = min_n(range = c(5, 50)),
    trees = trees(range = c(500, 2000)),      
    tree_depth = tree_depth(range = c(4, 12)),
    learn_rate = learn_rate(range = c(0.001, 0.2)),  
    loss_reduction = loss_reduction(range = c(0, 2)),
    lambda_l1 = penalty(range = c(-10, 0)),    
    lambda_l2 = penalty(range = c(-10, 0))     
  )

# build larger tuning grid for better exploration
bt_grid <- grid_space_filling(bt_params, size = 25)  

# fit workflow/model with additional metrics ----
bt_tuned <- bt_wflow |> 
  tune_grid(
    resamples = bt_folds,
    grid = bt_grid,
    metrics = metric_set(mae),  
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE,  
      verbose = TRUE
    )
  )

# show tuning results
show_best(bt_tuned, metric = "mae", n = 5)

# extract best model (optimal tuning parameters) ----
best_params <- select_best(bt_tuned, metric = "mae")
optimal_wflow <- bt_wflow |> 
  finalize_workflow(best_params)

# fit best model/results ----
var_select_fit_bt <- fit(optimal_wflow, reg_train)

# function to handle dummy variable alignment for testing ----
align_dummy_variables <- function(model_fit, new_data) {
  # extract the recipe from the fitted workflow
  recipe_fit <- model_fit$pre$mold$blueprint$recipe
  
  # process new data through the recipe
  tryCatch({
    processed_data <- bake(recipe_fit, new_data = new_data)
    return(processed_data)
  }, error = function(e) {
    warning("Error in recipe processing: ", e$message)
    # Fallback: return original data
    return(new_data)
  })
}

# create a robust prediction function ----
predict_robust <- function(model_fit, new_data) {
  tryCatch({
    # Process data through recipe
    processed_data <- align_dummy_variables(model_fit, new_data)
    
    # make predictions
    predictions <- predict(model_fit, new_data = processed_data)
    return(predictions)
  }, error = function(e) {
    warning("Prediction error: ", e$message)
    return(NULL)
  })
}

# model diagnostics ----
# extract variable importance if available
if(any(grepl("lightgbm", class(var_select_fit_bt$fit$fit$fit)))) {
  cat("Model trained successfully with LightGBM engine.\n")
  
  # extract feature importance
  importance_scores <- var_select_fit_bt$fit$fit$fit$feature_importance()
  feature_names <- var_select_fit_bt$pre$mold$predictors |> colnames()
  
  if(length(importance_scores) == length(feature_names)) {
    importance_df <- tibble(
      feature = feature_names,
      importance = importance_scores
    ) |>
      arrange(desc(importance)) |>
      slice_head(n = 20)  # Top 20 features
    
    cat("Top 20 most important features:\n")
    print(importance_df)
  }
}

# cross-validation performance summary
cv_metrics <- collect_metrics(bt_tuned) |>
  filter(.metric == "mae") |>
  summarise(
    best_mae = min(mean),
    mean_mae = mean(mean),
    std_mae = sd(mean)
  )

# write out variable selection results ----
save(var_select_fit_bt, file = here("09_attempt/results/9a_var_select_fit_bt.rda"))

# also save the prediction function and alignment function
save(predict_robust, align_dummy_variables, 
     file = here("09_attempt/results/9a_prediction_helpers.rda"))