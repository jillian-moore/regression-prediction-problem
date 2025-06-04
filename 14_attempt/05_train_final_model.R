# Regression AirBnB Problem ----
# Analysis of tuned and trained models (comparisons)
# Select final model for each location
# Fit & analyze final models
# Create combined predictions

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(bonsai)
library(future)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# set seed
set.seed(3765755)

# load data ----
load(here("14_attempt/data_split/asheville_test.rda"))
load(here("14_attempt/data_split/chicago_test.rda"))  
load(here("14_attempt/data_split/kauai_test.rda"))

load(here("14_attempt/data_split/asheville_train.rda"))  
load(here("14_attempt/data_split/chicago_train.rda"))    
load(here("14_attempt/data_split/kauai_train.rda"))     

# load recipes ----
load(here("14_attempt/recipes/14a_recipe_asheville.rda"))
load(here("14_attempt/recipes/14a_recipe_chicago.rda"))
load(here("14_attempt/recipes/14a_recipe_kauai.rda"))

# load tuning results ----
load(here("14_attempt/results/14a_bt_combined_results.rda"))

# create location configurations ----
locations <- list(
  asheville = list(
    train = asheville_train,
    test = asheville_test,
    recipe = simple_recipe_asheville,  
    tune_result = all_results$asheville$tune_result
  ),
  chicago = list(
    train = chicago_train,
    test = chicago_test,
    recipe = simple_recipe_chicago,    
    tune_result = all_results$chicago$tune_result
  ),
  kauai = list(
    train = kauai_train,
    test = kauai_test,
    recipe = simple_recipe_kauai,     
    tune_result = all_results$kauai$tune_result
  )
)

# function to analyze and train each city model ----
train_location_model <- function(location_name, location_config) {
  
  # collect and display metrics
  metrics_summary <- location_config$tune_result |> 
    collect_metrics() |> 
    filter(.metric == "mae") |> 
    arrange(mean)
  
  print(metrics_summary |> head())
  
  autoplot_result <- location_config$tune_result |> autoplot(metric = "mae")
  ggsave(here("13_attempt/results", paste0("13a_", location_name, "_autoplot.png")),
         autoplot_result, width = 10, height = 6)
  
  # get best parameters
  best_params <- select_best(location_config$tune_result, metric = "mae")
  
  # extract and finalize workflow
  bt_wflow <- extract_workflow(location_config$tune_result)
  final_bt <- finalize_workflow(bt_wflow, best_params)
  
  # fit final model
  final_fit <- fit(final_bt, location_config$train)
  
  # make predictions
  predictions <- predict(final_fit, location_config$test)
  
  # create submission tibble for this city
  location_submit <- tibble(
    id = location_config$test$id,
    predicted = 10^predictions$.pred,  
    location = location_name
  )
  
  # save individual city results
  save(
    final_fit,
    best_params,
    predictions,
    file = here("14_attempt/results", paste0("13a_", location_name, "_final_model.rda"))
  )
  
  return(list(
    final_fit = final_fit,
    predictions = location_submit,
    best_params = best_params,
    best_mae = min(metrics_summary$mean)
  ))
}

# train all location models ----
final_results <- map2(names(locations), locations, train_location_model)
names(final_results) <- names(locations)

# combine all predictions ----
combined_predictions <- map_dfr(final_results, ~.x$predictions)

# create final submission file 
final_submit <- combined_predictions |> 
  select(id, predicted) |>
  arrange(id)

# create summary of model performance ----
model_summary <- tibble(
  location = names(final_results),
  best_mae = map_dbl(final_results, ~.x$best_mae),
  n_predictions = map_int(final_results, ~nrow(.x$predictions))
)

# save out ----
write_csv(final_submit, here('14_attempt/results/14a_bt_submission.csv'))
write_csv(combined_predictions, here('14_attempt/results/14a_bt_predictions_with_locations.csv'))
write_csv(model_summary, here('14_attempt/results/14a_model_summary.csv'))
save(
  final_results,
  combined_predictions,
  final_submit,
  model_summary,
  file = here("14_attempt/results/14a_final_results.rda")
)