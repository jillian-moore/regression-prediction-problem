# Model Comparison: LASSO vs LightGBM Variable Selection ----
# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)
library(glmnet)
library(ggplot2)
library(patchwork)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("data_split/reg_train.rda"))

# load fitted models ----
load(here("09_attempt/results/9a_var_select_fit_bt.rda"))     
load(here("09_attempt/results/9a_var_select_fit_lasso.rda"))  

# create test folds for comparison ----
set.seed(9557)
comparison_folds <- reg_train |> 
  vfold_cv(v = 5, repeats = 2, strata = price)

# function to evaluate model performance ----
evaluate_model <- function(workflow_fit, folds, model_name) {
  
  cv_results <- map_dfr(folds$splits, function(split) {

    analysis_data <- analysis(split)
    assessment_data <- assessment(split)
    
    fold_fit <- fit(workflow_fit, analysis_data)
    
    preds <- predict(fold_fit, assessment_data)
    
    truth <- assessment_data$price
    pred_values <- preds$.pred
    
    tibble(
      mae = mean(abs(pred_values - truth)),
      rmse = sqrt(mean((pred_values - truth)^2)),
      rsq = cor(pred_values, truth)^2,
      fold = as.character(split)
    )
  })
  
  cv_results$model <- model_name
  return(cv_results)
}

# compare models on cross-validation ----
lgb_results <- evaluate_model(
  var_select_fit_bt,
  comparison_folds, 
  "LightGBM"
)

lasso_results <- evaluate_model(
  var_select_fit_lasso, 
  comparison_folds, 
  "LASSO"
)

# Combine results ----
all_results <- bind_rows(lgb_results, lasso_results)

# performance summary ----
performance_summary <- all_results |>
  group_by(model) |>
  summarise(
    mean_mae = mean(mae),
    sd_mae = sd(mae),
    mean_rmse = mean(rmse),
    sd_rmse = sd(rmse),
    mean_rsq = mean(rsq),
    sd_rsq = sd(rsq),
    .groups = "drop"
  ) |>
  arrange(mean_mae)

# LASSO feature selection
print(lasso_coefs |> slice_head(n = 20))

lasso_vars <- var_select_fit_lasso$pre$mold$predictors %>%
  colnames()

# xxtract LightGBM-important features ----
lgb_fit <- var_select_fit_bt$fit$fit$fit
lgb_importance <- lgb.importance(lgb_fit)

lgb_importance_df <- as_tibble(lgb_importance) |> 
  rename(feature = Feature, importance = Gain) |> 
  filter(importance > 0) |> 
  arrange(desc(importance))

lgb_vars <- lgb_importance_df$feature

# define feature sets ----
union_vars <- union(lasso_vars, lgb_vars)
intersect_vars <- intersect(lasso_vars, lgb_vars)

# save comparison results ----
save(all_results, performance_summary, intersect_vars, union_vars,
     file = here("09_attempt/results/9a_model_comparison.rda"))