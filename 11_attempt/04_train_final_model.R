# Regression AirBnB Problem ----
# Analysis of tuned and trained models (comparisons)
# Select final model
# Fit & analyze final model

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
set.seed(3765)

# load data ----
load(here("data_split/reg_test.rda"))
load(here("data_split/reg_train.rda"))

# load necessary objects ----
load(here("11_attempt/results/11a_bt_gbm_tune.rda"))

# collect metrics ----
bt_gbm_tune |> 
  collect_metrics() |> 
  filter(.metric == "mae") |> 
  arrange(mean)

bt_gbm_tune |> 
  autoplot(metric = "mae")

# best params ---- 
best_params <- select_best(bt_gbm_tune, metric = "mae")

# finalize workflow ----
bt_wflow <- extract_workflow(bt_gbm_tune)
final_bt <- finalize_workflow(
  bt_wflow,
  best_params
)

# finalize fit and predictions ----
final_fit <- fit(final_bt, reg_train)
predictions <- predict(final_fit, reg_test)

# make table ----
bt_submit <- tibble(
  id = reg_test$id,
  predicted = 10^predictions$.pred
)

# save out ----
write_csv(bt_submit, here('11_attempt/results/11a_bt_submission.csv'))
