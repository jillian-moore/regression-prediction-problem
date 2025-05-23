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
set.seed(3775)

# load data ----
load(here("data_split/reg_test.rda"))
load(here("data_split/reg_train.rda"))

# load necessary objects ----
load(here("05_attempt/results/5a_bt_tune.rda"))

# collect metrics ----
bt_tune |> 
  collect_metrics() |> 
  filter(.metric == "mae") |> 
  arrange(mean)

bt_tune |> 
  autoplot(metric = "mae")
# increase learn rate, search mtry more 45-70

# best params ---- 
best_params <- select_best(bt_tune, metric = "mae")

# finalize workflow ----
bt_wflow <- extract_workflow(bt_tune)
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
  predicted = exp(predictions$.pred)
)

# save out ----
write_csv(bt_submit, here('05_attempt/results/5a_bt_submission.csv'))
