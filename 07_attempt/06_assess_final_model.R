# Regression AirBnB Problem ----
# Assess trained ensemble model

# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(here)
library(stacks)
library(ensr)

# Handle common conflicts
tidymodels_prefer()

# load testing data ----
load(here("data_split/reg_test.rda"))

# load trained ensemble model info ----
load(here("07_attempt/results/7a_reg_ensemble.rda"))
load(here("07_attempt/results/7a_reg_stack_blend.rda"))

# predict on test set ----
ensemble_predictions <- predict(reg_ensemble, new_data = reg_test)

# make final submission table ----
ensemble_submit <- tibble(
  id = reg_test$id,
  predicted = exp(ensemble_predictions$.pred)
)

# save out submission ----
write_csv(ensemble_submit, here("07_attempt/results/7a_ensemble_submission.csv"))
