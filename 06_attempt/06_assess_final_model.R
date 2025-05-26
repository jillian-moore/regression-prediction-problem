# L07 Ensemble Models ----
# Assess trained ensemble model

# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(here)
library(stacks)
library(ensr)

# Handle common conflicts
tidymodels_prefer()

# Load testing data
load(here("data_split/reg_test.rda"))
load(here("data_split/reg_train.rda"))
load(here("06_attempt/recipes/6a_recipe.rda"))

# Get the column names the recipe expects
required_cols <- recipe_prep$var_info$variable[recipe_prep$var_info$role %in% c("predictor", "outcome")]

# Check which are missing
missing_cols <- setdiff(required_cols, names(reg_test))
print(missing_cols)  # See exactly what's missing

# Add them with 0s (appropriate for amenity dummies)
for(col in missing_cols) {
  reg_test[[col]] <- 0
}

# Load trained ensemble model info
load(here("06_attempt/results/reg_ensemble.rda"))
load(here("06_attempt/results/reg_stack_blend.rda"))

# predict on test set ----
ensemble_predictions <- predict(reg_ensemble, new_data = reg_test)

# make final submission table ----
ensemble_submit <- tibble(
  id = reg_test$id,
  predicted = exp(ensemble_predictions$.pred)
)

# save out submission ----
write_csv(ensemble_submit, here("06_attempt/results/6a_ensemble_submission.csv"))
