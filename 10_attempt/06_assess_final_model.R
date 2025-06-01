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
load(here("data_split/reg_train.rda"))
load(here("10_attempt/recipes/10b_recipe.rda"))

# load trained ensemble model info ----
load(here("10_attempt/results/10a_reg_ensemble.rda"))
load(here("10_attempt/results/10a_reg_stack_blend.rda"))

# get the column names the recipe expects ----
required_cols <- recipe_filtered$var_info$variable[recipe_filtered$var_info$role %in% c("predictor", "outcome")]

# check best models ----
reg_stack_blend$coefs |>  
  tidy() |> 
  filter(estimate != 0) |> 
  arrange(desc(abs(estimate)))

# check which are missing
missing_cols <- setdiff(required_cols, names(reg_test))
print(missing_cols)  

# add them with 0s 
for(col in missing_cols) {
  reg_test[[col]] <- 0
}

# predict on test set ----
ensemble_predictions <- predict(reg_ensemble, new_data = reg_test)

# make final submission table ----
ensemble_submit <- tibble(
  id = reg_test$id,
  predicted = exp(ensemble_predictions$.pred)
)

# save out submission ----
write_csv(ensemble_submit, here("10_attempt/results/10a_ensemble_submission.csv"))
