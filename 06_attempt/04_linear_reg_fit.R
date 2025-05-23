# L07 Ensemble Models ----
# Fit linear regression model

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(here)

# Handle common conflicts
tidymodels_prefer()

# load required objects ----
load(here("data_split/reg_folds.rda"))

# load preprocessing/recipe ----
load(here("06_attempt/recipes/6a_recipe.rda"))

# model specification ----
lin_reg_spec <-
  linear_reg() |>
  set_mode("regression") |>
  set_engine("lm")

# workflow ----
lin_reg_wflow <-
  workflow() |>
  add_model(lin_reg_spec) |>
  add_recipe(recipe_improved_filtered)

# tuning/fitting ----
lin_reg_fit <-
  lin_reg_wflow |>
  fit_resamples(
    resamples = reg_folds,
    control = stacks::control_stack_resamples(), 
    save_workflow = TRUE, 
    verbose = TRUE
  )

# save out
save(lin_reg_fit, file = here("06_attempt/results/6a_lin_reg_fit.rda"))
