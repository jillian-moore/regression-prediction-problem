# Regression AirBnB Problem ----
# Train & explore ensemble model

# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(here)
library(bonsai)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info ----
load(here("07_attempt/results/7a_lin_reg_fit.rda"))
load(here("07_attempt/results/7a_bt_cat_tune.rda"))
load(here("07_attempt/results/7a_bt_gbm_tune.rda"))
load(here("07_attempt/results/7a_bt_xg_tune.rda"))
load(here("07_attempt/results/7a_rf_tune.rda"))
load(here("07_attempt/results/7a_svm_tune.rda"))

# create data stack ----
reg_data_stacks <- 
  stacks() |> 
  add_candidates(lin_reg_fit) |> 
  add_candidates(rf_tune) |> 
  add_candidates(bt_gbm_tune) |> 
  add_candidates(bt_cat_tune) |> 
  add_candidates(bt_xg_tune) |> 
  add_candidates(svm_tune)

# fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

# blend predictions (tuning step, set seed)
set.seed(9854) # needed bc tuning process happening in background
reg_stack_blend <- 
  reg_data_stacks |> 
  blend_predictions(penalty = blend_penalty, control = control_stack_grid())

# save blended model stack
save(reg_stack_blend, file = here("07_attempt/results/7a_reg_stack_blend.rda"))

# explore the blended model stack ----
reg_stack_blend |> 
  autoplot()

reg_stack_blend |> 
  autoplot(type = "members")

# fit to training set ----
reg_ensemble <- fit_members(reg_stack_blend)

# save trained ensemble model ----
save(reg_ensemble, file = here("07_attempt/results/7a_reg_ensemble.rda"))