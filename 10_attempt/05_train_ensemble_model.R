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

# load candidate model info ----
load(here("10_attempt/results/10a_bt_gbm_tune.rda"))
load(here("10_attempt/results/10b_bt_xg_tune.rda"))
load(here("10_attempt/results/10b_svm_tune.rda"))

# create data stack ----
reg_data_stacks <- 
  stacks() |> 
  add_candidates(bt_gbm_tune) |> 
  add_candidates(bt_xg_tune) |> 
  add_candidates(svm_tune)

# fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^seq(-8, -1, length.out = 20), 0.25, 0.5, 1, 1.5, 2)

# blend predictions (tuning step, set seed)
set.seed(98554) # needed bc tuning process happening in background
reg_stack_blend <- reg_data_stacks |> 
  blend_predictions()

# save blended model stack
save(reg_stack_blend, file = here("10_attempt/results/10a_reg_stack_blend.rda"))

# explore the blended model stack ----
reg_stack_blend |> 
  autoplot()

reg_stack_blend |> 
  autoplot(type = "members")

# next, go thru these and re-train with metrics properly set
collect_metrics(bt_xg_tune) |> 
  filter(.metric == "mae") |> 
  arrange(mean) %>%
  slice_head(n = 10)

# fit to training set ----
reg_ensemble <- fit_members(reg_stack_blend)

# save trained ensemble model ----
save(reg_ensemble, file = here("10_attempt/results/10a_reg_ensemble.rda"))