# L07 Ensemble Models ----
# Train & explore ensemble model

# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(here)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info ----
load(here("results/knn_res.rda"))
load(here("results/svm_res.rda"))
load(here("results/lin_reg_res.rda"))

# Create data stack ----
wildfires_data_stacks <- 
  stacks() |> 
  add_candidates(knn_res) |> 
  add_candidates(svm_res) |> 
  add_candidates(lin_reg_res)

# Fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

# Blend predictions (tuning step, set seed)
set.seed(9876) # needed bc tuning process happening in background
wildfires_stack_blend <- 
  wildfires_data_stacks |> 
  blend_predictions(penalty = blend_penalty)

# Save blended model stack for reproducibility & easy reference (for report)
save(wildfires_stack_blend, file = here("results/wildfires_stack_blend.rda"))

# Explore the blended model stack
wildfires_stack_blend |> 
  autoplot()

wildfires_stack_blend |> 
  autoplot(type = "members")

# fit to training set ----
wildfires_ensemble <- fit_members(wildfires_stack_blend)

# Save trained ensemble model for reproducibility & easy reference (for report)
save(wildfires_ensemble, file = here("results/wildfires_ensemble.rda") )

