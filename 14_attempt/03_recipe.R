# Regression AirBnB Problem ----
# Setup preprocessing/recipes/feature engineering

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# load objects ----
load(here("14_attempt/data_split/asheville_train.rda"))
load(here("14_attempt/data_split/chicago_train.rda"))
load(here("14_attempt/data_split/kauai_train.rda"))

load(here("14_attempt/data_split/asheville_test.rda"))
load(here("14_attempt/data_split/chicago_test.rda"))
load(here("14_attempt/data_split/kauai_test.rda"))

load(here("14_attempt/results/14a_var_select_fit_bt_kauai.rda"))
load(here("14_attempt/results/14a_var_select_fit_bt_asheville.rda"))
load(here("14_attempt/results/14a_var_select_fit_bt_chicago.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores - 1)

# examine important vars ----
var_select_bt_asheville <- var_select_fit_bt_asheville |>
  extract_fit_parsnip() |>
  vip::vi()

var_select_bt_chicago <- var_select_fit_bt_chicago |>
  extract_fit_parsnip() |>
  vip::vi()

var_select_bt_kauai <- var_select_fit_bt_kauai |>
  extract_fit_parsnip() |>
  vip::vi()

# recipes ----
# asheville
simple_recipe_asheville <- recipe(price ~ ., data = asheville_train) |>
  update_role(id, new_role = "ID") |>
  step_zv(all_predictors()) |>
  step_mutate_at(where(is.logical), fn = as.numeric) |>
  step_mutate_at(all_nominal_predictors(), fn = as.factor)

simple_recipe_asheville |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(simple_recipe_asheville) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

# chicago
simple_recipe_chicago <- recipe(price ~ ., data = chicago_train) |>
  update_role(id, new_role = "ID") |>
  step_zv(all_predictors()) |>
  step_mutate_at(where(is.logical), fn = as.numeric) |>
  step_mutate_at(all_nominal_predictors(), fn = as.factor)

simple_recipe_chicago |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(simple_recipe_chicago) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

# kauai
simple_recipe_kauai <- recipe(price ~ ., data = kauai_train) |>
  update_role(id, new_role = "ID") |>
  step_zv(all_predictors()) |>
  step_mutate_at(where(is.logical), fn = as.numeric) |>
  step_mutate_at(all_nominal_predictors(), fn = as.factor)

simple_recipe_kauai |>
  prep() |>
  bake(new_data = NULL) |> dim()

prep(simple_recipe_kauai) |>
  bake(new_data = NULL) |>
  select(contains("price")) |> 
  names()

save(simple_recipe_asheville, file = here("14_attempt/recipes/14a_recipe_asheville.rda"))
save(simple_recipe_chicago, file = here("14_attempt/recipes/14a_recipe_chicago.rda"))
save(simple_recipe_kauai, file = here("14_attempt/recipes/14a_recipe_kauai.rda"))
