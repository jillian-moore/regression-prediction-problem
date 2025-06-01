# Regression AirBnB Problem ----
# LASSO Variable selection

# load packages
library(tidyverse)
library(tidymodels)
library(here)
library(future)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

# load data ----
load(here("data_split/reg_train.rda"))

# create resamples/folds ----
set.seed(9557)
lasso_folds <- reg_train |> 
  vfold_cv(v = 5, repeats = 3, strata = price)

# LASSO selection recipe ----
recipe_lasso <- recipe(price ~ ., data = reg_train) |>
  step_rm(id, amenities, amenities_clean, description, description_clean,
          host_about, host_about_clean) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_string2factor(all_nominal_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.001) |>
  step_unknown(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# check recipe
recipe_prepped <- recipe_lasso |> 
  prep()

recipe_baked <- recipe_prepped |> 
  bake(new_data = NULL)

# LASSO model specification ----
lasso_spec <- 
  linear_reg(
    penalty = tune(), 
    mixture = 1
    ) |>
  set_engine("glmnet") |>
  set_mode("regression")

# define workflow ----
lasso_wflow <- 
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(recipe_lasso)

# hyperparameter tuning values ----
lasso_tuned <- tune_grid(
  lasso_wflow,
  resamples = lasso_folds,
  grid = 15,
  metrics = metric_set(mae)
)

# show tuning results
show_best(lasso_tuned, metric = "mae", n = 5)

# extract best model (optimal tuning parameters) ----
best_lasso_params <- select_best(lasso_tuned, metric = "mae")
optimal_lasso_wflow <- lasso_wflow |> 
  finalize_workflow(best_lasso_params)

# fit best model/results ----
var_select_fit_lasso <- fit(optimal_lasso_wflow, reg_train)

# extract selected variables from LASSO ----
lasso_fit <- var_select_fit_lasso$fit$fit$fit
best_penalty <- best_lasso_params$penalty

# extract coefficients at best penalty
lasso_coefs <- coef(lasso_fit, s = best_penalty) |>
  as.matrix() |>
  as.data.frame() |>
  rownames_to_column("feature") |>
  rename(coefficient = s1) |>
  filter(coefficient != 0, feature != "(Intercept)") |>
  arrange(desc(abs(coefficient)))

# cross-validation performance summary
cv_metrics_lasso <- collect_metrics(lasso_tuned) |>
  filter(.metric == "mae") |>
  summarise(
    best_mae = min(mean),
    mean_mae = mean(mean),
    std_mae = sd(mean)
  )

# write out LASSO variable selection results ----
save(var_select_fit_lasso, lasso_coefs, file = here("09_attempt/results/9a_var_select_fit_lasso.rda"))

