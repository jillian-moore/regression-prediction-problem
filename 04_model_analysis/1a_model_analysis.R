# L06 Model Tuning ----
# Analysis of tuned and trained models (comparisons)
# Select final model
# Fit & analyze final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = num_cores)

# set seed
set.seed(333)

# load data ----
load(here("data_split/reg_test.rda"))
load(here("data_split/reg_train.rda"))

# load necessary objects ----
load(here("results/1a_bt_tune.rda"))

# collect metrics ----
bt_tune |> 
  collect_metrics() |> 
  filter(.metric == "mae") |> 
  arrange(mean)

bt_tune |> 
  autoplot(metric = "mae")

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
  predicted = predictions$.pred
)

# save out ----
write_csv(bt_submit, here('data/1a_bt_submission.csv'))

# apply my metric set ----
evaluation_metrics <- bt_submit |> 
  my_metrics(truth = price, estimate = .pred)

# display
eval_metrics_tbl <- evaluation_metrics |> 
  select(.metric, .estimate) |> 
  rename(
    "Evaluation Metric" = .metric,
    "Estimate" = .estimate
  ) |> 
  knitr::kable(digits = 2)

# scatterplot
graphic_1 <- ggplot( bt_submit, aes(x = price, y = .pred)) +
  geom_point() +
  geom_abline(lty = 2) + 
  labs(
    title = "Actual vs. Predicted Price"
  ) +
  theme_bw() +
  coord_obs_pred()

# save out
ggsave(filename = here::here("figures/graphic_1.png"), plot = graphic_1)