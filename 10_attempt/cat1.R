# If not already installed, get the 'remotes' package
install.packages("remotes")

# Then install treeSnip from GitHub
remotes::install_github("curso-r/treesnip")

library(treeSnip)

# Get our custom CatBoost integration
source(here("10_attempt/cat2.R"))
load(here("data_split/reg_train.rda"))

# Set seed
set.seed(8957)

# Parallel processing ----
plan(multisession, workers = parallel::detectCores(logical = TRUE) - 1)

catboost_spec <- catboost_reg(trees = 500, learn_rate = 0.05) %>%
  set_engine("catboost") %>%
  set_mode("regression")

# Workflow for tuning
catboost_wflow <- workflow() %>% 
  add_model(catboost_spec) %>% 
  add_recipe(recipe_filtered)

# Tune/fit workflow/model ----
tic.clearlog()
tic("catboost_tune")

# Add error handling for the tuning process
catboost_tune <- tryCatch({
  tune_grid(
    catboost_wflow,
    resamples = reg_folds,
    grid = catboost_grid,  # Fixed variable name
    metrics = metric_set(mae, rmse),  # Start with basic metrics
    control = control_grid(
      save_pred = TRUE,
      parallel_over = "everything",  # Enable parallel processing
      verbose = TRUE,  # Add verbose output for debugging
      allow_par = TRUE,
      extract = NULL  # Don't extract additional info to avoid issues
    )
  )
}, error = function(e) {
  cat("Tuning failed with error:", e$message, "\n")
  # Save the error for debugging
  assign(".Last.tune.error", e, envir = .GlobalEnv)
  stop("Tuning process failed. Check .Last.tune.error for details.")
})

toc(log = TRUE)

# Extract timing information
time_log <- tic.log(format = FALSE)
catboost_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

cat("Best CatBoost results:\n")
show_best(catboost_tune, metric = "mae", n = 5)

# Select best parameters
best_catboost <- select_best(catboost_tune, metric = "mae")
cat("\nBest parameters:\n")
print(best_catboost)

# Finalize workflow with best parameters
final_catboost_wflow <- finalize_workflow(catboost_wflow, best_catboost)

