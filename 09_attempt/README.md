# Attempt 09

Attempt 09 makes additional comparisons for boosted tree variable selection and lasso variable selection and builds an ensemble model with SVM, boosted tree with xgboost engine, and boosted tree with LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_var_select_gbm.R`: variable selection with boosted tree
-   `02_var_select_lasso.R`: variable selection with elastic net
-   `02_var_select_comparison.R`: variable selection comparison
-   `03_recipe.R`: feature engineering/recipe
-   `04_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine
-   `04_bt_xg_tune.R`: tuning boosted tree with xgboost engine
-   `05_train_bt_final_model.R`: training the best boosted tree with LightGBM engine model
-   `05_train_ensemble_svm.R`: training the ensemble model
-   `06_assess_final_model.R`: create submission using ensemble model
