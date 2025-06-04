# Attempt 07

Attempt 07 uses the stacks package to build ensemble model with elastic net, random forest, support vector machine (SVM), boosted tree with xgboost engine, and boosted tree with LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_var_select_gbm.R`: variable selection with boosted tree
-   `02_var_select_lasso.R`: variable selection with elastic net
-   `03_recipe.R`: feature engineering/recipe
-   `04_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine
-   `04_bt_xg_tune.R`: tuning boosted tree with xgboost engine
-   `04_rf_tune.R`: fitting random forest model
-   `04_svm_tune.R`: tuning support vector machine (SVM) model
-   `04_en_tune.R`: tuning elastic net model
-   `05_train_ensemble_model.R`: training the ensemble model
-   `06_assess_final_model.R`: creating submission form

