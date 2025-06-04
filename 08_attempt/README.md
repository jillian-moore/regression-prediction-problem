# Attempt 08

Attempt 08 cross-references boosted tree variable selection with lasso variable selection, and tunes SVM and boosted tree with LightGBM engine models.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_var_select_gbm.R`: variable selection with boosted tree
-   `02_var_select_lasso.R`: variable selection with elastic net
-   `03_recipe.R`: feature engineering/recipe
-   `04_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine
-   `04_svm_tune.R`: tuning support vector machine (SVM) model
-   `05_train_final_svm.R`: training the best SVM model
-   `05_train_final_bt_gbm_model.R`: training the best boosted tree with LightGBM engine model
