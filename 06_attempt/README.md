# Attempt 06

Attempt 06 uses the stacks package to build an ensemble model with linear regression, random forest, and boosted tree with LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_var_select.R`: variable selection with boosted tree
-   `03_recipe.R`: feature engineering/recipe
-   `04_bt_tune.R`: tuning boosted tree model
-   `04_linear_reg.R`: fitting linear regression model
-   `04_rf_tune.R`: tuning random forest model
-   `05_train_ensemble_model.R`: training the ensemble model
-   `05_train_final_model.R`: training the best boosted tree model

