# Attempt 14

Attempt 14 uses separate variable selection to filter three groups of data by listing location and boosted tree with LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results
-   [data_split/](./data_split/): contains the training data split by listing location

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_var_select_bt.R`: variable selection with boosted tree
-   `02_recipe.R`: feature engineering/recipe
-   `03_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine
-   `05_train_final_model.R`: training the best boosted tree with LightGBM engine model
