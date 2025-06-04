# Attempt 13

Attempt 13 splits simple pre-processed data into three groups by listing location, using a boosted tree with LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results
-   [data_split/](./data_split/): contains the training data split by listing location

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_recipe.R`: feature engineering/recipe
-   `03_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine
-   `04_train_final_model.R`: training the best boosted tree with LightGBM engine model
