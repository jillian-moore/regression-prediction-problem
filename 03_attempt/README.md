# Attempt 03

Attempt 03 adds self-selected amenity binary flags and addresses outliers, using a boosted tree model with a LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_var_select.R`: variable selection with boosted tree
-   `03_recipe.R`: feature engineering/recipe
-   `04_bt_tune.R`: tuning boosted tree model
-   `05_train_final_model.R`: training the best boosted tree model
