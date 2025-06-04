# Attempt 11

Attempt 11 goes back to the basics with simple pre-processing and binary flags for missing information, using boosted tree with LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `02_recipe.R`: feature engineering/recipe
-   `03_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine
-   `05_train_final_model.R`: training the best boosted tree with LightGBM engine model
