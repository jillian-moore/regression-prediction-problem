# Attempt 04

Attempt 04 log-transforms variables based on exploratory data analysis, using boosted tree with LightGBM engine and Bayesian tuning.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01_initial_setup.R`: data splitting and exploration
-   `03_recipe.R`: feature engineering/recipe
-   `04_bt_tune.R`: tuning boosted tree model
-   `05_train_final_model.R`: training the best boosted tree model
