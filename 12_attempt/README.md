# Attempt 12

Attempt 12 goes back to the basics with simple pre-processing and binary flags for missing information, using a boosted tree with LightGBM engine.

#### Directories

-   [recipes/](./recipes/): contains all recipes
-   [results/](./results/): contains .rda file and CVS file results

#### Files

-   `01a_initial_setup.R`: data splitting and exploration with self-written functions
-   `01b_initial_setup.R`: data splitting and exploration with thorough pre-processing from past attempts
-   `02_recipe.R`: feature engineering/recipe
-   `03a_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine with 01a pre-processing
-   `03b_bt_gbm_tune.R`: tuning boosted tree with LightGBM engine with 01b pre-processing
-   `04a_train_final_model.R`: training the best boosted tree with LightGBM engine model with 01a pre-processing
-   `04b_train_final_model.R`: training the best boosted tree with LightGBM engine model with 01b pre-processing
