# Data Split

This directory is continually updated each time initial setup scripts are run. This is because I run all pre-processing directly through the original data, rather than a training script with basic cleaning, to ensure total reproducibility and give me more freedom to adjust pre-processing.

#### Files
-   `reg_folds.R`: v-fold cross validation information
-   `reg_train.R`: pre-processed training data
-   `reg_test.R`: pre-processed testing data
-   `my_metrics.R`: the metric set (contains MAE only in most cases)
-   `reg_folds_mini.R`: mini v-fold cross-validation to check my tuning scripts quickly
-   `reg_mini.R`: mini regification data to to run my workflow quickly
