# Regression Prediction Problem

This repo is for a regression prediction problem using Airbnb data, completed for STAT 301-3 (R track) at Northwestern University.

## Overview

I am aiming to predict Airbnb price. The data includes listing and host information for Airbnbs in Asheville, Chicago, and Kauai.

## What's in the repo

#### Executive Summary

-   `Moore_Jillian_executive_summary.html`: contains the HTML file of the Executive Summary
-   `Moore_Jillian_executive_summary.qmd`: contains the QMD file of the Executive Summary

#### Directories

-   [data/](./data/): contains all original data and codebooks for this lab
-   [data_split/](./data_split/): contains the training, testing, folds, and metrics data after pre-processing (continually updated with attempts)
-   [01_attempt/](./01_attempt/): implementing self-written functions and in-class suggestions; boosted tree with LightGBM engine
-   [02_attempt/](./02_attempt/): trying again with only in-class suggestions, no self-written functions; boosted tree with LightGBM engine
-   [03_attempt/](./03_attempt/): adding self-selected amenity binary flags and addressing outliers; boosted tree with LightGBM engine
-   [04_attempt/](./04_attempt/): log-transforming variables based on exploratory data analysis; boosted tree with LightGBM engine; Bayesian tuning
-   [05_attempt/](./05_attempt/): using boosted tree variable selection; boosted tree with LightGBM engine; Bayesian tuning
-   [06_attempt/](./06_attempt/): using stacks package to build ensemble model with linear regression, random forest, and boosted tree with LightGBM engine
-   [07_attempt/](./07_attempt/): using stacks package to build ensemble model with elastic net, random forest, support vector machine (SVM), boosted tree with xgboost engine, and boosted tree with LightGBM engine
-   [08_attempt/](./08_attempt/): cross-referencing boosted tree variable selection with lasso variable selection; SVM and boosted tree with LightGBM engine
-   [09_attempt/](./09_attempt/): additional comparison for boosted tree variable selection with lasso variable selection; ensemble model with SVM, boosted tree with xgboost engine, and boosted tree with LightGBM engine
-   [10_attempt/](./10_attempt/): adding additional variables (especially interaction terms and calculated variables) in pre-processing; ensemble model with SVM, boosted tree with xgboost engine, and boosted tree with LightGBM engine
-   [11_attempt/](./11_attempt/): going back to the basics with simple pre-processing and binary flags for missing information; boosted tree with LightGBM engine
-   [12_attempt/](./12_attempt/): going back to the basics with simple pre-processing and binary flags for missing information; boosted tree with LightGBM engine
-   [13_attempt/](./13_attempt/): splitting simple pre-processed data into three groups by listing location; boosted tree with LightGBM engine
-   [14_attempt/](./14_attempt/): using separate variable selection to filter three groups of data by listing location; boosted tree with LightGBM engine

#### Other Files

-   `helper_functions.R`: contains pre-processing functions in initial setup R scripts
-   `eda.R`: exploratory data analysis
-   `check_for_big_objects.R`: checking my repo for large objects before committing and pushing
-   `sandbox.R`: notes and scratch work

## Considerations

All R scripts are reproducible for any possible missing results or information.

There are more submissions on Kaggle than attempt folders on this repo because for minor mistakes (e.g. forgetting to reverse the log-transformation of the target variable before submitting), I did not create another attempt folder. I also did not create a separate attempt folder for hyperparameter adjustments or *minor* pre-processing adjustments. These differences are labeled with letters (e.g. 1a and 1b).

The attempt folders therefore correspond to each unique method and might have more than one submission. However, all valid submissions are entirely reproducible.
