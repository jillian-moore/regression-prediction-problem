# Load required packages
library(parsnip)
library(catboost)
library(rlang)
library(hardhat)

# 1. Register the model type with parsnip
set_new_model("catboost_reg")
set_model_mode(model = "catboost_reg", mode = "regression")
set_model_engine("catboost_reg", mode = "regression", eng = "catboost")
set_dependency("catboost_reg", eng = "catboost", pkg = "catboost")

# 2. Set fit and prediction bridges
set_fit(
  model = "catboost_reg",
  eng = "catboost",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun = "catboost_fit_impl"),
    defaults = list()
  )
)

set_pred(
  model = "catboost_reg",
  eng = "catboost",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = function(results, object) as.numeric(results),
    func = c(fun = "catboost_predict_impl"),
    args = list(
      object = expr(object$fit),
      new_data = expr(new_data)
    )
  )
)

# 3. Model spec constructor
catboost_reg <- function(mode = "regression", trees = NULL, tree_depth = NULL, learn_rate = NULL, l2_leaf_reg = NULL, loss_function = NULL) {
  args <- list(
    iterations     = rlang::enquo(trees),
    depth          = rlang::enquo(tree_depth),
    learning_rate  = rlang::enquo(learn_rate),
    l2_leaf_reg    = rlang::enquo(l2_leaf_reg),
    loss_function  = rlang::enquo(loss_function)
  )
  
  parsnip::new_model_spec(
    "catboost_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = "catboost"
  )
}

# 4. Translate args for tuning
set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "trees",
  original = "iterations",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = FALSE
)

set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "tree_depth",
  original = "depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  has_submodel = FALSE
)

set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "learn_rate",
  original = "learning_rate",
  func = list(pkg = "dials", fun = "learn_rate"),
  has_submodel = FALSE
)

set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "l2_leaf_reg",
  original = "l2_leaf_reg",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = FALSE
)

# Optional
set_model_arg(
  model = "catboost_reg",
  eng = "catboost",
  parsnip = "loss_function",
  original = "loss_function",
  func = list(pkg = "dials", fun = "value_set"),
  has_submodel = FALSE
)

# 5. Custom fitting function
catboost_fit_impl <- function(x, y, ...) {
  x_mat <- hardhat::forge(x, hardhat::default_formula_env())[["predictors"]]
  pool <- catboost.load_pool(data = as.matrix(x_mat), label = y)
  
  args <- list(...)
  args$data <- pool
  
  model <- do.call(catboost.train, args)
  
  list(fit = model)
}

# 6. Custom prediction function
catboost_predict_impl <- function(object, new_data) {
  x_mat <- hardhat::forge(new_data, hardhat::default_formula_env())[["predictors"]]
  pool <- catboost.load_pool(data = as.matrix(x_mat))
  
  predict(object, pool)
}
