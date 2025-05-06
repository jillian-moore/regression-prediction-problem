# Train model
model <- catboost.train(train_pool, NULL, params = params)

# Predict
preds <- catboost.predict(model, test_pool)

# Evaluate
mae_score <- mean(abs(preds - test_data[[target_index]]))

# Fit the model
model <- catboost.train(train_pool, params = catboost_params)

# Make predictions
preds <- catboost.predict(model, test_pool)

# Return metrics
data.frame(
  rmse = sqrt(mean((test_data$price - preds)^2)),
  mae = mean(abs(test_data$price - preds)),
  rsq = cor(test_data$price, preds)^2,
  model = list(model)
)
}