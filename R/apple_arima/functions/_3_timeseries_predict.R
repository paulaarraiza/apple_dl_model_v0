library(rugarch)
library(tseries)
library(ggplot2) 

# Function to split data and perform rolling predictions with re-fitting at each step
evaluate_garch_model <- function(ts_data, p, q, r, s, train_size = 0.8) {
  
  # 1. Split data into train (80%) and test (20%)
  n <- length(ts_data)
  train_index <- floor(train_size * n)
  
  ts_train <- ts_data[1:train_index]
  ts_test <- ts_data[(train_index + 1):n]
  
  cat("Training on first", length(ts_train), "observations.\n")
  cat("Testing on remaining", length(ts_test), "observations.\n")
  
  # 2. Define the GARCH model specification using the best (p, q) and (r, s) orders
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(r, s)),
    mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  # 3. Perform rolling predictions on the test set by re-fitting the model at each step
  rolling_predictions <- numeric(length(ts_test))
  
  for (i in seq_along(ts_test)) {
    # Incrementally increase the size of the training data with each new observation from the test set
    ts_train_window <- ts_data[1:(train_index + i-1)]
    
    # Fit the GARCH model to the updated training window
    fit <- tryCatch(
      ugarchfit(spec = spec, data = ts_train_window, solver = "hybrid"),
      error = function(e) NULL
    )
    
    # If the model was successfully fitted, predict the next value
    if (!is.null(fit)) {
      forecast <- ugarchforecast(fit, n.ahead = 1)
      rolling_predictions[i] <- fitted(forecast)
    } else {
      rolling_predictions[i] <- NA  # If fitting failed, return NA
    }
  }
  
  # 4. Evaluate the model using RMSE (or any other metric)
  actual_values <- ts_test
  rmse <- sqrt(mean((rolling_predictions - actual_values)^2, na.rm = TRUE))
  
  cat("Rolling RMSE: ", rmse, "\n")
  
  # Return the rolling predictions and RMSE
  return(list(predictions = rolling_predictions, rmse = rmse, train_index = train_index))
}

# Example usage
# Assuming ts_ret is your time series data and you have obtained p, q, r, s from your model
prediction_result <- evaluate_garch_model(ts_ret, p = result$p, q = result$q, r = result$r, s = result$s)

# View the rolling predictions and RMSE
prediction_result$predictions
prediction_result$rmse
prediction_result$train_index

test_length <- length(ts_ret) - prediction_result$train_index

# 1) Plot Actual vs Predicted Returns
df <- data.frame(
  Time = seq(1, test_length),
  Actual = coredata(tail(ts_ret, test_length)),
  Predicted = prediction_result$predictions
)

# Create your ggplot object and give it a different name
ggplot_obj <- ggplot(df, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed") +
  labs(title = "Actual vs Predicted Returns", x = "Time", y = "Returns") +
  theme_minimal() +
  scale_color_manual("", values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot using ggsave with the new object name
ggsave(filename = "plots/aapl_actual_vs_predicted.png", plot = ggplot_obj, width = 8, height = 6)