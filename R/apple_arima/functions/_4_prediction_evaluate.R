
evaluate_direction_accuracy <- function(ts_data, predictions, train_index, p, q, r, s) {
  
  test_length <- length(ts_data) - prediction_result$train_index
  realized_values <- tail(ts_data, test_length)
  
  # Ensure both inputs are numeric vectors and of the same length
  if (length(predictions) != length(realized_values)) {
    stop("Predictions and actual values must have the same length.")
  }
  
  # Create a data frame with actual, predicted, and a column 'Correct'
  df <- data.frame(
    Realized = realized_values,
    Predicted = predictions,
    Correct = ifelse(realized_values*predictions >= 0, 1, 0)  # Assign 1 if both are > 0, else 0
  )
  
  # Calculate the proportion of correct predictions (sum of 1's divided by the number of rows)
  accuracy <- sum(df$Correct) / nrow(df)
  cat("Proportion of correct direction predictions:", accuracy, "\n")
  
  # Return the accuracy
  return(list(df = df, accuracy = accuracy))
}

# Example usage:
# Assuming prediction_result$predictions contains your predicted values
# and prediction_result$actual contains the actual stock prices

# Assuming ts_ret is your time series data and you have obtained p, q, r, s from your model
# prediction_result <- evaluate_garch_model(ts_ret, p = best_p, q = best_q, r = best_r, s = best_s)

# View the rolling predictions and RMSE
# prediction_result$predictions
# prediction_result$rmse
# prediction_result$train_index

# test_length <- length(ts_ret) - prediction_result$train_index

