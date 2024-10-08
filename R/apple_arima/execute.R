# Working dir
setwd("/Users/paulaarraizaarias/Documents/ucm/tfg/matematicas/git/R/apple_arima/")

# Load necessary files
function_files <- list.files(path = "functions", pattern = "\\.R$", full.names = TRUE)
sapply(function_files, source)

# 1. Load data and obtain df
df <- read.csv("data/apple_2y.csv")
ts_ret <- generate_ts(df)

# Also separate train and test data
train_size <- 0.8
n <- length(ts_ret)
train_index <- floor(train_size * n)
ts_train <- ts_ret[1:train_index]
ts_test <- ts_ret[(train_index + 1):n]

cat("Training on first", length(ts_train), "observations.\n")
cat("Testing on remaining", length(ts_test), "observations.\n")

# Check that there are no NA values
if (any(is.na(ts_ret))) {
  print("NA values in df")
} else 
  {
  adf_result <- adf.test(ts_train)  # Now, check that returns are stationary
  adf_pval <- adf_result$p.value
  if (adf_pval>0.05) {
    print("Returns are not stationary")
  } else {
    arima_result <- analyze_time_series(ts_train, max_p=3, max_d=0, max_q=3, max_r=3, max_s=3, garch_threshold = 0.6)
    best_p <- arima_result$p
    best_q <- arima_result$q
    best_r <- arima_result$r
    best_s <- arima_result$s
    
    # Now, start predicting
    # Assuming ts_ret is your time series data and you have obtained p, q, r, s from your model
    prediction_result <- evaluate_garch_model(ts_ret, p = best_p, q = best_q, r = best_r, s = best_s)
    test_length <- length(ts_ret) - prediction_result$train_index
    
    # Plot Actual vs Predicted Returns
    df <- data.frame(
      Time = seq(1, test_length),
      Actual = coredata(tail(ts_ret, test_length)),
      Predicted = prediction_result$predictions
    )
    ggplot_obj <- ggplot(df, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed") +
      labs(title = "Actual vs Predicted Returns", x = "Time", y = "Returns") +
      theme_minimal() +
      scale_color_manual("", values = c("Actual" = "blue", "Predicted" = "red"))
    ggsave(filename = "plots/actual_vs_predicted.png", plot = ggplot_obj, width = 8, height = 6)
  
    # Save accuracy df
    accuracy <- evaluate_direction_accuracy(ts_ret, prediction_result$predictions, prediction_result$train_index, 
                                            p = best_p, q = best_q, r = best_r, s = best_s)
    filename <- paste0("predictions/apple_garch_p", best_p, "_q", best_q, "_r", best_r, "_s", best_s, ".csv")
    write.csv(accuracy$df, filename, row.names = FALSE)
    
    # Generate output df
    variables_df <- generate_var_res_dataframe(ts_ret, best_p, best_q, best_r, best_s)
    output_df <- generate_lagged_df(variables_df, best_p, best_q, best_r, best_s)
    write.csv(output_df, "output/apple_garch.csv", row.names = FALSE)
    
  }
  
}

