generate_var_res_dataframe <- function(ts_data, p, q, r, s) {
  
  # Fit the ARMA(p, q) - GARCH(r, s) model to the time series
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(r, s)),
    mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  n <- length(ts_data)
  first_index <- 100  # +1 because we need to start in the next time step
  returns <- coredata(ts_data)
  
  # Initialize data frame
  df <- data.frame(
    Date = index(ts_data),
    Returns = returns
  )
  
  # Initialize columns for residuals and variances
  df$Variance <- rep(NA, n)
  df$Residuals <- rep(NA, n)
  df$Squared_Residuals <- rep(NA, n)
  
  # Initialize a progress bar
  pb <- txtProgressBar(min = first_index, max = n, style = 3)
  
  # When there are no lags available
  for (i in 2:first_index-1) {
    # Obtain variance when there are fewer data points available
    ts_window <- ts_data[1:(i-1)]
    variance_window <- var(coredata(ts_window))
    df$Variance[i] <- variance_window
  }
  
  # When all lags are available
  for (i in first_index:n) {
    ts_window <- ts_data[1:(i-1)]
    variance_window <- var(coredata(ts_window))
    df$Variance[i] <- variance_window  # Assign variance from the model
    
    fit <- tryCatch(
      ugarchfit(spec = spec, data = ts_window, solver = "hybrid"),
      error = function(e) NULL
    )
    
    # Only proceed if fit is successful
    if (!is.null(fit)) {
      residuals_fit <- residuals(fit)
      
      # Check if residuals were successfully extracted
      if (length(residuals_fit) > 0) {
        residuals2_fit <- residuals_fit[length(residuals_fit)]^2  # Take the last residual and square it
        
        # Assign values to columns
        df$Residuals[i] <- residuals_fit[length(residuals_fit)]  # Assign last residual
        df$Squared_Residuals[i] <- residuals2_fit  # Assign squared residual
      }
    } else {
      # Handle cases where the fit fails by keeping NA in the dataframe
      df$Residuals[i] <- NA
      df$Squared_Residuals[i] <- NA
    }
    
    # Update the progress bar after each iteration
    setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  
  return(df)
}

variables_df <- generate_var_res_dataframe(ts_ret, result$p, result$q, result$r, result$s)

generate_lagged_df <- function(df, p, q, r, s) {
  
  # Initialize a new dataframe with just the 'Date' column
  new_df <- data.frame(Date = df$Date, Returns = df$Returns)
  
  for (i in 1:p) {
    new_df[[paste0("Lag_Returns_", i)]] <- dplyr::lag(df$Returns, i)
  }
  
  # Add the 'Residuals' column and q lagged 'Residuals' columns
  new_df$Residuals <- df$Residuals
  for (i in 1:q) {
    new_df[[paste0("Lag_Residuals_", i)]] <- dplyr::lag(df$Residuals, i)
  }
  
  # Add the 'Variance' column and r lagged 'Variance' columns
  new_df$Variance <- df$Variance
  for (i in 1:r) {
    new_df[[paste0("Lag_Variance_", i)]] <- dplyr::lag(df$Variance, i)
  }
  
  # Add the 'Squared_Residuals' column and s lagged 'Squared_Residuals' columns
  new_df$Squared_Residuals <- df$Squared_Residuals
  for (i in 1:s) {
    new_df[[paste0("Lag_Squared_Residuals_", i)]] <- dplyr::lag(df$Squared_Residuals, i)
  }
  
  # Return the dataframe
  return(new_df)
}

# Example usage with a dataframe 'df'
output_df <- generate_lagged_df(variables_df, result$p, result$q, result$r, result$s)
