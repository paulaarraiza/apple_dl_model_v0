# Load necessary libraries
library(zoo)
library(tseries)
library(ggplot2)
library(dplyr)

generate_ts <- function(df) {
  
  # Step 1: Select the necessary columns and format the date
  df <- df[c("Date", "Close")]
  df$Date <- as.Date(df$Date)
  
  # Step 2: Generate df with returns instead of raw prices
  df_ret <- df %>%
    mutate(Return = (Close / lag(Close) - 1))
  
  # Step 3: Delete the first row (no return for first observation) and Price column
  df_ret <- df_ret[-1, ]  # Remove first row with NA in 'Return'
  
  # Step 4: Check for NA values
  if (any(is.na(df_ret))) {
    cat("NA values found in the data.\n")
  } else {
    cat("No NA values found.\n")
  }
  
  # Step 5: Create time series objects
  ts_ret_data <- zoo(df_ret$Return, order.by = df_ret$Date)
  
  # Step 6: Create regular date sequence for interpolation
  all_dates_ret <- seq(min(df_ret$Date), max(df_ret$Date), by = "day")
  
  # Step 7: Merge the time series with regular date sequence and interpolate missing values
  ts_ret <- merge(ts_ret_data, zoo(, all_dates_ret))
  ts_ret <- na.approx(ts_ret)  # Interpolate missing values for returns
  
  # Step 8: Create data frames for the time series (optional)
  df_full_ret <- data.frame(Date = index(ts_ret), Return = coredata(ts_ret))
  
  # Step 9: Plot the returns with ggplot2 and save the plot
  plot_returns <- ggplot(df_full_ret, aes(x = Date, y = Return)) +
    geom_line(color = "blue") +
    geom_hline(aes(yintercept = mean(Return)), linetype = "dashed", color = "red") +
    labs(title = "AAPL Daily Returns", x = "Date", y = "Return") +
    theme_minimal() + 
    theme(
      panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
      plot.background = element_rect(fill = "white", color = NA),   # Set plot background to white
      panel.grid.major = element_line(color = "grey90"),            # Optional: customize grid lines
      panel.grid.minor = element_blank() 
    )
    
  # Save the ggplot plot
  ggsave("plots/aapl_daily_returns.png", plot = plot_returns, width = 8, height = 6)
  
  # Step 10: Check for stationarity using Augmented Dickey-Fuller Test
  adf_result <- adf.test(ts_ret)
  
  if (adf_result$p.value < 0.05) {
    cat("Time series is stationary (p-value =", adf_result$p.value, ").\n")
  } else {
    cat("Time series is not stationary (p-value =", adf_result$p.value, ").\n")
  }
  
  # Step 11: Plot ACF and PACF and save them to a file
  png("plots/aapl_acf_pacf.png", width = 800, height = 600)  # Open a PNG device to save the plot
  par(mfrow = c(1, 2))  # Plot ACF and PACF side by side
  acf(ts_ret, main = "ACF of Returns")
  pacf(ts_ret, main = "PACF of Returns")
  dev.off()  # Close the PNG device and save the plot
  # Return the interpolated returns time series
  return(ts_ret)
}

# Example usage
df <- read.csv("apple_2y.csv")
ts_ret <- generate_ts(df)
