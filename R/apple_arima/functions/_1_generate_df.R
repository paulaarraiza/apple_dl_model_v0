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
  df_ret$Return <- df_ret$Return*100
  
  # Step 5: Create time series objects
  ts_data <- zoo(df_ret$Return, order.by = df_ret$Date)
  
  # Step 6: Create regular date sequence for interpolation
  all_dates_ret <- seq(min(df_ret$Date), max(df_ret$Date), by = "day")
  
  # Step 7: Merge the time series with regular date sequence and interpolate missing values
  ts <- merge(ts_data, zoo(, all_dates_ret))
  ts <- na.approx(ts)  # Interpolate missing values for returns
  
  # Step 8: Create data frames for the time series (optional)
  df_full_ret <- data.frame(Date = index(ts), Return = coredata(ts))
  
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
  
  # Step 11: Plot ACF and PACF and save them to a file
  png("plots/aapl_acf_pacf.png", width = 800, height = 600)  # Open a PNG device to save the plot
  par(mfrow = c(1, 2))  # Plot ACF and PACF side by side
  acf(ts, main = "ACF of Returns")
  pacf(ts, main = "PACF of Returns")
  dev.off()  # Close the PNG device and save the plot
  # Return the interpolated returns time series
  return(ts)
}


# 1. Load data and obtain df
df <- read.csv("data/apple_2y.csv")
ts_ret <- generate_ts(df)

# Also separate train and test data
train_size <- 0.8
# We cannot estimate p, q, r, s orders looking at future values. Thus, we will only work with train data
n <- length(ts_ret)
train_index <- floor(train_size * n)
ts_train <- ts_ret[1:train_index]
ts_test <- ts_ret[(train_index + 1):n]

cat("Training on first", length(ts_train), "observations.\n")
cat("Testing on remaining", length(ts_test), "observations.\n")
