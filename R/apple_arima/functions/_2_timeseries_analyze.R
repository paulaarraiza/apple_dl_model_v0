library(forecast)
library(tseries)
library(lmtest)
library(rugarch)
library(FinTS)
library(ggplot2)

find_best_garch_order <- function(ts_data, arma_p, arma_q, max_p = 5, max_q = 5) {
  best_aic <- Inf
  best_order <- c(1, 1)
  
  for (p in 0:max_p) {
    for (q in 0:max_q) {
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
        mean.model = list(armaOrder = c(arma_p, arma_q), include.mean = TRUE),
        distribution.model = "norm"
      )
      fit <- tryCatch(
        ugarchfit(spec = spec, data = ts_data, solver = "hybrid"),
        error = function(e) NULL
      )
      
      if (!is.null(fit)) {
        aic_val <- infocriteria(fit)[1]  # Extract AIC value
        if (aic_val < best_aic) {
          best_aic <- aic_val
          best_order <- c(p, q)
        }
      }
    }
  }
  
  return(best_order)
}

# Define the main function
analyze_time_series <- function(ts_ret) {
  
  # 1. Check for stationarity (ADF test)
  adf_test <- adf.test(ts_ret)
  
  if (adf_test$p.value > 0.05) {
    print("Nonstationary time series")
    return()
  }
  
  # 2. If stationary, continue. Plot ACF and PACF
  par(mfrow = c(1, 2))  # Set plotting area to show ACF and PACF side by side
  acf(ts_ret, main = "ACF of Returns", lag.max = 20)
  pacf(ts_ret, main = "PACF of Returns", lag.max = 20)
  
  # 3. Fit ARMA model using auto.arima
  best_aic_model <- auto.arima(ts_ret)
  p_aic <- best_aic_model$arma[1]
  q_aic <- best_aic_model$arma[2]
  cat("Best ARMA model: ARMA(", p_aic, ",", q_aic, ")\n")
  
  # 4. Check if residuals are normal (Shapiro-Wilk test)
  residuals_aic <- residuals(best_aic_model)
  shapiro_test <- shapiro.test(residuals_aic)
  if (shapiro_test$p.value < 0.05) {
    cat("Residuals are not normally distributed (p-value =", shapiro_test$p.value, ")\n")
  } else {
    cat("Residuals are normally distributed (p-value =", shapiro_test$p.value, ")\n")
  }
  
  # 5. Check for homoscedasticity (Breusch-Pagan test)
  bp_test <- bptest(residuals_aic ~ fitted(best_aic_model))
  if (bp_test$p.value < 0.05) {
    cat("Residuals are heteroscedastic (Breusch-Pagan p-value =", bp_test$p.value, ")\n")
    
    # 6. Test for ARCH effects
    arch_test <- capture.output(archTest(residuals_aic))
    first_p_value <- as.numeric(sub(".*p-value: ", "", arch_test[grep("Rank-based Test", arch_test) + 1]))
    second_p_value <- as.numeric(sub(".*p-value: ", "", arch_test[grep("Rank-based Test", arch_test) + 1]))
    p_values <- c(first_p_value, second_p_value)
    if (any(p_values < 0.05)) {
      print("ARCH effects detected")
      
      # Plot ACF and PACF of squared residuals
      par(mfrow = c(1, 2))
      acf(residuals_aic^2, main = "ACF of Squared Residuals")
      pacf(residuals_aic^2, main = "PACF of Squared Residuals")
    } else {
      cat("No ARCH effects detected (p-value =", arch_test$p.value, ")\n")
    }
    
  } else {
    cat("Residuals are homoscedastic (Breusch-Pagan p-value =", bp_test$p.value, ")\n")
  }
  
  # 7. Automatically detect best GARCH(p, q) orders using AIC
  best_garch_order <- find_best_garch_order(ts_ret, p_aic, q_aic)
  cat("Best GARCH order: GARCH(", best_garch_order[1], ",", best_garch_order[2], ")\n")
  
  # Fit GARCH model with detected orders
  spec_garch <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = best_garch_order),
    mean.model = list(armaOrder = c(p_aic, q_aic), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  fit_garch <- ugarchfit(spec = spec_garch, data = ts_ret)
  
  # Output GARCH coefficients and p-values
  garch_coef <- coef(fit_garch)
  garch_pvals <- fit_garch@fit$robust.matcoef[, 4]  # Extract p-values
  cat("\nGARCH Model Coefficients and p-values:\n")
  print(data.frame(Coefficient = garch_coef, p_value = garch_pvals))
  
  # Check the Weighted Ljung-Box Test for lags
  ljung_box_res <- Box.test(residuals_aic, lag = 10, type = "Ljung-Box")$p.value
  ljung_box_res2 <- Box.test(residuals_aic^2, lag = 10, type = "Ljung-Box")$p.value
  
  if (ljung_box_res < 0.05 & ljung_box_res2 < 0.05) {
    cat("Significant autocorrelation in both residuals and squared residuals.\n")
  } else if (ljung_box_res < 0.05) {
    cat("Significant autocorrelation in residuals.\n")
  } else if (ljung_box_res2 < 0.05) {
    cat("Significant autocorrelation in squared residuals (GARCH effects).\n")
  } else {
    cat("No significant autocorrelation found.\n")
  }
  
  # 8. Return the ARMA(p, q) and GARCH(r, s) parameters
  return(list(p = p_aic, q = q_aic, r = best_garch_order[1], s = best_garch_order[2]))
}

train_size <- 0.8

# We cannot estimate p, q, r, s orders looking at future values. Thus, we will only work with train data
n <- length(ts_ret)
train_index <- floor(train_size * n)

ts_train <- ts_data[1:train_index]
ts_test <- ts_data[(train_index + 1):n]

cat("Training on first", length(ts_train), "observations.\n")
cat("Testing on remaining", length(ts_test), "observations.\n")

result <- analyze_time_series(ts_train)
