library(forecast)
library(tseries)
library(lmtest)
library(rugarch)
library(FinTS)
library(ggplot2)

calculate_p_values <- function(arima_model) {
  coefs <- arima_model$coef
  std_errors <- sqrt(diag(vcov(arima_model)))
  t_values <- coefs / std_errors
  p_values <- 2 * (1 - pnorm(abs(t_values)))
  return(p_values)
}

select_best_arima_model <- function(ts, max_p, max_d, max_q, threshold = 0.7) {
  # Initialize a data frame to store results
  results <- data.frame(
    Model = character(),
    Proportion = numeric(),
    AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (p in 0:max_p) {
    for (d in 0:max_d) {
      for (q in 0:max_q) {
        model <- tryCatch({
          arima(ts, order = c(p, d, q), method = "CSS-ML")
        }, warning = function(w) {
          message("Warning: ", w)  
          return(NULL)
        }, error = function(e) {
          message("Error: ", e)  
          return(NULL)
        })
        
        if (is.null(model)) next
        
        p_values <- calculate_p_values(model)
        prop <- mean(p_values <= 0.05, na.rm = TRUE)
        aic_value <- AIC(model)
        arima_model_str <- paste0("ARIMA(", p, ",", d, ",", q, ")")
        cat("ARIMA(", p, ",", d, ",", q, ") - Proportion p-values:", prop, "- AIC:", aic_value, "\n")
        results <- rbind(
          results,
          data.frame(Model = arima_model_str, Proportion = prop, AIC = aic_value)
        )}}}
  filtered_results <- results %>% filter(Proportion > threshold)
  best_model <- filtered_results[which.min(filtered_results$AIC), ]
  list(
    results = results,
    best_model = best_model
  )
}


choose_best_garch <- function(ts, p, q, max_r, max_s, garch_threshold = 0.6) {
  
  # Inicializar variables para almacenar el mejor modelo y el AIC más bajo
  best_aic <- Inf
  best_model <- NULL
  best_r <- 0
  best_s <- 0
  
  # Iterar sobre las combinaciones de r (p para GARCH) y s (q para ARCH)
  for (r in 0:max_r) {
    for (s in 0:max_s) {
      # Definir la especificación del modelo GARCH-ARMA
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(r, s)),
        mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
        distribution.model = "norm"
      )
      
      # Ajustar el modelo GARCH-ARMA
      fit <- tryCatch({
        ugarchfit(spec = spec, data = ts, solver = "hybrid")
      }, error = function(e) {
        cat("Error en el ajuste para GARCH(", r, ",", s, "):", e$message, "\n")
        return(NULL)
      })
      
      # Si el ajuste fue exitoso, calcular los p-valores y el AIC
      if (!is.null(fit)) {
        fit_summary <- fit@fit$matcoef
        
        # Extraer coeficientes y errores estándar
        coefficients <- fit_summary[, 1]
        std_errors <- fit_summary[, 2]
        
        # Calcular valores Z y p-valores
        z_values <- coefficients / std_errors
        p_values <- 2 * (1 - pnorm(abs(z_values)))
        
        # Calcular la proporción de p-valores <= 0.05
        prop_pvalues <- mean(p_values <= 0.05)
        
        # Calcular el AIC
        aic_value <- infocriteria(fit)["Akaike", 1]
        
        # Filtrar por la proporción de p-valores mayor a 0.66
        if (prop_pvalues > garch_threshold) {
          # Comparar el AIC con el mejor AIC encontrado hasta ahora
          if (!is.na(aic_value) && aic_value < best_aic) {
            best_aic <- aic_value
            best_model <- fit
            best_r <- r
            best_s <- s
          }
        } 
        # Imprimir el progreso
        cat("GARCH(", r, ",", s, ") - Proporción p-valores:", prop_pvalues, "- AIC:", aic_value, "\n")
      }
    }
  }
  
  # Imprimir el mejor modelo encontrado
  if (!is.null(best_model)) {
    cat("Best GARCH(", best_r, ",", best_s, ") con AIC:", best_aic, "\n")
  } else {
    print("Threshold was too restrictive")
    }
  
  # Devolver el mejor modelo y su AIC
  return(list(best_model = best_model, best_aic = best_aic, best_r = best_r, best_s = best_s))
}

# Define the main function
analyze_time_series <- function(ts, max_p, max_d, max_q, max_r, max_s, garch_threshold = 0.6) {
  
  # 1. If stationary, continue. Plot ACF and PACF
  png("plots/acf_pacf_returns.png", width = 800, height = 400)
  par(mfrow = c(1, 2))  # Set plotting area to show ACF and PACF side by side
  acf(ts, main = "ACF of Returns", lag.max = 20)
  pacf(ts, main = "PACF of Returns", lag.max = 20)
  dev.off()
  
  # 2. Fit ARIMA model
  output <- select_best_arima_model(ts, max_p, max_d, max_q)
  best_model_text <- output$best_model["Model"]
  best_model_params <- scan(text = gsub("ARIMA\\(|\\)", "", best_model_text), sep = ",", quiet = TRUE) 
  best_p <- best_model_params[1]
  best_d <- best_model_params[2]
  best_q <- best_model_params[3]
  cat("Best ARIMA model: ARIMA(", best_p, ",", best_d, ",", best_q, ")\n")
  
  best_model <- arima(ts, order = c(best_p, best_d, best_q))
  # 3. Test for ARCH effects
  residuals <- residuals(best_model)
  arch_test <- capture.output(archTest(residuals))
  
  first_p_value <- as.numeric(sub(".*p-value: ", "", arch_test[grep("Rank-based Test", arch_test) + 1]))
  second_p_value <- as.numeric(sub(".*p-value: ", "", arch_test[grep("Rank-based Test", arch_test) + 1]))
  p_values <- c(first_p_value, second_p_value)
  
  if (any(p_values < 0.05)) {
    print("ARCH effects detected")
    
    # Plot ACF and PACF of squared residuals
    png("plots/acf_pacf_sq_residuals.png", width = 800, height = 400)
    par(mfrow = c(1, 2))
    acf(residuals^2, main = "ACF of Squared Residuals")
    pacf(residuals^2, main = "PACF of Squared Residuals")
    dev.off()
  } else {
    cat("No ARCH effects detected (p-value =", arch_test$p.value, ")\n")
  }
  
  # 4. Automatically detect best GARCH(p, q) orders
  garch_result <- choose_best_garch(ts, best_p, best_q, max_r, max_s, garch_threshold = 0.6)
  best_r <- garch_result$best_r
  best_s <- garch_result$best_s
  best_garch_model <- garch_result$best_model
  garch_residuals <- residuals(best_garch_model)
  
  cat("Best GARCH order: GARCH(", best_r, ",", best_s, ")\n")
  
  # 5. Fit GARCH model with detected orders to check normality and heteroskedasticity
  spec_garch <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(best_r, best_s)),
    mean.model = list(armaOrder = c(best_p, best_q), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  fit_garch <- ugarchfit(spec = spec_garch, data = ts)
  
  # Normality
  ks_result <- ks.test(garch_residuals, "pnorm", mean = mean(garch_residuals), sd = sd(garch_residuals))
  if (ks_result$p.value<0.05) {
    print("Residuals are not normal")
  } else {
    print("Residuals are normal")
  }
  
  # Heteroskedasticity
  bp_test <- bptest(garch_residuals ~ fitted(best_garch_model))
  if (bp_test$p.value < 0.05) {
    print("Residuals are heteroscedastic.")
  } else {
    print("Residuals are homoscedastic.")
  }
  
  # 8. Return the ARMA(p, q) and GARCH(r, s) parameters
  return(list(p = best_p, q = best_q, r = best_r, s = best_s))
}

