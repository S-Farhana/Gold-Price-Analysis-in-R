
library(openxlsx)
library(ggplot2)
library(forecast)  
library(lmtest)
library(dplyr)


load_data <- function() {
  data <- read.xlsx("C:\\Users\\nacha\\Downloads\\FINAL_USO (1).xlsx")
  data$Date <- as.Date(data$Date, format="%Y-%m-%d")
  return(data)
}


cal_price_elasticity <- function(data) {
  data <- data %>%
    mutate(elasticity = c(NA, diff(log(EU_Price)) / diff(log(Volume))))
  mean_elasticity <- mean(data$elasticity, na.rm = TRUE)
  
  
  cat("\n\nH0: No significant relationship between EU_Price and Volume (Price elasticity < 1).")
  cat("\nHa: Significant relationship between EU_Price and Volume (Price elasticity > 1).")
  
  cat("\n\nMean elasticity: ", mean_elasticity)
  
 
  if (abs(mean_elasticity) > 0.1) {
    cat("\n\nConclusion: There is a significant relationship between EU_Price and Volume.\n")
  } else {
    cat("\n\nConclusion: There is no significant relationship between EU_Price and Volume.\n")
  }
  
  return(mean_elasticity)
}


Granger_test <- function(data, order = 2) {
  granger_test <- grangertest(EU_Price ~ SP_open, order = order, data = data)
  
 
  cat("\n\nH0: SP_open does not Granger-cause EU_Price.")
  cat("\nHa: SP_open Granger-causes EU_Price.")
  
  cat("\n\nGranger Causality Test Results:\n")
  print(granger_test)
  
 
  if (granger_test$`Pr(>F)`[2] < 0.05) {
    cat("\n\nConclusion: SP_open Granger-causes EU_Price (p-value < 0.05).\n")
  } else {
    cat("\n\nConclusion: SP_open does not Granger-cause EU_Price (p-value >= 0.05).\n")
  }
}


analyze_event_impact <- function(data, start_date, end_date) {
  data$event <- ifelse(data$Date >= as.Date(start_date) & data$Date <= as.Date(end_date), 1, 0)
  event_impact <- lm(EU_Price ~ event, data = data)
  coefficients <- coef(event_impact)
  
  
  cat("\n\nH0: The major event has no impact on EU_Price.")
  cat("\nHa: The major event impacts EU_Price.")
  
  cat("\n----------------------------")
  cat("\n   Impact of Major Event")
  cat("\n----------------------------")
  cat("\n Intercept  : ", coefficients[1])
  cat("\n Slope of x : ", coefficients[2])
  cat("\n----------------------------")
  
 
  if (coefficients[1] != 0) {
    cat("\n\nConclusion: The major event has a significant impact on EU_Price.\n")
  } else {
    cat("\n\nConclusion: The major event does not significantly impact EU_Price.\n")
  }
}


PCA <- function(data) {
  pca_columns <- data %>% select(SP_open, DJ_open, EG_open, OF_Open, SF_Open)
  pca_model <- prcomp(pca_columns, scale = TRUE)
  
  
  cat("\n\nH0: No significant variance is explained by the principal components.")
  cat("\nHa: Significant variance is explained by the principal components.")
  
  cat("\n\nPCA Summary (standard deviations):\n")
  print(pca_model$sdev)
  plot(pca_model)
  
  
  if (pca_model$sdev[1] > 1) {
    cat("\n\nConclusion: The first principal component explains a significant amount of variance.\n")
  } else {
    cat("\n\nConclusion: The principal components do not explain significant variance.\n")
  }
}


seasonal_decomposition <- function(data) {
  gold_ts <- ts(data$EU_Price, frequency = 12)
  seasonal_decomp <- decompose(gold_ts)
  plot(seasonal_decomp)
  
 
  cat("\n\nH0: No significant seasonal pattern in EU_Price.")
  cat("\nHa: Significant seasonal pattern in EU_Price.")
  

  if (max(abs(seasonal_decomp$seasonal)) > 0.05) {
    cat("\n\nConclusion: There is a significant seasonal pattern in EU_Price.\n")
  } else {
    cat("\n\nConclusion: There is no significant seasonal pattern in EU_Price.\n")
  }
}


calculate_error_metrics <- function(actual, model, predicted) {
  MAE <- mean(abs(actual - predicted), na.rm = TRUE)  # Mean absolute error
  MSE <- mean((actual - predicted)^2, na.rm = TRUE)   # Mean squared error
  RMSE <- sqrt(MSE)  # Root mean squared error
  coefficients <- coef(model)
  
  
  cat("\n\nH0: The model does not predict EU_Price accurately.")
  cat("\nHa: The model predicts EU_Price accurately.")
  
  cat("\n-----------------------------------")
  cat("\n\t Error metrics")
  cat("\n-----------------------------------")
  cat("\n Intercept : ", coefficients[1])
  cat("\n Open      : ", coefficients[2])
  cat("\n High      : ", coefficients[3])
  cat("\n Low       : ", coefficients[4])
  cat("\n Volume    : ", coefficients[5])
  cat("\n\n Mean absolute error : ", MAE)
  cat("\n Mean squared error  : ", MSE)
  cat("\n Root mean squared error:", RMSE)
  cat("\n-----------------------------------\n")
  
 
  if (RMSE < 0.05 * mean(actual, na.rm = TRUE)) {
    cat("\n\nConclusion: The model predicts EU_Price accurately.\n")
  } else {
    cat("\n\nConclusion: The model does not predict EU_Price accurately.\n")
  }
}


goodness_of_fit_test <- function(predicted) {
  chisq_test <- chisq.test(predicted)
  
 
  cat("\n\n\nH0: The predicted values follow the expected distribution.")
  cat("\nHa: The predicted values do not follow the expected distribution.")
  
  cat("\n-------------------------------")
  cat("\n  Goodness-of-Fit Test Results ")
  cat("\n-------------------------------")
  cat("\n Chi-square value : ", chisq_test$statistic)
  cat("\n Degrees of freedom : ", chisq_test$parameter)
  cat("\n P-value : ", chisq_test$p.value)
  cat("\n-------------------------------")
  
  
  if (chisq_test$p.value < 0.05) {
    cat("\n\nConclusion: The predicted values do not follow the expected distribution (p-value < 0.05).\n\n")
  } else {
    cat("\n\nConclusion: The predicted values follow the expected distribution (p-value >= 0.05).\n\n")
  }
}


add_usd_price <- function(data, exchange_rate_column = "Exchange_Rate") {
  if (!exchange_rate_column %in% colnames(data)) {
    stop("Exchange rate column not found in the dataset.")
  }
  data <- data %>% mutate(USD_Price = EU_Price * get(exchange_rate_column))
  return(data)
}


plot_eu_vs_usd <- function(data) {
  if (!"USD_Price" %in% colnames(data)) {
    stop("USD_Price column not found in the dataset.")
  }
  
  plot(data$Date, data$EU_Price, type = "l", col = "blue", lwd = 2, 
       xlab = "Date", ylab = "Price", main = "EU Price vs USD Price Over Time")
  lines(data$Date, data$USD_Price, col = "red", lwd = 2)
  
  legend("topright", legend = c("EU_Price", "USD_Price"), col = c("blue", "red"), lty = 1, lwd = 2)
}



gold_data <- load_data()
# To calculate price elasticity
mean_elasticity <- cal_price_elasticity(gold_data)

cat("-------")


granger_test_result <- Granger_test(gold_data)

cat("-------")


event_impact <- analyze_event_impact(gold_data, '2011-09-01', '2018-09-01')

cat("-------")


pca_model <- PCA(gold_data)

cat("-------")


seasonal_decomposition(gold_data)

cat("-------")


regression_model <- lm(Close ~ Open + High + Low + Volume, data = gold_data)
predicted <- predict(regression_model)
error_metrics <- calculate_error_metrics(gold_data$EU_Price, regression_model, predicted)

cat("-------")


goodness_of_fit_test(predicted)

cat("-------")


gold_data <- gold_data %>%
  mutate(Exchange_Rate = 1.1)  

gold_data <- add_usd_price(gold_data, "Exchange_Rate")

cat("-------")


plot_eu_vs_usd(gold_data)


target_variable <- "GDX_Close"
formula <- as.formula(paste(target_variable, "~ ."))
linear_model <- lm(formula, data = gold_data)

cat("\n--- Linear Model Summary ---\n")
print(summary(linear_model))


gold_data$Predicted_GDX_Close <- predict(linear_model, gold_data)
linear_plot <- ggplot(gold_data, aes(x = GDX_Close, y = Predicted_GDX_Close)) +
  geom_point(color = 'blue', size = 2) +  
  geom_abline(intercept = 0, slope = 1, color = 'red', size = 1) +  
  labs(title = "Actual vs Predicted GDX Closing Prices (Linear Model)", 
       x = "Actual GDX_Close", y = "Predicted GDX_Close") +
  theme_minimal()

print(linear_plot)


gdx_ts <- ts(gold_data$GDX_Close, frequency = 252)  
arima_model <- auto.arima(gdx_ts)
cat("\n--- ARIMA Model Summary ---\n")
print(summary(arima_model))

forecast_horizon <- 30
gdx_forecast <- forecast(arima_model, h = forecast_horizon)
print(gdx_forecast)

arima_plot <- autoplot(gdx_forecast) +
  autolayer(gdx_ts, series = "Historical Data", PI = FALSE) +
  labs(title = "GDX_Close Forecast (ARIMA)", x = "Time", y = "GDX Close Price") +
  theme_minimal()

print(arima_plot)


arima_forecast_values <- as.numeric(gdx_forecast$mean)
comparison_plot <- ggplot(gold_data) +
  geom_line(aes(x = seq_along(GDX_Close), y = GDX_Close, color = "Actual GDX_Close")) +
  geom_line(aes(x = seq_along(GDX_Close), y = Predicted_GDX_Close, color = "Linear Regression Prediction")) +
  geom_line(data = data.frame(seq = (nrow(gold_data) - forecast_horizon + 1):nrow(gold_data),
                              ARIMA_Forecast = arima_forecast_values),
            aes(x = seq, y = ARIMA_Forecast, color = "ARIMA Forecast")) +
  labs(title = "Comparison of Actual vs Predicted Values: Linear vs ARIMA",
       x = "Time", y = "GDX Close Price", color = "Legend") +
  scale_color_manual(values = c("Actual GDX_Close" = "black",
                                "Linear Regression Prediction" = "yellow",
                                "ARIMA Forecast" = "red")) + 
  theme_minimal()

print(comparison_plot)
