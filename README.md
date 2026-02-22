# Gold & Financial Market Econometric Analysis (R)

## Overview

This project conducts comprehensive financial time series and econometric analysis on Gold and related market indices using R. It integrates statistical testing, regression modeling, dimensionality reduction, seasonal decomposition, and ARIMA forecasting to evaluate price dynamics, market relationships, and predictive performance.

The project demonstrates applied econometrics, hypothesis testing, and time series modeling using real-world financial data.

---

## Key Features

- Price Elasticity Analysis (log-difference method)
- Granger Causality Testing
- Event Impact Analysis using Linear Regression
- Principal Component Analysis (PCA)
- Seasonal Decomposition of Time Series
- Multiple Linear Regression Modeling
- ARIMA Time Series Forecasting (Auto ARIMA)
- Model Evaluation using MAE, MSE, RMSE
- Chi-Square Goodness-of-Fit Testing
- Currency Conversion and Comparative Price Visualization
- Actual vs Predicted Model Comparison

---

## Technologies Used

- R Programming
- openxlsx (Excel data import)
- dplyr (Data manipulation)
- ggplot2 (Data visualization)
- forecast (ARIMA modeling and forecasting)
- lmtest (Granger causality testing)
- stats (Regression, PCA, decomposition)

---

## Project Structure

```
Gold-Market-Analysis/
│
├── FINAL_USO.xlsx        # Input dataset
├── analysis_script.R     # Main R analysis script
└── README.md             # Project documentation
```

---

## Installation and Setup

### 1. Install Required Packages

```r
install.packages(c("openxlsx", "ggplot2", "forecast", "lmtest", "dplyr"))
```

### 2. Update Dataset Path

Modify the file path inside the script:

```r
read.xlsx("path_to_your_file/FINAL_USO.xlsx")
```

### 3. Run the Script

```r
source("analysis_script.R")
```

---

## Analytical Components

### Price Elasticity
Calculates log-differenced elasticity between EU_Price and Volume to measure price responsiveness.

### Granger Causality Test
Evaluates whether SP_open statistically predicts EU_Price using lag-based causality testing.

### Event Impact Analysis
Uses regression modeling to assess the influence of a specified time-period event on gold prices.

### Principal Component Analysis (PCA)
Reduces dimensionality of correlated financial indices (SP_open, DJ_open, EG_open, etc.) to identify dominant variance components.

### Seasonal Decomposition
Decomposes the gold price time series into trend, seasonal, and irregular components.

### Regression Modeling
Builds a linear regression model to predict closing prices using Open, High, Low, and Volume.

### ARIMA Forecasting
Applies auto.arima() to generate short-term forecasts and compares time series predictions with regression outputs.

---

## Evaluation Metrics

- Mean Absolute Error (MAE)
- Mean Squared Error (MSE)
- Root Mean Squared Error (RMSE)
- Chi-Square Goodness-of-Fit
- Visual model comparison plots

---

## Outputs

- Statistical test summaries in console
- PCA scree plot
- Seasonal decomposition plots
- EU vs USD price comparison chart
- Linear regression actual vs predicted scatter plot
- ARIMA forecast visualization
- Combined comparison of actual vs regression vs ARIMA predictions

---

## Objectives

- Analyze gold price behavior and market interdependencies
- Test statistical causality between financial indices
- Evaluate predictive accuracy of regression and ARIMA models
- Identify structural, seasonal, and event-driven patterns
- Compare econometric and time series forecasting approaches
