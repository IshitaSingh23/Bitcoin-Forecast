# 📈 Ethereum Price & Volatility Forecasting  
**STAT 429 – Time Series Analysis**  

**Author:** Ishita Singh  
**Instructor:** Prof. Hyoeun Lee  
**University:** University of Illinois Urbana-Champaign  

---

## 🔍 Project Overview

Cryptocurrency markets are highly volatile and operate 24/7. Even short-term forecasts can help traders, analysts, and risk managers plan entries and exits, manage risk, and understand uncertainty.

This project builds a **statistically sound and explainable time-series forecasting pipeline** to answer two core questions:

1. Can we forecast Ethereum’s daily closing price for the next 7 days?
2. Can we quantify and forecast the volatility (risk) around those prices?

To address these, Ethereum prices are modeled using **ARIMA**, **SARIMA**, **SARIMAX**, and **GARCH** frameworks, with evaluation based on both fixed and rolling forecast windows.

---

## 🎯 Objectives

- Clean and transform daily ETH-USD price data  
- Enforce stationarity using log transformations and differencing  
- Build and compare:
  - ARIMA (trend)
  - SARIMA (trend + weekly seasonality)
  - SARIMAX (seasonality + exogenous lag)
  - GARCH (time-varying volatility)
- Evaluate forecast accuracy using **7-day ahead rolling windows**
- Interpret both **price predictability** and **volatility clustering**

---

## 📊 Dataset

- **Source:** Investing.com  
- **Frequency:** Daily  
- **Observations:** ~1,850 days  

### Key Variables
- `price` – Daily closing price (USD)  
- `open`, `high`, `low` – Daily OHLC prices  

### Derived Series
- `log_price`
- `d_log_price` (first difference of log price)
- `log_return` (used for GARCH modeling)

The dataset spans multiple market regimes, including bull runs, crashes, and recovery periods.

---

## 🧠 Methodology

### 1. Data Preprocessing
- Cleaned column names and numeric formats  
- Parsed and ordered dates chronologically  
- Applied log transformation and differencing  
- Verified stationarity using Augmented Dickey–Fuller (ADF) tests  

---

### 2. ARIMA Modeling
- Fitted multiple ARIMA(p,1,q) candidates  
- Selected **ARIMA(4,1,4)** based on AIC and residual diagnostics  
- Residuals behaved like white noise  
- Produced 7-day ahead forecasts with confidence intervals  

---

### 3. SARIMA (Seasonal ARIMA)
- Captured weekly seasonality (period = 7)  
- Best model: **SARIMA(5,1,5) × (1,0,1)\_7**  
- Outperformed ARIMA based on:
  - Lower AIC
  - Likelihood Ratio Test
  - Lower forecast error  

---

### 4. Rolling Window Evaluation
- Used a rolling **7-day forecast window**
- Computed RMSE for each window
- Observed:
  - Stable performance during calm periods
  - Large error spikes during volatile periods  
- Motivated explicit volatility modeling

---

### 5. GARCH Volatility Modeling
- Fitted **GARCH(1,1)** on log returns  
- Found strong volatility persistence (α + β ≈ 0.99)  
- Effectively captured volatility clustering  
- Useful for **risk forecasting**, not price level prediction  

---

### 6. SARIMAX Extension
- Added lag-1 log price as an exogenous regressor  
- Performance similar to SARIMA  
- Extra regressor did not add significant predictive power  

---

## 📈 Results Summary

| Model | Purpose | Performance |
|------|--------|------------|
| ARIMA(4,1,4) | Price forecasting | RMSE ≈ 119 USD |
| SARIMA(5,1,5)(1,0,1)\_7 | Price + seasonality | RMSE ≈ 105 USD |
| SARIMAX | Price + exogenous lag | Comparable to SARIMA |
| GARCH(1,1) | Volatility forecasting | Captures risk dynamics |

**Key takeaway:**  
- **SARIMA** provides the best short-term price forecasts  
- **GARCH** adds critical insight into time-varying market risk  

---

## 🧩 Key Insights

- Ethereum prices show weak mean dynamics but strong volatility clustering  
- Forecast accuracy varies significantly over time  
- Weekly seasonality improves short-term forecasts  
- Volatility modeling is essential for understanding market risk  

---

## 🛠️ Tech Stack

- **Language:** R  
- **Libraries:** `forecast`, `tseries`, `rugarch`, `ggplot2`  
- **Methods:** ARIMA, SARIMA, SARIMAX, GARCH  
- **Evaluation:** RMSE, AIC, Ljung-Box tests, rolling forecasts  

---

## 📁 Project Artifacts

- Full technical report (PDF)
- Presentation slides (PPT)

---

## 🚀 Future Extensions

- Incorporate macroeconomic or on-chain indicators as exogenous variables  
- Explore regime-switching or stochastic volatility models  
- Extend to multivariate crypto portfolios  
- Compare against machine learning benchmarks (LSTM, XGBoost)

---

