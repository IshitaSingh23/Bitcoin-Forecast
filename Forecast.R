#' ---
#' title: "Ethereum Forecasting"
#' output: html_document
#' ---



#' ## Data Reading & Cleaning

############################################################
# CHUNK 1: READ & CLEAN ETHEREUM PRICE DATA (INVESTING.COM)
############################################################

library(readr)      # for read_csv()
library(dplyr)      # for data cleaning
library(lubridate)  # for dates
library(janitor)    # for clean_names()


#-----------------------------------------------------------
# 1.1 Read the CSV file
#-----------------------------------------------------------
eth_raw <- read_csv("Ethereum.csv")



# Look at the raw column names and first few rows
names(eth_raw)
head(eth_raw)

#-----------------------------------------------------------
# 1.2 Clean the column names (turn them into lowercase_with_underscores)
#-----------------------------------------------------------
eth_raw <- clean_names(eth_raw)

# Check new names
names(eth_raw)

#-----------------------------------------------------------
# 1.3 Fix the Date column and sort by date
#-----------------------------------------------------------

library(lubridate)

eth <- eth_raw %>%
  mutate(
    # Try common orders: mdy, dmy, ymd.
    # lubridate will parse correctly for most Investing.com files.
    date = parse_date_time(date, orders = c("mdy", "dmy", "ymd"))
  ) %>%
  arrange(date)

head(eth)


#-----------------------------------------------------------
# 1.4 Turn price columns from text -> numeric
#-----------------------------------------------------------

# Helper function: remove commas and convert to numeric
to_numeric <- function(x) {
  as.numeric(gsub(",", "", x))
}

eth <- eth %>%
  mutate(
    price = to_numeric(price),  # this is the CLOSE price on Investing.com
    open  = to_numeric(open),
    high  = to_numeric(high),
    low   = to_numeric(low)
  )
str(eth)

#-----------------------------------------------------------
# 1.5 Keep only the main columns we need for now
#-----------------------------------------------------------

eth <- eth %>%
  select(date, open, high, low, price)

head(eth)

#-----------------------------------------------------------
# 1.6 Create log_price and log_return
#-----------------------------------------------------------

# log_price = natural log of the closing price
eth <- eth %>%
  mutate(
    log_price = log(price)
  )

# log_return = 100 * daily log-return (approx % change)
# This is: 100 * (log_price_today - log_price_yesterday)
eth <- eth %>%
  arrange(date) %>%
  mutate(
    log_return = 100 * (log_price - dplyr::lag(log_price))
  )

# First row has NA log_return (no previous day)
head(eth)

#-----------------------------------------------------------
# 1.7 Check for missing values and missing dates
#-----------------------------------------------------------
colSums(is.na(eth))

# Check if any calendar days are missing in between
all_dates <- seq(from = min(eth$date, na.rm = TRUE),
                 to   = max(eth$date, na.rm = TRUE),
                 by   = "day")

missing_dates <- setdiff(all_dates, eth$date)
missing_dates

head(eth)


#' ## Check Stationarity

###########################################################
# CHUNK 2: PRELIMINARY PLOTS – SEE IF SERIES IS STATIONARY
###########################################################

library(ggplot2)

## 2.1 Plot raw closing price over time --------------------

ggplot(eth, aes(x = date, y = price)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily Ethereum Closing Price (USD)",
       x = "Date",
       y = "Price (USD)")

# Big upward & downward moves, the average level changes over time
# This means the series is NOT stationary in level.


## 2.2 Plot log of price over time ------------------------

ggplot(eth, aes(x = date, y = log_price)) +
  geom_line(color = "darkgreen") +
  labs(title = "Log of Daily Ethereum Price",
       x = "Date",
       y = "log(Price)")

# Still shows strong trend so still non-stationary.


## 2.3 Plot daily log-returns over time -------------------

ggplot(eth %>% dplyr::filter(!is.na(log_return)),
       aes(x = date, y = log_return)) +
  geom_line(color = "purple") +
  labs(title = "Daily Ethereum Log-Returns (%)",
       x = "Date",
       y = "Log-return (%)")

# - Values bouncing around zero
# - Periods of calm and periods of huge spikes (volatility clustering)
# - Much more "stable" mean than the price plots

## 2.4.summary statistics ----------------

summary(eth$price)
sd(eth$price, na.rm = TRUE)

summary(eth$log_return)
sd(eth$log_return, na.rm = TRUE)

# - Price has a huge range (min vs max)
# - Returns have mean ~ 0 but large standard deviation



#' ## Data Splitting

###########################################################
# CHUNK 3: TRAIN / TEST SPLIT (KEEP TIME ORDER)
###########################################################

log_price_ts <- ts(eth$log_price, frequency = 7)  # daily data, weekly pattern

# Total length of the series
n_total <- length(log_price_ts)

# For example: 80% train, 20% test
train_ratio <- 0.8
n_train <- floor(train_ratio * n_total)

# Split the series:
log_price_train_ts <- window(log_price_ts, end = c(0, n_train))
log_price_test_ts  <- window(log_price_ts, start = c(0, n_train + 1))

# Simple checks
length(log_price_train_ts)  # should be about 80% of total
length(log_price_test_ts)   # remaining 20%

# - log_price_train_ts = earlier dates (used to fit models)
# - log_price_test_ts  = most recent dates (used to check forecast accuracy)


#' ## Stationarity checks & Differencing

###########################################################
# CHUNK 4: STATIONARITY – ADF + DIFFERENCING
###########################################################

library(tseries)  # for adf.test()

## 4.1 ADF test on log_price (training part) --------------

adf_log_price_train <- adf.test(log_price_train_ts)
adf_log_price_train

# - H0: series has a unit root (non-stationary).
# - If p-value is large (usually > 0.05), we FAIL to reject H0: log_price is NON-STATIONARY.


## 4.2 First difference of log_price (training) ------------

d_log_price_train_ts <- diff(log_price_train_ts)

# Plot to see it
plot(d_log_price_train_ts,
     main = "Differenced Log Price (Training Data)",
     xlab = "Time", ylab = "diff log(price)")

## 4.3 ADF test on differenced log_price -------------------

adf_d_log_price_train <- adf.test(d_log_price_train_ts)
adf_d_log_price_train

# Now we expect a SMALL p-value (< 0.05):
# - We REJECT H0 and say: differenced log price is STATIONARY.
# - This differenced series is what we will use for ARMA/ARIMA(p,0,q).


###########################################################
# CHUNK 4b: ACF & PACF OF DIFFERENCED LOG PRICE (TRAIN)
###########################################################

# d_log_price_train_ts is the differenced log-price series
# We use ACF/PACF plots on this series to help choose p and q.

par(mfrow = c(1, 2))  

# ACF plot – helps us choose MA(q) order
acf(
  d_log_price_train_ts,
  lag.max = 50,                     
  main = "ACF of Differenced Log Price"
)

# PACF plot – helps us choose AR(p) order
pacf(
  d_log_price_train_ts,
  lag.max = 30,
  main = "PACF of Differenced Log Price"
)

par(mfrow = c(1, 1))  # reset plotting layout


#' ## Modelling

###########################################################
# CHUNK 5: ARIMA MODELS
###########################################################

#' ## FIRST MODEL: ARIMA(1,1)

# We use stats::arima(). Because we already differenced manually,
# we set d = 0 in the order: (p, d, q) = (1, 0, 1).

library(lmtest)

arma11_fit <- arima(d_log_price_train_ts, order = c(1, 0, 1))

coeftest(arma11_fit)

cat("AIC (ARMA 1,0,1):", AIC(arma11_fit))
cat("Log-likelihood (ARMA(1,0,1)):", as.numeric(logLik(arma11_fit)))



#' ## SECOND MODEL: ARIMA(2,2)

# Fit the model to the differenced log-price training series
arma22_fit <- arima(d_log_price_train_ts, order = c(2, 0, 2))

# Print coefficient table: estimate, std error, t (z), p-value
cat("===== ARMA(2,0,2) coefficients =====")
coeftest(arma22_fit)

# Print log-likelihood and AIC
cat("Log-likelihood (ARMA 2,0,2):", as.numeric(logLik(arma22_fit)))
cat("AIC (ARMA 2,0,2):", AIC(arma22_fit))



#' ## THIRD MODEL: ARIMA(3,3)
arma33_fit <- arima(d_log_price_train_ts, order = c(3, 0, 3))

cat("===== ARMA(3,0,3) coefficients =====")
coeftest(arma33_fit)

cat("Log-likelihood (ARMA 3,0,3):", as.numeric(logLik(arma33_fit)))
cat("AIC (ARMA 3,0,3):", AIC(arma33_fit))



#' ## FOURTH MODEL: ARIMA(4,4)

arma44_fit <- arima(d_log_price_train_ts, order = c(4, 0, 4))

cat("===== ARMA(4,0,4) coefficients =====")
coeftest(arma44_fit)

cat("Log-likelihood (ARMA 4,0,4):", as.numeric(logLik(arma44_fit)))
cat("AIC (ARMA 4,0,4):", AIC(arma44_fit))


#' ## FIFTH MODEL: ARIMA(0,1)

arma01_fit <- arima(d_log_price_train_ts, order = c(0, 0, 1))

cat("===== ARMA(0,0,1) coefficients =====")
coeftest(arma01_fit)

cat("Log-likelihood (ARMA 0, 0, 1):", as.numeric(logLik(arma01_fit)))
cat("AIC (ARMA 0, 0, 1):", AIC(arma01_fit))



#' ## SIXTH MODEL: ARIMA(2,1)

arma21_fit <- arima(d_log_price_train_ts, order = c(2, 0, 1))

cat("===== ARMA(2,0,1) coefficients =====")
coeftest(arma21_fit)

cat("Log-likelihood (ARMA 2, 0, 1):", as.numeric(logLik(arma21_fit)))
cat("AIC (ARMA 2, 0, 1):", AIC(arma21_fit))



#' ## ARIMA MODEL SUMMARY
############################################################
# ARMA summary table using already-fitted models
############################################################

arma_models <- list(
  "ARMA(1,0,1)" = arma11_fit,
  "ARMA(2,0,2)" = arma22_fit,
  "ARMA(3,0,3)" = arma33_fit,
  "ARMA(4,0,4)" = arma44_fit,
  "ARMA(0,0,1)" = arma01_fit,
  "ARMA(2,0,1)" = arma21_fit
)

# 2. Build a table with AIC and log-likelihood
arma_table <- do.call(
  rbind,
  lapply(names(arma_models), function(mname) {
    fit <- arma_models[[mname]]
    data.frame(
      model  = mname,
      AIC    = AIC(fit),
      logLik = as.numeric(logLik(fit)),
      row.names = NULL
    )
  })
)

# 3.sort by AIC: best model at top
arma_table <- arma_table[order(arma_table$AIC), ]

arma_table


#' ## RESIDUAL DIAGNOSTIC FOR BEST ARIMA MODEL (4,1,4)

#LOWEST AIC & HIGHEST LOGLIKELIHOOD IS FOR ARIMA(4,1,4)
# Check residuals – they should look like white noise
arma44_res <- residuals(arma44_fit)

# Plot residuals
plot(arma44_res,
     main = "Residuals of ARMA(4,4) on Differenced Log Price",
     xlab = "Time", ylab = "Residual")

# ACF of residuals
acf(arma44_res,
    main = "ACF of ARMA(4,4) Residuals", lag.max = 30)

# Ljung–Box test for ARMA(4,4) residuals
# H0: residuals are white noise (no remaining autocorrelation)

Box.test(
  arma44_res,
  lag  = 20,          # you can change 20 to 10, 30, etc.
  type = "Ljung-Box",
  fitdf = 8           # p + q = 4 + 4 = 8 parameters in ARMA(4,4)
)
# p-value is >0.05 so we fail to reject null hypothesis.
# therefore, residuals look like white noise. 



#' ## FORECASTING WITH ARIMA MODEL 

###########################################################
# CHUNK 6: FORECAST WITH ARIMA(4,0,4) MODEL
###########################################################


################################################
# 1. Forecast only a short horizon (7 days)
################################################

h <- 7L   # 7-day (1-week) ahead forecast

# arma44_fit is your ARIMA(4,0,4) on d_log_price_train_ts
fc <- predict(arma44_fit, n.ahead = h)

# Make sure these are plain numeric vectors
last_log_train <- as.numeric(tail(log_price_train_ts, 1))
pred_diffs     <- as.numeric(fc$pred)

###############################################################
# 2. Convert predicted differences back to log-price & price
###############################################################

# cumulative sum of predicted daily changes in log(price)
log_price_forecast <- last_log_train + cumsum(pred_diffs)

# back to dollar prices
pred_price <- exp(log_price_forecast)

#################################################
# 3. Actual test prices & dates for those 7 days
#################################################

start_test <- n_train + 1
end_test   <- start_test + h - 1

dates_test   <- eth$date[start_test:end_test]
actual_price <- eth$price[start_test:end_test]

########################################
# 4. RMSE of ARIMA(4,0,4) on this week
########################################

rmse_404 <- sqrt(mean((actual_price - pred_price)^2))
rmse_404

###########################################################
# 5. Build simple K band from RMSE 
###########################################################

band_width <- 2 * rmse_404          
upper <- pred_price + band_width
lower <- pred_price - band_width

forecast_df <- data.frame(
  date         = dates_test,
  actual_price = actual_price,
  pred_price   = pred_price,
  lower        = lower,
  upper        = upper
)

###########################################
# 6. Plot actual vs predicted + RMSE band
###########################################
forecast_df$date <- as.Date(forecast_df$date)

library(ggplot2)

library(ggplot2)

# Make sure date is Date
forecast_df$date <- as.Date(forecast_df$date)

ggplot(forecast_df, aes(x = date)) +
  # Grey error band (±2*RMSE)
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.6) +
  
  # Actual prices: line + points, mapped to colour = "Actual"
  geom_line(aes(y = actual_price, colour = "Actual"), linewidth = 0.7) +
  geom_point(aes(y = actual_price, colour = "Actual"), size = 2) +
  
  # Forecast prices: line + points, mapped to colour = "Forecast"
  geom_line(aes(y = pred_price, colour = "Forecast"), linewidth = 0.7) +
  geom_point(aes(y = pred_price, colour = "Forecast"), size = 2) +
  
  # Text labels for ACTUAL prices (blue)
  geom_text(aes(y = actual_price,
                label = round(actual_price, 0)),
            vjust = -1.0,          # a bit above the blue point
            colour = "blue",
            size = 3) +
  
  # Text labels for FORECAST prices (red)
  geom_text(aes(y = pred_price,
                label = round(pred_price, 0)),
            vjust = 1.5,           # a bit below the red point
            colour = "red",
            size = 3) +
  
  # Colour legend: map series names to colours
  scale_color_manual(
    name   = "Series",
    values = c("Actual" = "blue", "Forecast" = "red")
  ) +
  
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  labs(
    title = "ARIMA(4,0,4) – 7-day Forecast vs Actual ETH Price",
    x     = "Date",
    y     = "Price (USD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





#' ## FIFTH MODEL: SARIMA MODELS


###########################################################
# CHUNK 7: SARIMA MODELS WITH WEEKLY SEASONALITY (m = 7)
###########################################################
# The general form is:
# arima(log_price_train_ts,
#       order    = c(p, 1, q),
#       seasonal = list(order = c(P, D, Q), period = 7))

sarima_111_101 <- arima(
  log_price_train_ts,
  order    = c(1, 1, 1),              # non-seasonal (p,d,q)
  seasonal = list(order = c(1, 0, 1), # seasonal (P,D,Q)
                  period = 7)         # weekly seasonality
)

coeftest(sarima_111_101)
logLik(sarima_111_101)
AIC(sarima_111_101)

sarima_111_101_res <- residuals(sarima_111_101)


# Plot residuals
plot(sarima_111_101_res,
     main = "Residuals of SARIMA(1,1,1)(1,0,1) on Differenced Log Price",
     xlab = "Time", ylab = "Residual")

# ACF of residuals
acf(sarima_111_101_res,
    main = "ACF of SARIMA(1,1,1)(1,0,1) Residuals", lag.max = 30)


sarima_212_101 <- arima(
  log_price_train_ts,
  order    = c(2, 1, 2),
  seasonal = list(order = c(1, 0, 1), period = 7)
)

coeftest(sarima_212_101)
logLik(sarima_212_101)
AIC(sarima_212_101)

sarima_313_101 <- arima(
  log_price_train_ts,
  order    = c(3, 1, 3),
  seasonal = list(order = c(1, 0, 1), period = 7)
)

coeftest(sarima_313_101)
logLik(sarima_313_101)
AIC(sarima_313_101)

sarima_515_101 <- arima(
  log_price_train_ts,
  order    = c(5, 1, 5),
  seasonal = list(order = c(1, 0, 1), period = 7)
)

coeftest(sarima_515_101)
logLik(sarima_515_101)
AIC(sarima_515_101)

sarima_515_101_res <- residuals(sarima_515_101)


# Plot residuals
plot(sarima_515_101_res,
     main = "Residuals of SARIMA(5,1,5)(1,0,1) on Differenced Log Price",
     xlab = "Time", ylab = "Residual")

# ACF of residuals
acf(sarima_515_101_res,
    main = "ACF of SARIMA(5,1,5)(1,0,1) Residuals", lag.max = 30)



sarima_results <- data.frame(
  model = c("SARIMA(1,1,1)x(1,0,1)[7]",
            "SARIMA(2,1,2)x(1,0,1)[7]",
            "SARIMA(3,1,3)x(1,0,1)[7]",
            "SARIMA(5,1,5)x(1,0,1)[7]"),
  logLik = c(logLik(sarima_111_101),
             logLik(sarima_212_101),
             logLik(sarima_313_101),
             logLik(sarima_515_101)),
  AIC    = c(AIC(sarima_111_101),
             AIC(sarima_212_101),
             AIC(sarima_313_101),
             AIC(sarima_515_101))
)

sarima_results

# sarima_515_101 is the best model.



#' ## SARIMA & ARIMA Model Comparision

###########################################################
# CHUNK 8: COMPARE BEST ARIMA VS BEST SARIMA
###########################################################

# Helper: Likelihood Ratio Test between two models
llr_test <- function(model_big, model_small) {
  # LR statistic
  LR <- 2 * (as.numeric(logLik(model_big)) - as.numeric(logLik(model_small)))
  
  # difference in number of parameters
  df_diff <- length(coef(model_big)) - length(coef(model_small))
  
  # p-value from Chi-square distribution
  p_value <- 1 - pchisq(LR, df = df_diff)
  
  return(list(LR = LR, df = df_diff, p_value = p_value))
}

# Compare ARIMA(4,1,4) vs SARIMA(5,1,5)x(1,0,1)[7]
# (SARIMA has more parameters so SARIMA is "big" model)

test_sarima_vs_arima <- llr_test(sarima_515_101, arma44_fit)
test_sarima_vs_arima

#From LLR: If p_value is very small (< 0.05, often close to 0), then 
#adding weekly seasonality (the SARIMA model) gives a significantly 
#better fit than plain ARIMA.
AIC(arma44_fit)
AIC(sarima_515_101)



#' ## Residual Diagnostics

###########################################################
# CHUNK 9: RESIDUAL DIAGNOSTICS FOR FINAL SARIMA
###########################################################

# Residuals of final SARIMA
sarima_res <- residuals(sarima_515_101)

# 9.1 Plot residuals over time
plot(sarima_res,
     main = "Residuals of Final SARIMA Model",
     xlab = "Time", ylab = "Residual")

# 9.2 ACF of residuals
acf(sarima_res,
    main = "ACF of SARIMA Residuals")

# 9.3 ADF test on residuals (should be stationary, just noise)
library(tseries)
adf_res <- adf.test(sarima_res)
adf_res

# Ljung–Box test for ARMA(4,4) residuals
# H0: residuals are white noise (no remaining autocorrelation)

Box.test(
  sarima_res,
  lag  = 20,          # you can change 20 to 10, 30, etc.
  type = "Ljung-Box"
)




#' ## FORECAST WITH SARIMA MODEL 

###########################################################
# CHUNK 10: 7-day forecast from SARIMA(5,1,5)x(1,0,1)[7]
###########################################################

library(ggplot2)

# 1. Forecast horizon = 7 days
h <- 7L

# 2. Get 7-day forecast on the log-price scale
sarima_fc <- predict(sarima_515_101, n.ahead = h)

# Predicted log-prices and their standard errors
log_pred <- sarima_fc$pred
log_se   <- sarima_fc$se

# 3. 95% CI on the log-price scale
log_upper <- log_pred + 1.96 * log_se
log_lower <- log_pred - 1.96 * log_se

# 4. Convert everything back to actual price (exp of log-price)
pred_price  <- exp(log_pred)
upper_price <- exp(log_upper)
lower_price <- exp(log_lower)

# 5. Actual prices for the first 7 days of the TEST set
#    (log_price_test_ts is log(price) for the test period)
actual_price <- exp(log_price_test_ts[1:h])

# Dates for the TEST set, then keep only the first 7
dates_all_test <- tail(eth$date, length(log_price_test_ts))
dates_7        <- dates_all_test[1:h]

# Make sure dates are Date class
dates_7 <- as.Date(dates_7)

# 6. Put everything into one data frame
sarima_forecast_df <- data.frame(
  date         = dates_7,
  actual_price = actual_price,
  pred_price   = pred_price,
  lower        = lower_price,
  upper        = upper_price
)

# 7. RMSE of 7-day SARIMA forecast
rmse_sarima_7 <- sqrt(mean((sarima_forecast_df$actual_price -
                              sarima_forecast_df$pred_price)^2))
rmse_sarima_7
# <- this is your SARIMA 7-day forecast error in USD

# 8. Plot: Actual vs Forecast with 95% CI and value labels
ggplot(sarima_forecast_df, aes(x = date)) +
  # Confidence band
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.5) +
  # Actual prices
  geom_line(aes(y = actual_price, colour = "Actual"), linewidth = 1.1) +
  geom_point(aes(y = actual_price, colour = "Actual"), size = 2.5) +
  # Forecast prices
  geom_line(aes(y = pred_price, colour = "Forecast"), linewidth = 1.1) +
  geom_point(aes(y = pred_price, colour = "Forecast"), size = 2.5) +
  # Labels on points (rounded to nearest dollar)
  geom_text(aes(y = actual_price,
                label = round(actual_price)),
            vjust = -1.0, colour = "coral3", size = 3.2) +
  geom_text(aes(y = pred_price,
                label = round(pred_price)),
            vjust =  1.5, colour = "steelblue4", size = 3.2) +
  # Legend colours
  scale_colour_manual(
    name   = "",
    values = c("Actual" = "coral3",
               "Forecast" = "steelblue4")
  ) +
  # Show all 7 dates on x-axis
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  labs(
    title = "SARIMA(5,1,5)×(1,0,1)[7] – 7-day Forecast vs Actual ETH Price",
    x = "Date",
    y = "Price (USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(face = "bold", size = 16)
  )





#' ## ROLLING WINDOW EVALUATION

###########################################################
# CHUNK 11: ROLLING 7-DAY EVALUATION FOR SARIMA(5,1,5)x(1,0,1)[7]
###########################################################

h <- 7                               # we forecast 7 days ahead
n_total <- length(log_price_ts)      # length of log-price series
train_min <- floor(0.6 * n_total)    # first 60% used as minimum training

# move in blocks of 7 days
start_points <- seq(from = train_min + 1,
                    to   = n_total - h,
                    by   = h)

rmse_sarima <- rep(NA_real_, length(start_points))  # store RMSEs

for (i in seq_along(start_points)) {
  
  s <- start_points[i]              # first test index in this window
  
  # training indices: all points BEFORE s
  train_idx <- 1:(s - 1)
  # test indices: next 7 days
  test_idx  <- s:(s + h - 1)
  
  # pull training series, drop any NA just in case
  train_ts <- log_price_ts[train_idx]
  train_ts <- train_ts[is.finite(train_ts)]
  
  # if not enough points, skip
  if (length(train_ts) < 50) next
  
  # ---- TRY to fit SARIMA; if it breaks, skip this window ----
  fit_sarima <- try(
    arima(
      train_ts,
      order    = c(5, 1, 5),
      seasonal = list(order = c(1, 0, 1), period = 7),
      method   = "CSS-ML",      # a bit more stable
      transform.pars = FALSE
    ),
    silent = TRUE
  )
  
  # if fitting failed, go to next window
  if (inherits(fit_sarima, "try-error")) {
    cat("Skipping window", i, "- SARIMA did not converge\n")
    next
  }
  
  # forecast next 7 days (log scale)
  fc <- predict(fit_sarima, n.ahead = h)
  pred_log <- as.numeric(fc$pred)
  
  # convert to normal prices
  pred_price   <- exp(pred_log)
  actual_price <- exp(log_price_ts[test_idx])
  
  # compute RMSE for this window
  rmse_sarima[i] <- sqrt(mean((pred_price - actual_price)^2, na.rm = TRUE))
}

# OVERALL summary
mean_rmse_sarima <- mean(rmse_sarima, na.rm = TRUE)
mean_rmse_sarima

rolling_sarima <- data.frame(
  window = seq_along(start_points),
  start_index = start_points,
  rmse = rmse_sarima
)

head(rolling_sarima)


library(ggplot2)

# Plot rolling SARIMA RMSE over windows
ggplot(rolling_sarima, aes(x = window, y = rmse)) +
  geom_line(na.rm = TRUE) +            # connect the points with a line
  geom_point(na.rm = TRUE) +           # show each window as a dot
  geom_hline(                          # add a horizontal line = mean RMSE
    yintercept = mean(rolling_sarima$rmse, na.rm = TRUE),
    linetype = "dashed"
  ) +
  labs(
    title = "Rolling 7-Day RMSE for SARIMA Model",
    x     = "Rolling Window Number",
    y     = "RMSE (USD)"
  ) +
  theme_minimal()







#' ## GARCH MODEL 
###########################################################
# CHUNK 12: GARCH Model
###########################################################
###########################################################
# CHUNK: GARCH(1,1) on log-returns + forecast + RMSE
###########################################################

library(rugarch)

# 1. Training returns (you already have this from earlier steps)
#    d_log_price_train_ts ≈ daily log-returns of the training data
ret_train <- d_log_price_train_ts

# 2. Specify a basic GARCH(1,1) with constant mean
garch11_spec <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)   # (p, q)
  ),
  mean.model = list(
    armaOrder = c(0, 0),   # no ARMA in the mean, just a constant
    include.mean = TRUE
  ),
  distribution.model = "norm"
)

# 3. Fit the GARCH(1,1) model on the training returns
garch11_fit <- ugarchfit(
  spec = garch11_spec,
  data = ret_train
)

show(garch11_fit)   # optional: to see the parameter estimates

# 4. Forecast returns for the whole TEST period
h_test <- length(log_price_test_ts)      # forecast horizon = size of test set

garch11_fc <- ugarchforecast(
  garch11_fit,
  n.ahead = h_test
)

# 5. Get the predicted mean returns and turn them back into log-prices
pred_ret_garch <- as.numeric(fitted(garch11_fc))  # μ_t forecasts

# last log-price from the training set
last_log_train  <- as.numeric(tail(log_price_train_ts, 1))

# cumulative sum of predicted returns = change in log-price
cum_ret_garch   <- cumsum(pred_ret_garch)

# predicted log-prices over the test horizon
log_pred_garch  <- last_log_train + cum_ret_garch

# 6. Compute RMSE for GARCH

## (a) RMSE on log-price
log_test <- as.numeric(log_price_test_ts)

rmse_garch_log <- sqrt(mean((log_test - log_pred_garch)^2))

## (b) RMSE on price scale
price_test <- exp(log_test)
price_pred <- exp(log_pred_garch)

rmse_garch_price <- sqrt(mean((price_test - price_pred)^2))

rmse_garch_log
rmse_garch_price





















############################################################
# CHUNK: SARIMAX – SARIMA with an exogenous regressor
# Here we use "yesterday's log price" as xreg (lag-1).
############################################################

# 1. Build a lag-1 log-price series for the whole dataset
log_price_all <- c(log_price_train_ts, log_price_test_ts)

lag1_all <- c(NA, head(log_price_all, -1))   # shift by 1 day

# 2. Split the lagged series into train and test parts
xreg_train <- lag1_all[1:length(log_price_train_ts)]
xreg_test  <- lag1_all[(length(log_price_train_ts) + 1) :
                         (length(log_price_train_ts) + length(log_price_test_ts))]

# Remove the first NA in training to line up y and x properly
log_price_train_ts_x <- log_price_train_ts[-1]
xreg_train_x         <- xreg_train[-1]

# 3. Fit a SARIMAX model
#    Here I use your best SARIMA orders: (5,1,5) × (1,0,1)[7]
sarimax_fit <- arima(
  log_price_train_ts_x,
  order    = c(5, 1, 5),                  # non-seasonal (p,d,q)
  seasonal = list(order = c(1, 0, 1),     # seasonal (P,D,Q)
                  period = 7),           # weekly seasonality
  xreg     = xreg_train_x                # exogenous regressor
)

sarimax_fit

# 4. Predicted log-prices (already done)
log_pred_sarimax <- sarimax_fc$pred   # ts object

# 5. Compute RMSE (both on log scale and price scale)

# (a) RMSE on log-price
rmse_sarimax_log <- sqrt(
  mean((as.numeric(log_price_test_ts) - as.numeric(log_pred_sarimax))^2)
)

# (b) RMSE on actual price (back-transform with exp)
actual_price_test  <- exp(as.numeric(log_price_test_ts))
pred_price_sarimax <- exp(as.numeric(log_pred_sarimax))

rmse_sarimax_price <- sqrt(
  mean((actual_price_test - pred_price_sarimax)^2)
)

rmse_sarimax_log
rmse_sarimax_price
