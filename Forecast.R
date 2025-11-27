#' ---
#' title: "Ethereum Forecasting"
#' output: html_document
#' ---

#' ## Data Reading

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


#' ## Differencing

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
# CHUNK 5: ARIMA MODELS
###########################################################


#' ## FIRST MODEL: ARIMA(1,1) (differencing already done so d=0)

# We use stats::arima(). Because we already differenced manually,
# we set d = 0 in the order: (p, d, q) = (1, 0, 1).

library(lmtest)

arma11_fit <- arima(d_log_price_train_ts, order = c(1, 0, 1))

coeftest(arma11_fit)

cat("\nLog-likelihood (ARMA 1,0,1):", as.numeric(logLik(arma11_fit)), "\n")
cat("AIC (ARMA 1,0,1):", AIC(arma11_fit), "\n")

# Check residuals – they should look like white noise
arma11_res <- residuals(arma11_fit)


# Plot residuals
plot(arma11_res,
     main = "Residuals of ARMA(1,1) on Differenced Log Price",
     xlab = "Time", ylab = "Residual")

# ACF of residuals
acf(arma11_res,
    main = "ACF of ARMA(1,1) Residuals", lag.max = 30)

# - Many spikes in the residual ACF are still outside the blue bands, the model has not captured all the structure.
# - That tells us we may need a higher-order ARMA(p,q) (e.g., ARMA(2,2), ARMA(5,5), etc.)



#' ## SECOND MODEL: ARIMA(2,2)

# Fit the model to the differenced log-price training series
arma22_fit <- arima(d_log_price_train_ts, order = c(2, 0, 2))

# Print coefficient table: estimate, std error, t (z), p-value
cat("\n===== ARMA(2,0,2) coefficients =====\n")
coeftest(arma22_fit)

# Print log-likelihood and AIC
cat("\nLog-likelihood (ARMA 2,0,2):", as.numeric(logLik(arma22_fit)), "\n")
cat("AIC (ARMA 2,0,2):", AIC(arma22_fit), "\n")



#' ## THIRD MODEL: ARIMA(3,3)
arma33_fit <- arima(d_log_price_train_ts, order = c(3, 0, 3))

cat("\n===== ARMA(3,0,3) coefficients =====\n")
coeftest(arma33_fit)

cat("\nLog-likelihood (ARMA 3,0,3):", as.numeric(logLik(arma33_fit)), "\n")
cat("AIC (ARMA 3,0,3):", AIC(arma33_fit), "\n")



#' ## FOURTH MODEL: ARIMA(4,4)

arma44_fit <- arima(d_log_price_train_ts, order = c(4, 0, 4))

cat("\n===== ARMA(4,0,4) coefficients =====\n")
coeftest(arma44_fit)

cat("\nLog-likelihood (ARMA 4,0,4):", as.numeric(logLik(arma44_fit)), "\n")
cat("AIC (ARMA 4,0,4):", AIC(arma44_fit), "\n")

# Check residuals – they should look like white noise
arma44_res <- residuals(arma44_fit)


# Plot residuals
plot(arma44_res,
     main = "Residuals of ARMA(4,4) on Differenced Log Price",
     xlab = "Time", ylab = "Residual")

# ACF of residuals
acf(arma44_res,
    main = "ACF of ARMA(4,4) Residuals", lag.max = 30)

# Lower AIC is better; higher logLik is better.


#' ## FORECASTING WITH ARIMA MODEL 

###########################################################
# CHUNK 6: FORECAST WITH ARIMA(4,0,4) MODEL
###########################################################


################################################
# 1. Forecast only a short horizon (7 days)
################################################

h <- 7L   # 7-day (1-week) ahead forecast

# final_fit_404 is your ARIMA(4,0,4) on d_log_price_train_ts
fc <- predict(final_fit_404, n.ahead = h)

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
# 5. Build simple ±K band from RMSE (article-style)
###########################################################

band_width <- 2 * rmse_404           # like a ~95% band; you can tweak
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
# Make sure the date column is really of class Date
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

#From LLR: If p_value is very small (< 0.05, often close to 0), then adding weekly seasonality (the SARIMA model) gives a significantly better fit than plain ARIMA.
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


#' ## FORECAST WITH SARIMA MODEL 

library(ggplot2)

############################################################
# 1. Forecast ONLY the next 7 days from the end of TRAIN set
############################################################
h <- 7   # forecast horizon = 7 days

fc_sarima_7 <- predict(sarima_515_101, n.ahead = h)
# fc_sarima_7$pred = predicted differences in log(price)
# fc_sarima_7$se   = standard error of those predictions

############################################################
# 2. Convert predicted DIFFERENCES back to log-prices
#    and then back to normal prices
############################################################

# Last log-price in the TRAIN series
last_log_train <- as.numeric(tail(log_price_train_ts, 1))

# Build forecast path on log scale
pred_log_7   <- last_log_train + cumsum(fc_sarima_7$pred)
upper_log_7  <- pred_log_7 + 1.96 * fc_sarima_7$se
lower_log_7  <- pred_log_7 - 1.96 * fc_sarima_7$se

# Back-transform to price scale
pred_price_7  <- exp(pred_log_7)
upper_price_7 <- exp(upper_log_7)
lower_price_7 <- exp(lower_log_7)

############################################################
# 3. Actual prices & dates for those SAME 7 days
#    (first 7 days of the TEST period)
############################################################

n_train <- length(log_price_train_ts)
# first 7 test observations
actual_price_7 <- exp(as.numeric(log_price_test_ts[1:h]))
dates_7        <- eth$date[(n_train + 1):(n_train + h)]

############################################################
# 4. RMSE for these 7 days (SARIMA forecast vs actual price)
############################################################
rmse_sarima_7 <- sqrt(mean((pred_price_7 - actual_price_7)^2, na.rm = TRUE))
rmse_sarima_7

############################################################
# 5. Put everything into a data frame for plotting
############################################################
sarima_7_df <- data.frame(
  date         = as.Date(dates_7),
  actual_price = actual_price_7,
  pred_price   = pred_price_7,
  lower        = lower_price_7,
  upper        = upper_price_7
)

############################################################
# 6. Plot: 7-day forecast vs actual with CI band + labels
############################################################
ggplot(sarima_7_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.6) +
  geom_line(aes(y = actual_price, colour = "Actual ETH Price"),
            linewidth = 0.8) +
  geom_point(aes(y = actual_price, colour = "Actual ETH Price"),
             size = 2) +
  geom_line(aes(y = pred_price, colour = "SARIMA Forecast"),
            linewidth = 0.8) +
  geom_point(aes(y = pred_price, colour = "SARIMA Forecast"),
             size = 2) +
  # Value labels on both lines
  geom_text(aes(y = actual_price,
                label = round(actual_price),
                colour = "Actual ETH Price"),
            vjust = -1, size = 3) +
  geom_text(aes(y = pred_price,
                label = round(pred_price),
                colour = "SARIMA Forecast"),
            vjust = 1.8, size = 3) +
  scale_colour_manual(
    name   = "Series",
    values = c("Actual ETH Price" = "blue",
               "SARIMA Forecast" = "darkred")
  ) +
  labs(
    title = "SARIMA(5,0,5)×(1,0,1)[7] – 7-day Forecast vs Actual ETH Price",
    subtitle = paste("7-day RMSE =", round(rmse_sarima_7, 2)),
    x = "Date",
    y = "Price (USD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




