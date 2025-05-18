library(fpp2)
library(tseries)
setwd("C:/Users/Neha Chowdary/OneDrive/Desktop/EndTerms/Semester 8/Advanced Time Series")

# Retail Sales: Clothing stores from FRED MRTSSM4481USN
Retail_Sales <- read.csv("Retail_Sales.csv", header = TRUE)
#View(Retail_Sales)

Retail_ts <- ts(Retail_Sales$Sales, start = c(2005,1), frequency = 12)
#Retail_ts

# Check structure and summary
str(Retail_Sales)
summary(Retail_Sales)

# Check for missing values
sum(is.na(Retail_Sales$Sales))

# Basic Time Series Plot
autoplot(Retail_ts) + 
  ggtitle("Retail Sales : Clothing Stores (Units: Millions of Dollars) ") +
  ylab("Monthly Sales") + xlab("Year")

# Seasonal Plot
ggseasonplot(Retail_ts, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Seasonal Plot")

# Subseries Plot
ggsubseriesplot(Retail_ts) +
  ggtitle("Seasonal Subseries Plot")

# Lag Plot
gglagplot(Retail_ts) +
  ggtitle("Lag Plot")

# ACF Plot
Acf(Retail_ts) + ggtitle("Retail Sales: Cothing Stores")

#PACF Plot
pacf(Retail_ts, main = "Retail Sales: Cothing Stores")

# PACF plot with more control
pacf(Retail_ts, lags = 60, main = "Partial ACF of Raw Retail Sales Data")

#Decompose Plot
decompose(Retail_ts)
plot(decompose(Retail_ts))


# Linear regression model with trend and seasonality
reg_model <- tslm(Retail_ts ~ trend + season)
summary(reg_model)

# Plot actual vs fitted values
autoplot(Retail_ts) +
  autolayer(fitted(reg_model), series = "Fitted (Regression)") +
  ggtitle("Linear Regression Model: Retail Sales") +
  ylab("Sales") + xlab("Year")

# Simple Moving Average (4-month)
ma_4 <- ma(Retail_ts, order = 4)
autoplot(Retail_ts) +
  autolayer(ma_4, series = "4-Month MA") +
  ggtitle("Simple Moving Average (4-Month)") +
  ylab("Sales") + xlab("Year")

# Simple Exponential Smoothing (SES)
ses_model <- ses(Retail_ts, h = 12)
autoplot(ses_model) + ggtitle("Simple Exponential Smoothing")

# Holt’s Linear Trend Method
holt_model <- holt(Retail_ts, h = 12)
autoplot(holt_model) + ggtitle("Holt’s Linear Trend Smoothing")

# Holt-Winters Additive
hw_add <- hw(Retail_ts, seasonal = "additive", h = 12)
autoplot(hw_add) + ggtitle("Holt-Winters (Additive)")



#Part - 2

### Stationarity Test - Initial
adf.test(Retail_ts, k=12)
kpss.test(Retail_ts, null="Trend")
ndiffs(Retail_ts)

# Differencing and ACF/PACF
Retail_ts_seasonaldiff = diff(Retail_ts,12)
ggtsdisplay(Retail_ts_seasonaldiff, main= "Seasonal Differenced Time Series Representation")

ndiffs(Retail_ts_seasonaldiff)


#stationary test
adf.test(Retail_ts_seasonaldiff, k=12)
kpss.test(Retail_ts_seasonaldiff, null="Trend")


Retail_ts_Finaldiff = diff(Retail_ts_seasonaldiff, 1)
ggtsdisplay(Retail_ts_Finaldiff, main= "Seasonal + Trend Differenced Time Series Representation")

#stationary test
adf.test(Retail_ts_Finaldiff, k=12)
kpss.test(Retail_ts_Finaldiff, null="Trend")


auto.arima(Retail_ts_Finaldiff, trace =T,approximation = FALSE)
##ARIMA(1,1,3)(1,1,1)[12] 
#Model Formulation

## Model Formulation
arima(Retail_ts,order =c(1,1,1), seasonal = list(order = c(1,1,1), period = 12))
arima(Retail_ts,order =c(1,1,2), seasonal = list(order = c(1,1,1), period = 12))
arima(Retail_ts,order =c(1,1,3), seasonal = list(order = c(1,1,1), period = 12))
arima(Retail_ts,order =c(2,1,1), seasonal = list(order = c(1,1,1), period = 12))
arima(Retail_ts,order =c(2,1,2), seasonal = list(order = c(1,1,1), period = 12))
arima(Retail_ts,order =c(2,1,3), seasonal = list(order = c(1,1,1), period = 12))

arima(Retail_ts,order =c(1,1,1), seasonal = list(order = c(2,1,1), period = 12))
arima(Retail_ts,order =c(1,1,2), seasonal = list(order = c(2,1,1), period = 12))
arima(Retail_ts,order =c(1,1,3), seasonal = list(order = c(2,1,1), period = 12))
arima(Retail_ts,order =c(2,1,1), seasonal = list(order = c(2,1,1), period = 12))
arima(Retail_ts,order =c(2,1,2), seasonal = list(order = c(2,1,1), period = 12))
arima(Retail_ts,order =c(2,1,3), seasonal = list(order = c(2,1,1), period = 12))

arima(Retail_ts,order =c(1,1,1), seasonal = list(order = c(3,1,1), period = 12))
arima(Retail_ts,order =c(1,1,2), seasonal = list(order = c(3,1,1), period = 12))
arima(Retail_ts,order =c(1,1,3), seasonal = list(order = c(3,1,1), period = 12))
arima(Retail_ts,order =c(2,1,1), seasonal = list(order = c(3,1,1), period = 12))
arima(Retail_ts,order =c(2,1,2), seasonal = list(order = c(3,1,1), period = 12))
arima(Retail_ts,order =c(2,1,3), seasonal = list(order = c(3,1,1), period = 12))


auto.arima(Retail_ts, approximation = FALSE)

training <- subset(Retail_ts, end=length(Retail_ts)-14)
test <- subset(Retail_ts, start=length(Retail_ts)-13)

### Model1 ##ARIMA (1,1,2)(3,1,1)(12)
Model1 = arima(Retail_ts,order =c(1,1,2), seasonal = list(order = c(3,1,1), period = 12))
Model_1_residuals=residuals(Model1)
Box.test(Model_1_residuals, lag = 12, type = "Ljung-Box")
checkresiduals(Model1)
adf.test(Model_1_residuals)
Model_test1=arima(training,order =c(1,1,2), seasonal = list(order = c(3,1,1), period = 12))
forecast_ts1 = forecast(Model_test1,h=13)
AC_1 = accuracy(forecast_ts1,test)
AC_1

### Model2 ##ARIMA (1,1,2)(2,1,1)(12)
Model2 = arima(Retail_ts,order =c(1,1,2), seasonal = list(order = c(2,1,1), period = 12))
Model_2_residuals=residuals(Model2)
Box.test(Model_2_residuals, lag = 12, type = "Ljung-Box")
checkresiduals(Model2)
adf.test(Model_2_residuals)

Model_test2=arima(training,order =c(1,1,2), seasonal = list(order = c(2,1,1), period = 12))
forecast_Ts2 = forecast(Model_test2,h=13)
AC_2 = accuracy(forecast_Ts2,test)
AC_2


### Model3 ##ARIMA (2,1,3)(3,1,1)(12)
Model3 = arima(Retail_ts,order =c(2,1,3), seasonal = list(order = c(3,1,1), period = 12))
Model_3_residuals=residuals(Model3)
Box.test(Model_3_residuals, lag = 12, type = "Ljung-Box")
checkresiduals(Model3)
adf.test(Model_3_residuals)

Model_test3=arima(training,order =c(2,1,3), seasonal = list(order = c(3,1,1), period = 12))
forecast_Ts3 = forecast(Model_test3,h=13)
AC_3 =accuracy(forecast_Ts3,test)
AC_3


### Model4 ##ARIMA (1,1,2)(1,1,1)(12)
Model4 = arima(Retail_ts,order =c(1,1,2), seasonal = list(order = c(1,1,1), period = 12))
Model_4_residuals=residuals(Model4)
Box.test(Model_4_residuals, lag = 12, type = "Ljung-Box")
adf.test(Model_4_residuals)
checkresiduals(Model4)
Model_test4=arima(training,order =c(1,1,2), seasonal = list(order = c(1,1,1), period = 12))
forecast_Ts4 = forecast(Model_test4,h=13)
AC_4 = accuracy(forecast_Ts4,test)
AC_4


# 11. Compare Model Accuracies

comparison <- rbind(
  "ARIMA (1,1,2)(3,1,1)" = AC_1[2, c("RMSE", "MAE", "MAPE")],
  "ARIMA (1,1,2)(2,1,1)" = AC_2[2, c("RMSE", "MAE", "MAPE")],
  "ARIMA (2,1,3)(3,1,1)" = AC_3[2, c("RMSE", "MAE", "MAPE")],
  "ARIMA (1,1,2)(1,1,1)" = AC_4[2, c("RMSE", "MAE", "MAPE")])
print(round(comparison, 4))

# Identify best model based on lowest RMSE
rmse_values <- comparison[, "RMSE"]
best_model_index1 <- which.min(rmse_values)
best_model_name1 <- rownames(comparison)[best_model_index1]

cat("Best ARIMA model based on RMSE is:", best_model_name1, "\n")

# Identify best model based on lowest MAE
mae_values <- comparison[, "MAE"]
best_model_index2 <- which.min(mae_values)
best_model_name2 <- rownames(comparison)[best_model_index2]

cat("Best ARIMA model based on MAE is:", best_model_name2, "\n")

# Identify best model based on lowest MAPE
mape_values <- comparison[, "MAPE"]
best_model_index3 <- which.min(mape_values)
best_model_name3 <- rownames(comparison)[best_model_index3]

cat("Best ARIMA model based on MAPE is:", best_model_name3, "\n")
best_model_name <- "ARIMA (1,1,2)(2,1,1)[12]"
cat("Best ARIMA model based on evaluation metrics (lowest RMSE, MAPE, etc.):", best_model_name, "\n")


forecast_final=forecast(arima(Retail_ts,order =c(1,1,2), seasonal = list(order = c(2,1,1), period = 12)),h=12, level = c(80, 95))
forecast_final

library(forecast)
library(ggplot2)
library(zoo)

# Dates and fitted values
dates <- as.Date(as.yearmon(time(Retail_ts)))
fitted_vals <- fitted(forecast_final$model)

# Training + Fitted Data
train_df <- data.frame(
  Date = dates,
  Training = as.numeric(Retail_ts),
  Fitted = fitted_vals
)

# Forecast Dates
fc_dates <- seq(as.Date(as.yearmon(max(dates))) + 1/12, by = "month", length.out = length(forecast_final$mean))

# Forecast DataFrame
fc_df <- data.frame(
  Date = fc_dates,
  Forecast = as.numeric(forecast_final$mean),
  Lo80 = forecast_final$lower[,"80%"],
  Hi80 = forecast_final$upper[,"80%"],
  Lo95 = forecast_final$lower[,"95%"],
  Hi95 = forecast_final$upper[,"95%"]
)

# Final Plot with lines
ggplot() +
  # Confidence Intervals
  geom_ribbon(data = fc_df, aes(x = Date, ymin = Lo95, ymax = Hi95), fill = "#a6bddb", alpha = 0.4) +
  geom_ribbon(data = fc_df, aes(x = Date, ymin = Lo80, ymax = Hi80), fill = "#67a9cf", alpha = 0.5) +
  
  # Forecast Line
  geom_line(data = fc_df, aes(x = Date, y = Forecast, color = "Forecast"), size = 0.8) +
  
  # Training and Fitted Lines
  geom_line(data = train_df, aes(x = Date, y = Training, color = "Training"), size = 0.6) +
  geom_line(data = train_df, aes(x = Date, y = Fitted, color = "Fitted"), size = 0.6) +
  
  # Styling
  scale_color_manual(name = "Model Data",
                     values = c("Training" = "black",
                                "Fitted" = "#2ca25f",
                                "Forecast" = "#fdbb84")) +
  
  labs(
    title = "Retail Sales Forecast with ARIMA(1,1,2)(2,1,1)[12]",
    x = "Production Months",
    y = "Monthly Sales (Millions of Dollars)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_line(color = "lightblue", linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    legend.position = "right"
  )

