library(forecast)
library(tseries)

data_penjualan <- read.csv("D:/bab8.csv", header=FALSE)

ts_data <- ts(data_penjualan)
plot(ts_data)

acf(data_penjualan, plot = F)
acf(data_penjualan)

pacf(data_penjualan, plot = F)
pacf(data_penjualan)

arima100 <- arima(data_penjualan, order=c(1,0,0))
summary(arima100)

auto_elektronik <- auto.arima(data_penjualan)
summary(auto_elektronik)

res <- residuals(arima100)
plot(res)
qqnorm(res)
qqline(res, col="red")

Box.test(res,type="Ljung-Box")

res1 <- residuals(auto_elektronik)
plot(res1)
qqnorm(res1)
qqline(res1,col="red")

Box.test(res1, type="Ljung-Box")

forecast(arima100)
plot(forecast(arima100))

fit_model <- fitted(arima100)
plot(fitted(arima100))

plot(ts_data)
lines(fit_model,col="red")
