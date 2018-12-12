library(forecast)
library(tseries)

yset <- read.csv("indonesia-life-expectancy.csv", header=TRUE)

x = yset$Year
y = yset$Life_Expectancy

ts_y <- ts(y, start=c(1945), end=c(2016), frequency=1)
plot(ts_y)

acf(y, plot = F)
acf(y)

pacf(y, plot = F)
pacf(y)

arima100 <- arima(y, order=c(1,0,0))
summary(arima100)

auto_arima <- auto.arima(y)
summary(auto_arima)

res <- residuals(arima100)
plot(res)
qqnorm(res)
qqline(res, col="red")

Box.test(res,type="Ljung-Box")

res1 <- residuals(auto_arima)
plot(res1)
qqnorm(res1)
qqline(res1,col="red")

Box.test(res1, type="Ljung-Box")

forecast(arima100)
plot(forecast(arima100))

fit_model <- fitted(arima100)
plot(fitted(arima100))

plot(ts_y)
lines(fit_model,col="red")