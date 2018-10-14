rm(list = ls())
set.seed(43)
data = arima.sim(list(order = c(2, 0, 0), ar = c(0.7, 0.2)), n = 2000)

par(mfrow=c(2, 1))
acf(data, main='Auto corretion Function')
pacf(data, main='Partial Auto corretion Function')

