my.data = read.csv('monthly-sales-for-a-souvenir-sho.csv')
names(my.data)
plot.ts(my.data$Sales)
my.data.sales = ts(my.data$Sales)
plot(my.data.sales)

#Diff
plot(diff(my.data.sales))
plot(diff(log(my.data.sales)))
plot(diff(diff(log(my.data.sales)), 12))


library(astsa)
library(forecast)
data = diff(diff(log(my.data.sales)), 12)
acf2(data, 50)

d = 1
period = 12
D = 1
for (p in (1 : 2)){
  for (q in (1 : 2)){
    for (i in (1 : 2)){
      for (j in (1: 4)){
          if (p + q + d + D + i + j <= 10){
            model = arima(x = log(my.data.sales), order = c(p - 1, d, q - 1), seasonal = list(order = c((i - 1), D, (j - 1)), period = period))
            pval = Box.test(model$residuals, lag = log(length(model$residuals)))
            sse = sum(model$residuals ^ 2)
            cat(p- 1, d, q - 1, i - 1, D, j - 1, period, 'AIC ', model$aic, 'SSE ', sse, 'p-value', pval$p.value, '\n')
          }
      }
    }
  }
}


model = arima(log(my.data.sales), order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))
plot(forecast(model))
sarima(log(my.data.sales), 1, 1, 0, 0, 1, 1, 12)
sarima = sarima.for(log(my.data.sales),12, 1, 1, 0, 0, 1, 1, 12)
plot.ts(c(my.data.sales, exp(sarima$pred)), main = 'Monlthly sales + forecast')


