
#https://hub.coursera-notebooks.org/user/aoxvbylmwaqclwrrtiuvvc/notebooks/Daily%2Bfemale%2Bbirth.ipynb
birth.data = read.csv('data.csv')
names(birth.data)
colnames(birth.data)
plot.ts(file)
number_of_births = birth.data$Daily.total.female.births.in.California..1959
birth.data$Date = as.Date(birth.data$Date, '%m/%d/%Y')
plot.ts(number_of_births, main='Daily birth in California, 1959', ylab='Number of birth')
Box.test(number_of_births, lag = log(length(number_of_births)))


plot(number_of_births)
# found trend, removing by taking the difference
plot.ts(diff(number_of_births), main = 'Difference series', ylab='')
Box.test(diff(number_of_births))
par(mfrow=c(1,1))
dev.new(width=5,height=4)
acf(diff(number_of_births),50)
pacf(diff(number_of_births), 50)


#model the arima
model1= arima(number_of_births, order = c(0, 0, 1))
sse1 = sum(model1$residuals^2)
#sse - sum of square error
model1.test = Box.test(model1$residuals, lag = log(length(model1$residuals)))

model2 = arima(number_of_births, order = c(0, 1, 2))
sse2 = sum(model2$residuals^2)
model2.test = Box.test(model2$residuals, lag = log(length(model2$residuals)))


model3 = arima(number_of_births, order = c(7, 1, 1))
sse3 = sum(model3$residuals^2)
model3.test = Box.test(model3$residuals^2, lag = log(length(model3$residuals)))


model4 = arima(number_of_births, order = c(7, 1, 2))
sse4 = model4$residuals^2
model4.test = Box.test(number_of_births, lag = log(length(number_of_births)))


df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, sse1, model1.test$p.value), 
               c(model2$aic, sse2, model2.test$p.value), c(model3$aic, sse3, model3.test$p.value),
               c(model4$aic, sse4, model4.test$p.value))
colnames(df)<-c('Arima(0,1,1)','Arima(0,1,2)', 'Arima(7,1,1)', 'Arima(7,1,2)')

library(astsa)
sarima(number_of_births,0, 1, 2, 0,0,0)































