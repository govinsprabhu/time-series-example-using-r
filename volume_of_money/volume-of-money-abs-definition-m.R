data = read.csv('volume-of-money-abs-definition-m.csv')
plot(data)
data.money = ts(data$Volume.of.money..ABS.definition.m1..Feb.1960...Dec.1994,start =c(1962, 2)  ,frequency = 12)
plot(data.money)
HoltWinters(data.money, gamma = FALSE)

plot(AirPassengers)