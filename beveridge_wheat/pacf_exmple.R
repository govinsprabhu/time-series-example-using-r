beveridge = read.table('beveridge_wheat.txt')
beveridge.ts =ts(beveridge[,2], start=1500)
plot(beveridge.ts, main ='Beveridge wheat data', ylab = 'price')
beveridge.Ma = filter(beveridge.ts, rep(1/15, 15), sides = 2)
lines(beveridge.Ma,col='red')

par(mfrow=c(3,1))
y = beveridge.ts / beveridge.Ma
plot(y, ylab = 'Scaled price', main='Transformed beveridge wheat price data')

acf(na.omit(y), main='Auto correlation function of transformed beveridge data', type = 'partial')
ar(na.omit(y), order.max =5)


