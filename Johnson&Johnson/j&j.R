library(astsa)
my.data = JohnsonJohnson
plot(my.data, col = 'blue', lwd=3)

#transformation
my.data.log = diff(log(my.data))
plot(my.data, col='blue', lwd= 3)

acf(my.data.log)
pacf(my.data.log)

#shift to mean zero
my.data = my.data.log - mean(my.data.log)
par(mfrow=c(3,1))
plot(my.data, main ='Mean zero plot')
acf(my.data, main='ACF of log zero data')
pacf(my.data, main ='PACF of log zero data')

p = 4
r= NULL
r[1:p] = acf(my.data, plot = F)$acf[2:(p+1)]

R = matrix(1, p, p)
for (i in (1:p)){
  for (j in (1:p)){
    if (i != j){
      R[i, j] = r[abs(i - j)]
    }
  }
}

R


b = NULL
b = matrix(r, p, 1)
b

phi.hat = solve(R, b)[,1]
phi.hat

c0 = acf(my.data, plot = F, type = 'covariance')$acf[1]
variance = c0 * (1 - sum(r * phi.hat))
variance

phi0.hat = mean(my.data.log) * (1 - sum(phi.hat))
phi0.hat

cat('constant :', phi0.hat, ' Coefficients :', phi.hat, ' and variance :', variance)


