library(astsa)
my.data = rec

plot(my.data, col='red', main='recruitment time series data')
mean(my.data)

ar.process = my.data - mean(my.data)
par(mfrow=c(2,1))
acf(ar.process, main='Recruitment', col='red', lwd=3)
pacf(ar.process, main = 'Recruitment', col = 'green', lwd=3)

p = 2
r = NULL
r[1:p] = acf(ar.process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')


R = matrix(1, p, p)
for (i in (1:p)){
  for(j in (1:p)){
    if (i != j){
      R[i, j] = r[abs(i - j)]
    }
  }
}


b = NULL
b = matrix(r, p, 1)
b


phi.hat = NULL
phi.hat = solve(R, b)[,1]
phi.hat

c0 = acf(ar.process, type = 'covariance', plot=F)$acf[1]
c0

var.hat = c0 * (1 - sum(phi.hat *r))
var.hat

phi0.hat = mean(my.data) * (1 - sum(phi.hat))
phi0.hat

cat('constant:', phi0.hat, ' Coeffients ', phi.hat, ' and variance ', var.hat)


