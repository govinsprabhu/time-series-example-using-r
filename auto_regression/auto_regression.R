set.seed(2016)
N= 1000
phi = 1
Z = rnorm(N, 0, 1)
X=NULL
X[1] = Z[1]
for (i in 2:N){
  X[i] = Z[i] + phi *X[i - 1]
}
X.ts = ts(X)
plot(X.ts)
par(mfrow=c(2,1))
plot(X.ts, main='Ar(1) time series on white noise, phi = 0.4')
X.acf = acf(X.ts, main="AR(1) time series on white noise, phi = .4")
plot(X.acf)

#alternative
set.seed(2016)
X.ts = arima.sim(list(ar=c(.7,.2)), n= 1000)
par(mfrow=c(2,1))
plot(X.ts, main = 'AR(2) time seris, phi1=.7, phi2=.2')
X.acf= acf(X.ts, main = 'Autocorrelation of AR(2) time series')