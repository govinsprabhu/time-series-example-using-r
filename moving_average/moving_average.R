noise = rnorm(100000)

moving_average = NULL
for (i in 3: 100000){
  moving_average[i] =noise[i] +  0.5 * noise[i - 1] + 0.5 * noise[i - 2]
}
moving_average = moving_average[3:100000]
moving_average_ts = ts(moving_average)
par(mfrow=c(2,1))

plot(moving_average_ts, main='time series ', xlab='days')
(acf(moving_average_ts, main='Moving average of a process of order 2'))

