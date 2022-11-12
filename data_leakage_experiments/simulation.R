#This script performs the simulation for an example exchange rate time series

set.seed(3)
myTS <- cumsum(rnorm(2000))
plot(rev(myTS), type="l", ylab="share price", xlab="time")

# save the series to file
write.csv(rev(myTS), "./data/simulated_exchange_rate/ts.csv", row.names = FALSE)