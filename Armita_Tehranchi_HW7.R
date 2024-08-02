#armita Tehranchi
#27101237

#q1
set.seed(3663009)
eps <- rnorm(20, mean = 0, sd = 2.5)
x <- runif(20, 0, 3)
y <- 15 - 3.5*x + eps
xy <- data.frame(x=x,y=y)
print(xy)

#q2
usage.lm <- lm(y ~ x, data = xy)
summary(usage.lm)$coefficients

#intercept= 13.72
#slope = -3.13

#q3
plot(y ~ x, data = xy)
abline(usage.lm)
abline(15, -3.5, lty=2)

#q4
set.seed(3663009)
eps <- rnorm(20, mean = 0, sd = .5)
x <- runif(20, 0, 3)
y <- 15 - 3.5*x + eps
xy <- data.frame(x=x,y=y)
print(xy)
usage.lm <- lm(y ~ x, data = xy)
summary(usage.lm)$coefficients
# slope = -3.4 , intercepy = 14.7
plot(y ~ x, data = xy)
abline(usage.lm)
abline(15, -3.5, lty=2)

#now it looks much more integrated than before (less Outliers)
#and there is less error between
#the true line and the fitted line

#q5
myTS <- arima.sim(200, model = list(ar = c(0.4, -0.6)), sd = 3)

# Plotting the trace plot
ts.plot(myTS, main = "Trace Plot")

# Plotting the ACF plot
acf(myTS, main = "ACF Plot")

# Determine the autocorrelation at lag 2
autocorrelation_lag2 <- acf(myTS, lag.max = 2)$acf[3]
cat("Autocorrelation at lag 2:", autocorrelation_lag2, "\n")
# Data is negatively autocorrelated at lag 2
