#Armita Tehranchi - 27101237
#HW2

#question1
unirand <- function(n, s) {
  m = 30307
  a = 172
  x <- numeric(min(m-1,n)) #creates an empty vector x of length min(m-1,n)
  x[1] <- s #sets the first element of x to the seed value s
  for (i in 1:min(m-1,n)){
    y <- x[i]
    x[i+1] <- (a*y)%%m
  }
  x[2:(n+1)]/m
}

unirand(5,1)
#Does this generator have a maximal cycle length?
#1-m should be prime
prime <- function(n) n == 2L || all(n %% 2L:max(2,floor(sqrt(n))) != 0)
m <- 30307
a <- 172
prime(m)

#2-m is not divisible by a
if ( m %% a != 0 & prime(m)) {
  print("relatively prime")
} else {
  print("not relatively prime")
}
#they are relatively prime and m is big so this generator has a maximal cycle length

#---------------------------------------------------------------------------------
#question2)
unirand2 <- function(n, s) {
  m2 = 30323
  a2 = 170
  x <- numeric(min(m-1,n)) #creates an empty vector x of length min(m-1,n)
  x[1] <- s #sets the first element of x to the seed value s
  for (i in 1:min(m-1,n)){
    y <- x[i]
    x[i+1] <- (a*y)%%m
  }
  x[2:(n+1)]/m
}

m2 = 30323
a2 = 170
#1st condition
prime(m2)
#second condition
if ( m2 %% a2 != 0 & prime(m)) {
  print("relatively prime")
} else {
  print("not relatively prime")
}
unirand2(173,3)
#they are relatively prime and m is big so this generator has a maximal cycle length

#----------------------------------------------------------------------------------
#question3
unirand3 <- function(n, s1, s2) {
  u1 <- unirand(n, s1)
  u2 <- unirand2(n, s2)
  rand_nbrs <- u1 + u2 - floor(u1 + u2)
  return(rand_nbrs)
}
unirand3(10,1,3)

#Does the cycle length of this third generator exceed 300000?
length(unique(unirand3(300000,2500,34000)))
#the answer is no. Since the cycle length of both unirand and unirand2 generators
#is less than m = 30307, the cycle length of the unirand3 generator is also less than m.
#Therefore, the cycle length of this third generator does not exceed 300000.

x1 <- unirand3(2000, 20, 349)
hist(x1)
#It looks like they are uniformly distributed
lag.plot(x1)
#it looks like they are uniformly distibuted as they can be find everywhere
z <- acf(x1, lag.max=300)
print(z)
#values are small(near to zero). when most of the lines are near zero, it typically
#indicates that there is little or no correlation between the values at different lags. 

#question4
library(randomForest)

rftest <- function(m=10) {
  n <- 2000
  s1 <- 40
  s2 <- 13
  X <- matrix(0, nrow = n, ncol = m)
  for (i in 1:m) {
    X[,i] <- unirand3(n, s1 + i, s2 + i)
  }
  Y <- apply(X, 1, sum)
  rf <- randomForest(Y ~ ., data = data.frame(X, Y))
  print(rf)
  par(mfrow=c(1,2))
  plot(predict(rf), Y, cex=.3,
       xlab="predicted values", ylab="observed values",
       main = "training data")
  abline(lm(Y ~ predict(rf)))
  plot(predict(rf, newdata=data.frame(X)), Y, cex=.3,
       xlab="predicted values", ylab="observed values",
       main="test data")
  abline(lm(Y ~ predict(rf, newdata=data.frame(X))))
}

rftest()
#A line with positive slope, particularly on the second plot, is an indicator
#of failure for the generator.

#The output shows that the model has performed well in fitting the data.So it can predict it wll...
#The mean of squared residuals is relatively low, measuring at 0.02805652.
#The high percentage of variance explained
#indicates that the regression model effectively accounts for a 
#significant portion of the data's variability.
#all of these means this is not a good generator