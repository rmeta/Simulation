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
    r[i] = x[2:(n+1)]/m
  }
  return(r)
}

unirand(5,37) #you can change the s! 
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
    r1[i]= x[2:(n+1)]/m
  }

  return(r1)
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
unirand2(173,23)

#they are relatively prime and m is big so this generator has a maximal cycle length

#----------------------------------------------------------------------------------
#question3
unirand3 <- function(n, s1, s2) {
  u1 <- unirand(n, s1)
  u2 <- unirand2(n, s2)
  rand_nbrs <- u1 + u2 - floor(u1 + u2)
  return(rand_nbrs)
}
unirand3(10,37,23)

#Does the cycle length of this third generator exceed 300000?
length(unique(unirand3(700000,5,7)))
#the answer is no. Since the cycle length of both unirand and unirand2 generators
#is less than m = 30307, the cycle length of the unirand3 generator is also less than m.
#Therefore, the cycle length of this third generator does not exceed 300000.

x1 <- unirand3(2000, 20, 349)
hist(x1)
#It looks like they are uniformly distributed
lag.plot(x1)
#it looks like they are uniformly distributed as they can be find everywhere
z <- acf(x1, lag.max=300)
print(z)
#values are small(near to zero). when most of the lines are near zero, it typically
#indicates that there is little or no correlation between the values at different lags. 

#question4
library(randomForest)
library(scatterplot3d)
rftest1 <- function(m) {
  s1 <- 37
  s2 <- 23
  u <- unirand3(1000, s1, s2)
  n <- length(u) - 1
  A <- diag(rep(1, n))
  A <- rbind(rep(0, n), A)
  A <- cbind(A, rep(0, n + 1))
  xy <- matrix(u, nrow = n + 1)
  
  for (j in 1:m) {
    xy <- cbind(xy, A %*% xy[, j])
  }
  
  xy <- data.frame(xy)
  names(xy) <- c("y", paste("x", 1:m, sep = ""))
  
  par(mfrow=c(1,2))
  scatterplot3d(xy$x3,xy$x2, xy$x1,
                xlab="x3", ylab="x2", zlab="x1", cex.symbols=.3)
  scatterplot3d(xy$x3,xy$x2, xy$x1,
                xlab="x3", ylab="x2", zlab="x1", angle=150,
                cex.symbols=.3)
  
  xy <- xy[-(1:m), ]
  xytrain <- xy[1:(n/2), ]
  xytest <- xy[-(1:(n/2)), ]
  
  xy.rf <- randomForest(y ~ ., data = xytrain)
  
  par(mfrow = c(1, 2))
  
  # Plot for training data
  plot(predict(xy.rf), xytrain$y, cex = 0.3,
       xlab = "predicted values", ylab = "observed values",
       main = paste("Training data, m =", m))
  abline(lm(xytrain$y ~ predict(xy.rf)))
  
  # Plot for test data
  plot(predict(xy.rf, newdata = xytest), xytest$y, cex = 0.3,
       xlab = "predicted values", ylab = "observed values",
       main = paste("Test data, m =", m))
  abline(lm(xytest$y ~ predict(xy.rf, newdata = xytest)))
}

# Call the function with different values of m
r1 <-rftest1(10)
r2 <- rftest1(8)
r3 <- rftest1(5)

#A line with positive slope, particularly on the second plot, is an indicator
#of failure for the generator. m = 5 test data
#has a positive slope while the training is in a good shape!!( negative slope)
#test result are better most of the time and m = 8 has the better result usually 
#it can be seen that points could foster better in 3 dimension
print(rftest1(10))