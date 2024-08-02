set.seed(111)

#q1
binsim <- rbinom(10000, 20, 0.3)

a <- mean(binsim <= 5)
#alternative
aa <- sum(binsim <= 5)/10000

# (b) P(X = 5)
b <- mean(binsim == 5)
#alternative
bb <- sum(binsim == 5)/10000

# (c) E[X]
c<- mean(binsim)

# (d) Var(X)
d <- var(binsim)

cat("a= ",a, "\n")
cat("b= ",b, "\n")
cat("c= ", c, "\n")
cat("d= ", d, "\n")

#q2
rPois <- function(n, lambda) {
  U <- runif(n)
  X <- numeric(n)
  x <- 0
  while (max(U) > ppois(x, lambda)) {
    X[U >= ppois(x, lambda)] <- x + 1
    x <- x+1
  }
  return(X)
}

n <- 10000; rate <- 7.2
pois <- rPois(n, rate)
#(a) mean
a2<- mean(pois)

#(b) Var
b2 <- var(pois)

cat("a2= ", a2, "\n")
cat("b2= ", b2, "\n")

#comparison
table(pois)/n - dpois(0:max(pois), rate)

#q3
p1 <- rpois(10000, 5)
p2 <- rpois(10000, 25)
p3 <- rpois(10000, 125)
p4 <- rpois(10000, 625)

a31<- mean(p1)
a32<- mean(p2)
a33<- mean(p3)
a34<- mean(p4)

a312<- sqrt(mean(p1))
a32222<- sqrt(mean(p2))
a332<- sqrt(mean(p3))
a342<- sqrt(mean(p4))
# Var

a321<- var(p1)
a322<- var(p2)
a323<- var(p3)
a324<- var(p4)

a3212<- var(sqrt(p1))
a3222<- var(sqrt(p2))
a3232<- var(sqrt(p3))
a3242<- var(sqrt(p4))

cat("E[X] for rates λ = 5, 25, 125 and 625 = ",a31, a32, a33, a34, "\n")
cat("E[radical(X)] for rates λ = 5, 25, 125 and 625 = ",a312, a32222, a332, a342, "\n")

cat("var[X] for rates λ = 5, 25, 125 and 625 = ",a321, a322, a323, a324, "\n")
cat("var[radical(X)] for rates λ = 5, 25, 125 and 625 = ",a3212, a3222, a3232, a3242, "\n")

#b 
# It makes data less spreadout. Therefore, it can compress 
#the variability without changing the data. It makes the variation more unifrom
# it  tends to make the variances more nearly equal without 
#destroying the difference in the means.

#q4
runifAvg <- function(N, n) {
  X <- matrix(runif(N*n),N,n)
  rowMeans(X)
}

N <- 10000
n <- c(3, 6, 10, 15, 30, 100)
par(mfrow=c(2, 3))
for (i in n){
  hist(runifAvg(10000,i), main = paste("Sample Size =", i), xlab = "Sample Average")
  
}

#As the sample size increases, the distribution becomes smoother and the histogram
#becomes more symmetric. Also it won't be that much spreaded, and with its growing it becomes
# more like a normal distribution. 

#q5
runifAvg2 <- function(N, n) {
  X <- matrix(rbinom(N*n, 1, 0.25),N,n)
  rowMeans(X)
}

N <- 10000
n2 <- c(2, 3, 6, 10, 15, 30, 100)
par(mfrow=c(2, 3))
for (j in n2){
  hist(runifAvg2(10000,j), main = paste("Sample Size =", j), xlab = "Sample Average")
  
}
# It becomes less spreaded when sample size increases and it get more like a bell shape .
#when it increases a lot (100 For example) it looks like a normal distribution
# I think yes because in the privious function we had bell shaped histogram almost from the
# smallest sample size while in the second one it shows a lot of variability

#q6
rate <- 1.5; time <- 100000
N <- rpois(1, lambda = rate*time)
PoissonPoints <- sort(runif(N, max = time))
PoissonPoints
stripchart(PoissonPoints, pch=16)

#q7
intervals <- seq(0, time, by = 1)
cutfunc <- cut(PoissonPoints, breaks = intervals,  right = FALSE)

counts <- table(cutfunc)
counts
sec <-dpois(0:max(PoissonPoints), 1.5)
compare <- table(cutfunc)/time - dpois(0:(time-1), 1.5)
compare
#poissonpoints is so much more spread in comparison with poisson distribution. 
