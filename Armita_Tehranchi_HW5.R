N <- 10000; U <- runif(N)
V <- sqrt(-log(1-U))
Vq <- quantile(V, prob = c(.15, .75))
Vq

rmyV <- function(n){
  u <- runif(n)
  x <- sqrt(-log(1-u))
  return(x)
}

#b
saving <- rmyV(10000)
hist(saving)
h <- hist(saving, breaks = 50, plot = FALSE)
cum_freq <- cumsum(h$counts)
#normalize
cum_dens <- cum_freq/sum(h$counts)
plot(h$mids, cum_dens, type="s")

distr_fun <- function(x){
  ifelse(x<=0 , 0, 1-exp(-x^2))
}

curve(distr_fun, add = TRUE, col = "blue", lwd = 2)
grid()


# Create a QQ-plot
qqplot(V, saving, main = "QQ-Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")  # after 2.5 it is not that much fit

#PDF
#fv = 2*x*exp^(-x^2)
u <- seq(0, 3, by = 0.01)
x2 <- 2*u*exp((-u^2))

plot(u, x2, type = "l", col = "red")
h <- hist(saving, breaks = 50, freq = FALSE, add = TRUE)

#it looks like that they act like each other
#3

rmyX <- function(n) {
  x <- runif(n)
  x <- x^3
  return(x)
}

cdff <- function(n) {
  x <- runif(n)
  x <- (1-exp(-x^2))
  return(x)
}

CDF_fun2 <- function(y, p) {
  return(p * cdff(y) + (1 - p) * rmyX(y))
}

#quantile function
rmyX2 <- function(x) {
  x <- runif(n)
  x <- x^(1/3)
  return(x)
}

rmyX3 <- function(p) {
  # Inverse of the CDF for Y using numerical approximation
  result <- uniroot(function(y) rmyX2(y) - p, interval = c(0, 1))
  return(result$root)
}

rmyY <- function(p, n){
  pp <- rbinom(n, 1, p )
  result <- pp*rmyV(n)+(1-pp)*rmyX2(n)
  return(result)
}



CDF <- function(y, p=0.4) {
  F1 <- ifelse(y > 0, (1 - exp(-y^2)),0)
  F2<- ifelse(y >= 0 & y <=1 , y^3 , 1)
  result <- p*F1+(1-p)*F2
  return(result)
  }

p1 = 0.4
n1 = 10000
i <- rmyY(p1, n1)

PDF <- function(y, p=0.4){
  F1 <- ifelse(y > 0, 2*y*exp(-y^2),0)
  F2<- ifelse(y >= 0 & y <=1 , 3*y^2 , 0)
  result<- p * F1 + (1 - p) * F2
  return(result)
}

#i
hist(i, freq = FALSE)
curve(PDF, from=0, to=3, add = TRUE, col = "blue", lwd = 2)
#ii
h2 <- hist(i, breaks = 50, plot = FALSE)
cum_freq2 <- cumsum(h2$counts)
#normalize
cum_dens2 <- cum_freq2/sum(h2$counts)
plot(h2$mids, cum_dens2, type="s")

curve(CDF, from=0, to=3, add = TRUE, col = "blue", lwd = 2)
grid()