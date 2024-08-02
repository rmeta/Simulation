#Armita Tehranchi
#SN = 27101237
set.seed(123)
#question 1
rsin <- function(n){
  U <- runif(n, max = pi)
  V <- 0.5*runif(n)
  accept <- (V<0.5*sin(U))
  return(U[accept])
}

X <- rsin(2000)
hist(X, freq= FALSE, main=" ")
curve(0.5*sin(x), 0, pi, add=TRUE, col= "red" )

#question 7
n <- 100
X <- rexp(n)
Y <- rexp(n, rate = X)

#question 8
plot(X, Y, main="Scatterplot", xlab="X", ylab="Y", pch=16, col="blue")
curve(1/x, add=TRUE, col="red", lty=2)

#question 9
hist(Y, breaks=30, freq=FALSE, main="Relative Frequency Histogram and Marginal PDF", xlab="Y", ylab="Density", col="lightgreen")
curve(1/(1+x)^2, add=TRUE, col="purple", lty=2)

# Add legend
legend("topright", legend=c("Simulated Data", "Conditional Expected Value", "Marginal PDF"), col=c("blue", "red", "purple"), lty=1:2, cex=0.8)
