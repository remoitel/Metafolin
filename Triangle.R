library(triangle)

##  Number of iterations
n <- 10000

##  randomly sample inputs using triangular distribution
A <- rtriangle(n,1,40,10)
A1 <- rnorm(n,1,0.3)
B <- rtriangle(n,5,10,8)
X <- A1*B
C <- rtriangle(n,0,5,3)
D <- rtriangle(n,5,20,10)
Y <- C*D

Z <- X+Y

## histograms show outputs are not normally distributed
hist(X)
hist(Y)
hist(Z)

outputs <- data.frame(cbind(X,Y,Z))

## Calculate the mean, median and quantiles for X,Y,Z
means <- apply(outputs,2,mean)
median <- apply(outputs,2,median)
lower_quant <- apply(outputs,2,quantile,0.05)
upper_quant <- apply(outputs,2,quantile,0.95)
