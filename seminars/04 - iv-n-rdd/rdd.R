# two approaches to estimate RDDs with readily available packages

library(rdrobust)

# set seed to ensure replicability
set.seed(123)

x<-runif(1000,-1,1)
y<-5+3*x+2*(x>=0)+rnorm(1000)
rdrobust(y,x, kernel = "uniform")

rdplot(y,x, kernel = "uniform")


library(rdd)

# set seed to ensure replicability
set.seed(1379)

x2 <-runif(1000,-1,1)
cov<-rnorm(1000)
y2 <-3+2*x+3*cov+10*(x>=0)+rnorm(1000)
test <- RDestimate(y2~x2)
# Efficiency gains can be made by including covariates
test2 <- RDestimate(y2~x2|cov)

plot(test)
plot(test2)

bwIK <- IKbandwidth(x2, y2, cutpoint = NULL, verbose = FALSE, kernel = "triangular")

test3 <-  RDestimate(y2~x2|cov, bw = bwIK, se.type = "HC1")
plot(test3)
test3
