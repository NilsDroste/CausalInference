# an approaches to estimate RDDs with a readily available package

# set seed to ensure replicability
set.seed(1379)

x <-runif(1000,-1,1)
cov<-rnorm(1000)
y <-3+2*x+3*cov+10*(x>=0)+rnorm(1000)
test <- RDestimate(y~x)
summary(test)
# Efficiency gains can be made by including covariates
test2 <- RDestimate(y~x|cov)
summary(test2)

plot(test)
plot(test2)

bwIK <- IKbandwidth(x, y, cutpoint = NULL, verbose = FALSE, kernel = "triangular")

test3 <-  RDestimate(y~x|cov, bw = bwIK, se.type = "HC1")
plot(test3)
summary(test3)
