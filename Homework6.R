#for n = 10
n <- 10
vect <- c(rep(0, 1000))

for(i in 1:1000){
  expVals <- rexp(n, rate = 1/2)
  xbar <- mean(expVals)
  vect[i] <- (1/xbar)
}

hist(vect, main = "1000 Samples With N = 10", xlab = "x values", ylab = "1/xbar values", freq = FALSE)
sd <- 1/2*sqrt(1/n)
curve(dnorm(x, mean = 1/2, sd = sd), add = TRUE, col = "blue")

####
#for n = 100
n <- 100
vect <- c(rep(0, 1000))

for(i in 1:1000){
  expVals <- rexp(n, rate = 1/2)
  xbar <- mean(expVals)
  vect[i] <- (1/xbar)
}

hist(vect, main = "1000 Samples With N = 100", xlab = "x values", ylab = "1/xbar values", freq = FALSE)
sd <- 1/2*sqrt(1/n)
curve(dnorm(x, mean = 1/2, sd = sd), add = TRUE, col = "green")    

####
#for n = 1000
n <- 1000
vect <- c(rep(0, 1000))

for(i in 1:1000){
  expVals <- rexp(n, rate = 1/2)
  xbar <- mean(expVals)
  vect[i] <- (1/xbar)
}

hist(vect, main = "1000 Samples With N = 1000", xlab = "x values", ylab = "1/xbar values", freq = FALSE)
sd <- 1/2*sqrt(1/n)
curve(dnorm(x, mean = 1/2, sd = sd), add = TRUE, col = "red")

####
#for n = 5000
n <- 5000
vect <- c(rep(0, 1000))

for(i in 1:1000){
  expVals <- rexp(n, rate = 1/2)
  xbar <- mean(expVals)
  vect[i] <- (1/xbar)
}

hist(vect, main = "1000 Samples With N = 5000", xlab = "x values", ylab = "1/xbar values", freq = FALSE)
sd <- 1/2*sqrt(1/n)
curve(dnorm(x, mean = 1/2, sd = sd), add = TRUE, col = "blue")

##As n gets bigger, the curve fits the distribution better. This is visual evidence to the claim that
##the limiting distribution is most accurate as n approaches infinity