#6a
raydataRead <- read.table("/Users/Molly/Documents/R/Math371/rayleigh.txt")
raydata <- raydataRead$V1
n <- length(raydata)
sigLev <- 0.01
zLev <- 1- (sigLev/2)
thetaHat <- sqrt(sum(raydata^2)/(2*n))

testStatVal <- abs((thetaHat - 1)/(thetaHat/(2*sqrt(n))))
zScore <- qnorm(zLev)

if(testStatVal > zScore){
  print("Reject H_0")
}else{
  print("Cannot reject H_0")
}

#6b
testStatVal <- (thetaHat - 1)/(thetaHat/(2*sqrt(n)))

pVal = 2*(1 - pnorm(testStatVal))
pVal


#7a
xbar <- 25.55
n <- 60
stat <- (xbar - 25)/(sqrt(xbar/60))
zval <- -qnorm(0.95)

if(stat < zval){
  print("Reject H_0")
}else{
  print("Cannot reject H_0")
}

#7b
stat <- (xbar - 25)/(sqrt(xbar/60))
zVal <- (1 - pnorm(stat))
zVal

#8a
nikedataRead <- read.table("/Users/Molly/Documents/R/Math371/nike.txt", header = TRUE)
pop1 <- nikedataRead$AD1
pop2 <- nikedataRead$AD2
n <- length(pop1)
alpha <- 0.05
cp <- 1- alpha/2
degf <- 2*n -2
xbarPop1 <- mean(pop1)
xbarPop2 <- mean(pop2)
sPop1 <- var(pop1)
sPop2 <- var(pop2)
stanDev <- sqrt((sPop2 + sPop1)/n)

testStat <- abs((xbarPop1 - xbarPop2)/stanDev)
zval <- qnorm(cp)
zval

#8b
cp2 <- 1- alpha
testStat <- (xbarPop1 - xbarPop2)/stanDev
zval <- qnorm(cp2)
zval

#8c
pVal <- 2*(1 - pnorm(testStat))
pVal

#9b
statistic <- (11-12)/sqrt(2/20)
zscore <- -qnorm(0.99)

#9c
1.5/sqrt(2/20) + zscore
beta <- 1-pnorm(1.5/sqrt(2/20) + zscore)
power <- 1- beta
power

#9d
neededN <- 8*(zscore)^2 /(9/4)
neededN