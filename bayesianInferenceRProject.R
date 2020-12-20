dataTable <- read.csv("/Users/Molly/Documents/USF/Math371/Math371R/players_stats.csv")

#you can change which variable to use by opening dataTable and replacing "BMI" with "Height", for example
#then Command+F to find heightData and replace all with heightData
heightData <- t(dataTable["Height"])
heightData <- heightData[!is.na(heightData)]

hist(heightData)

#Commonly thought that height follows a normal distribution
#N(200, 25) for nba players
x <-seq(from = 175, to =225)
plot(dnorm(x, mean = 200, sd = 5),type = "l")

xbar <- mean(heightData)
stan <- sqrt(var(heightData))
lines(dnorm(x, xbar, stan), col = "blue")
#update now to new model with Bayesian Inference

fn <- 1/(sqrt(2*pi)*5)*exp(-((x-200)^2)/50)
gn <-1/(sqrt(2*pi)*stan)*exp(-((x-xbar)^2)/(2*stan^2))
post <- fn*gn
lines(post, col = "green")