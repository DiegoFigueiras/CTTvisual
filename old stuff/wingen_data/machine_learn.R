## trying to find p-value to b linking parameters via machine learning (minimizing actual DIF) 6/1/22

data <- read.csv("wingen_data\\normal_0_1.csv", header=FALSE)[,-1]

ps <- psych::describe(data)$mean           ## isolating p-values

skew <- psych::describe(ps)$skew           ## using isolated values to quantify distributional properties
kurt <- psych::describe(ps)$kurtosis
## hist(ps)



mod <- mirt::mirt(data, itemtype="2PL")

