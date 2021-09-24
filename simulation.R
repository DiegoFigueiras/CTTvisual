library(MASS)
library(tidyverse)
library(bindata)
library(mvtnorm)
library(psych)

data<-matrix(rnorm(2500), nrow=50)
data<-abs(cor(data))
data[,c(1:ncol(data))][data[,c(1:ncol(data))] < .2]<-data[,c(1:ncol(data))][data[,c(1:ncol(data))] < .2] +.1
data[,c(1:ncol(data))][data[,c(1:ncol(data))] > .145]<-data[,c(1:ncol(data))][data[,c(1:ncol(data))] > .145] +.3
data[,c(1:ncol(data))][data[,c(1:ncol(data))] > .4]<-data[,c(1:ncol(data))][data[,c(1:ncol(data))] > .4] +.3
data[,c(1:ncol(data))][data[,c(1:ncol(data))] > .54]<-data[,c(1:ncol(data))][data[,c(1:ncol(data))] > .54] +.3
data[,c(1:ncol(data))][data[,c(1:ncol(data))] >= 1]<-1
simulation<-rmvbin(100000, margprob=c(rep(0.5,50)), bincorr = data) 

core<-cor(simulation)


library(readr)
simluated_data <- read_csv("~/R/simluated_data.csv", 
                            col_names = FALSE)

core<-cor(simluated_data, use="complete.obs")
core<-na.omit(core)
alpha(simluated_data)
