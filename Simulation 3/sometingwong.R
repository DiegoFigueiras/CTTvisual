## Trying to figure out weird rows 

all_sims3<-read.csv("Simulation 3/all_sims3.csv")
all_sims3$Simulations<-rep(1:1000, each=100)

library(ggplot2)

ggplot(all_sims3[1:5000,], aes(x=b, y=pseudob, color=Simulations)) +
  geom_point()

hist(all_sims3$a)


all_sims4<-read.csv("Simulation 4/all_sims4.csv")
hist(all_sims4$a)
