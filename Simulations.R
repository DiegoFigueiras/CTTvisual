
library(descr)
library(ggplot2)         ## to visualize p-value distributions

###################### SIMULATION 1: ALL 0.5 ##############################
x<-data.frame(matrix(runif(10000, 0, 1), ncol=100, nrow=10000))
sim1<-data.frame(ifelse(x<0.5, 0, 1))
sim1means<-as.data.frame(colMeans(sim1))
colnames(sim1means)[1] <- "pvalues"

sim1means$cond<-"One"


###################### SIMULATION 2: UNIFORM DISTRIBUTION ##############################

x<-data.frame(matrix(runif(10000, 0, 1), ncol=100, nrow=10000))
sim2<-matrix(ncol=100, nrow=10000)
for(i in 1:100){
  sim2[,i]<-ifelse(x[,i]<0.01*i,0,1)
  if(sum(sim2[,i])==0){
    sim2[,i][i]<-1
  }
  if(sum(sim2[,i])==10000){
    sim2[,i][i]<-0
  }
}

sim2means<-as.data.frame(colMeans(sim2))
colnames(sim2means)[1] <- "pvalues"

sim2means$cond<-"Two"


###################### SIMULATION 3: NORMAL DISTRIBUTION ##############################
y<-data.frame(matrix(rnorm(100,.5,.2)))
x<-data.frame(matrix(rnorm(10000,0,1), ncol=100, nrow=10000))
x2<-data.frame(matrix(rnorm(10000,0,1), ncol=100, nrow=10000))
sim3<-matrix(ncol=100, nrow=10000)

for(i in 1:100){
  sim3[,i]<-sample(0:1, 10000, prob=c(y[i,], abs(y[i,]-1)), replace=TRUE)
}


colnames(sim3means)[1] <- "pvalues"
sim3means<-as.data.frame(colMeans(sim3))
sim3means$cond<-"Three"




###################### SIMULATION 4: INVERTED DISTRIBUTION ##############################

x<-data.frame(matrix(rbeta(10000, 5, 2), ncol=50, nrow=10000))
sim4.1<-matrix(ncol=50, nrow=10000)
for(i in 1:ncol(x)){
  
  sim4.1[,i]<-ifelse(x[,i]<(0.02*i), 0,1)
  if(sum(sim4.1[,i])==0){
    sim4.1[,i][i]<-1
  }
  if(sum(sim4.1[,i])==10000){
    sim4.1[,i][i]<-0
  }
}
sim4.2<-matrix(ncol=50, nrow=10000)
for(i in 1:ncol(x)){
  
  sim4.2[,i]<-ifelse(x[,i]>(0.02*i), 0,1)
  if(sum(sim4.2[,i])==0){
    sim4.2[,i][i]<-1
  }
  if(sum(sim4.2[,i])==10000){
    sim4.2[,i][i]<-0
  }
}
sim4<-cbind(sim4.1, sim4.2)
sim4means<-as.data.frame(colMeans(sim4))
colnames(sim4means)[1] <- "pvalues"

sim4means$cond<-"Four"



###################### SIMULATION 5: SKEWED NEGATIVE ##############################


x<-data.frame(matrix((rbeta(10000,5,2)), ncol=100, nrow=10000))
sim5<-matrix(ncol=100, nrow=10000)
for(i in 1:ncol(x)){
  
  sim5[,i]<-ifelse(x[,i]<(0.01*i), 0,1)
  if(sum(sim5[,i])==0){
    sim5[,i][i]<-1
  }
  if(sum(sim5[,i])==10000){
    sim5[,i][i]<-0
  }
}
sim5means<-as.data.frame(colMeans(sim5))
colnames(sim5means)[1] <- "pvalues"

sim5means$cond<-"Five"



###################### SIMULATION 6: SKEWED POSITIVE ##############################

x<-data.frame(matrix((rbeta(10000,5,2)), ncol=100, nrow=10000))
sim6<-matrix(ncol=100, nrow=10000)
for(i in 1:ncol(x)){
  
  sim6[,i]<-ifelse(x[,i]>(0.01*i), 0,1)
  if(sum(sim6[,i])==0){
    sim6[,i][i]<-1
  }
  if(sum(sim6[,i])==10000){
    sim6[,i][i]<-0
  }
}
sim6means<-as.data.frame(colMeans(sim6))
colnames(sim6means)[1] <- "pvalues"

sim6means$cond<-"Six"

##################################  Plot  ################################################

together <- rbind(sim1means, sim2means, sim3means, sim4means, sim5means, sim6means)


ggplot(together, aes(x=pvalues)) + geom_histogram() + facet_grid(cond ~ .)



################################ Regressions for simulation 2 #######################################

pseudob<-data.frame(qnorm(sim2means$pvalues))

ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)+((0.57-(0.009*pseudob)+(0.19*pseudob^2))*r)
  
}
library(psych)

alphas<-alpha(sim2)
pseudoA<-data.frame(ahat(alphas$item.stats$r.drop))

mod<-mirt(data.frame(sim2), 1, itemtype="2PL")
IRT_parms <- coef(mod, IRTpars = TRUE, simplify = TRUE)
irt <- IRT_parms$items


                    


























