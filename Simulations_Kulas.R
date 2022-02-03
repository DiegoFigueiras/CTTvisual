
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
random<-sample(10000, 500)
for(i in 1:100){
  sim2[,i]<-ifelse(x[,i]<0.01*i,0,1)
  if(sum(sim2[,i])==0){
    sim2[,i][random]<-1

    
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


sim3means<-as.data.frame(colMeans(sim3))
colnames(sim3means)[1] <- "pvalues"
sim3means$cond<-"Three"




###################### SIMULATION 4: INVERTED DISTRIBUTION ##############################

x<-data.frame(matrix(rbeta(10000, 5, 2), ncol=50, nrow=10000))
sim4.1<-matrix(ncol=50, nrow=10000)
for(i in 1:ncol(x)){
  
  sim4.1[,i]<-ifelse(x[,i]<(0.02*i), 0,1)
  if(sum(sim4.1[,i])==0){
    sim4.1[,i][random]<-1
  }
  if(sum(sim4.1[,i])==10000){
    sim4.1[,i][random]<-0
  }
}
sim4.2<-matrix(ncol=50, nrow=10000)
for(i in 1:ncol(x)){
  
  sim4.2[,i]<-ifelse(x[,i]>(0.02*i), 0,1)
  if(sum(sim4.2[,i])==0){
    sim4.2[,i][random]<-1
  }
  if(sum(sim4.2[,i])==10000){
    sim4.2[,i][random]<-0
  }
}
sim4<-cbind(sim4.1, sim4.2)
sim4means<-as.data.frame(colMeans(sim4))
colnames(sim4means)[1] <- "pvalues"

sim4means$cond<-"Four"



###################### SIMULATION 5: SKEWED NEGATIVE ##############################


x<-data.frame(matrix((rbeta(10000,2,1)), ncol=100, nrow=10000))
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

x<-data.frame(matrix((rbeta(10000,2,1)), ncol=100, nrow=10000))
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





############################## Regression for simulation 1 #########################################


pseudob<-data.frame(qnorm(sim1means$pvalues))
pvalues<-data.frame(sim1means$pvalues)

ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)+((0.57-(0.009*pseudob)+(0.19*pseudob^2))*r)
  
}
library(psych)

alphas<-alpha(sim1)
pseudoA<-data.frame(ahat(alphas$item.stats$r.drop))

mod<-mirt(data.frame(sim1), 1, itemtype="2PL")
IRT_parms <- coef(mod, IRTpars = TRUE, simplify = TRUE)
irt <- IRT_parms$items
df<-as.data.frame(cbind(pseudob, pvalues,pseudoA, irt))
colnames(df)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")

reg<-lm(b ~ pseudob, df)
summary(reg)
coef(reg)
reg2<-lm(b ~ pvalues, df)
summary(reg2)
coef(reg2)

library(tidyverse)
df%>%ggplot(aes(x=b, y=pvalues))+
  geom_point()+
  geom_text(aes(label=pvalues))





################################ Regressions for simulation 2 #######################################

pseudob2<-data.frame(qnorm(sim2means$pvalues))
pvalues2<-data.frame(sim2means$pvalues)

ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((0.51+(0.02*pseudob2)+(0.301*pseudob2^2))*r)+((0.57-(0.009*pseudob2)+(0.19*pseudob2^2))*r)
  
}
library(psych)

alphas<-alpha(sim2)
pseudoA2<-data.frame(ahat(alphas$item.stats$r.drop))

mod2<-mirt(data.frame(sim2), 1, itemtype="2PL")
IRT_parms2 <- coef(mod2, IRTpars = TRUE, simplify = TRUE)
irt2 <- IRT_parms2$items
df2<-as.data.frame(cbind(pseudob2, pvalues2,pseudoA2, irt2))
colnames(df2)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")

reg<-lm(b ~ pseudob, df2)
summary(reg)
coef(reg)
reg2<-lm(b ~ pvalues, df2)
summary(reg2)
coef(reg2)
                    
library(tidyverse)
df%>%ggplot(aes(x=b, y=pvalues))+
  geom_point()+
  geom_text(aes(label=pvalues))



################################# Regression for simulation 3 ####################################

pseudob3<-data.frame(qnorm(sim3means$pvalues))
pvalues3<-data.frame(sim3means$pvalues)

ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((0.51+(0.02*pseudob3)+(0.301*pseudob3^2))*r)+((0.57-(0.009*pseudob3)+(0.19*pseudob3^2))*r)
  
}
library(psych)

alphas<-alpha(sim3)
pseudoA3<-data.frame(ahat(alphas$item.stats$r.drop))

mod3<-mirt(data.frame(sim3), 1, itemtype="2PL")
IRT_parms3 <- coef(mod3, IRTpars = TRUE, simplify = TRUE)
irt3 <- IRT_parms3$items
df3<-as.data.frame(cbind(pseudob3, pvalues3,pseudoA3, irt3))
colnames(df3)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")

reg<-lm(b ~ pseudob, df3)
summary(reg)
coef(reg)
reg2<-lm(b ~ pvalues, df3)
summary(reg2)
coef(reg2)

library(tidyverse)
df%>%ggplot(aes(x=b, y=pvalues))+
  geom_point()+
  geom_text(aes(label=pvalues))


################### Regression for simulation 4 ###########################################

pseudob4<-data.frame(qnorm(sim4means$pvalues))
pvalues4<-data.frame(sim4means$pvalues)

ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((0.51+(0.02*pseudob4)+(0.301*pseudob4^2))*r)+((0.57-(0.009*pseudob4)+(0.19*pseudob4^2))*r)
  
}
library(psych)

alphas4<-alpha(sim4)
pseudoA4<-data.frame(ahat(alphas4$item.stats$r.drop))

mod4<-mirt(data.frame(sim4), 1, itemtype="2PL")
IRT_parms4 <- coef(mod4, IRTpars = TRUE, simplify = TRUE)
irt4 <- IRT_parms4$items
df4<-as.data.frame(cbind(pseudob4, pvalues4,pseudoA4, irt4))
colnames(df4)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")

reg<-lm(b ~ pseudob, df4)
summary(reg)
coef(reg)
reg2<-lm(b ~ pvalues, df4)
summary(reg2)
coef(reg2)

library(tidyverse)
df4%>%ggplot(aes(x=b, y=pvalues))+
  geom_point()+
  geom_text(aes(label=pvalues))

















