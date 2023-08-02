library(readxl)
library(tidyverse)
library(descr)
library(psych)
library(mirt)
df_ETS <- read_excel("ETS/ITP_Public Use Dataset_4LTN02A.xlsx")
df_ETS[2:141][df_ETS[2:141]=='M']<-NA
df_ETS[2:141][df_ETS[2:141]=='X']<-NA
df_ETS[2:141][df_ETS[2:141]=='N']<-NA

df_ETS[2:141] <- mutate_all(df_ETS[2:141], function(x) as.numeric(as.character(x)))

df_ETS$LC1<-as.numeric(df_ETS$LC1)
df_ETS$check<-rowSums(!is.na(df_ETS[2:141]))
set1<-df_ETS%>%select(LC1:LC5, LC7:LC11, LC13:LC17, LC19:LC23, LC25:LC29, LC31:LC41, LC47:LC50)
set2<-df_ETS%>%select(starts_with(c("SW")))
set2<-set2%>%select(-SW7, -SW15, -SW21, -SW28, -SW35)
set3<-df_ETS%>%select(starts_with(c("RD")))
set3<-set3%>%select(RD1:RD29, RD41:RD50)
psych::alpha(set1)

df1<-cbind(set1, set2, set3)

irt_model<-mirt(set1, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set1, na.rm=TRUE))#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)+((0.57-(0.009*pseudob)+(0.19*pseudob^2))*r)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set1)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations

pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df$PseudoB<-0.000006957584+(-1.52731*df$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB

## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, nrow(set1))

for (i in 1:nrow(df)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df$PseudoA[i]*(x-df$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df$a[i]*(x-df$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc<-unlist(auc)
hist(auc)

library(psych)
efa<-fa(set1, nfactors=1, rotate="varimax")
fa.parallel(set1)
fa.parallel(set2)
fa.parallel(set3)

library(mirt)
irt_model<-mirt(set1, 1, itemtype="2PL")
itemplot(irt_model, type="trace", which.items=1:2, facet_items=FALSE)
plot(irt_model, type='trace',face_items=FALSE)
