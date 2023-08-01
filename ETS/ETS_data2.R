library(readxl)
library(tidyverse)
library(descr)
library(psych)
library(mirt)
library(ggplot2)

df_ETS1 <- read_excel("ETS/ITP_Public Use Dataset_4LTN02A.xlsx")
df_ETS1[2:141][df_ETS1[2:141]=='M']<-NA
df_ETS1[2:141][df_ETS1[2:141]=='X']<-NA
df_ETS1[2:141][df_ETS1[2:141]=='N']<-NA

df_ETS1[2:141] <- mutate_all(df_ETS1[2:141], function(x) as.numeric(as.character(x)))

df_ETS1$LC1<-as.numeric(df_ETS1$LC1)
df_ETS1$check<-rowSums(!is.na(df_ETS1[2:141]))
set1<-df_ETS1%>%select(LC1:LC5, LC7:LC11, LC13:LC17, LC19:LC23, LC25:LC29, LC31:LC41, LC47:LC50)
set2<-df_ETS1%>%select(starts_with(c("SW")))
set2<-set2%>%select(-SW7, -SW15, -SW21, -SW28, -SW35)
set3<-df_ETS1%>%select(starts_with(c("RD")))
set3<-set3%>%select(RD1:RD29, RD41:RD50)




df_ETS <- read_excel("ETS/ITP_Public Use Dataset_4LTN06A.xlsx")
df_ETS[2:141][df_ETS[2:141]=='M']<-NA
df_ETS[2:141][df_ETS[2:141]=='X']<-NA
df_ETS[2:141][df_ETS[2:141]=='N']<-NA

df_ETS[2:141] <- mutate_all(df_ETS[2:141], function(x) as.numeric(as.character(x)))

df_ETS$LC1<-as.numeric(df_ETS$LC1)
df_ETS$check<-rowSums(!is.na(df_ETS[2:141]))
set4<-df_ETS%>%select(LC1:LC5, LC7:LC11, LC13:LC17, LC19:LC23, LC25:LC29, LC31:LC41, LC47:LC50)
set4<-set4[1:10000, 1:40]
set5<-df_ETS%>%select(starts_with(c("SW")))
set5<-set5%>%select(-SW7, -SW15, -SW21, -SW28, -SW35)
set5<-set5[1:10000, 1:35]
set6<-df_ETS%>%select(starts_with(c("RD")))
set6<-set6%>%select(RD1:RD10, RD22:RD50)
set6<-set6[1:10000, 1:39]


###################################################################################################################################################

set0<-read.csv("SIOP/simulated_data.csv")
irt_model<-mirt(set0, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set0, na.rm=TRUE))*-1#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set0)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations


pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df0<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df0)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df0$PseudoB<--0.000002895614 +(1.535589*df0$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB



## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, ncol(set0))

for (i in 1:nrow(df0)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df0$PseudoA[i]*(x-df0$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df0$a[i]*(x-df0$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc0<-unlist(auc)
# hist0<-hist(auc0)
# mean(auc0)








########################################################################################################################################################



irt_model<-mirt(set1, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set1, na.rm=TRUE))*-1#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set1)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations

pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df1<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df1)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df1$PseudoB<--0.000002895614 +(1.535589*df1$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB

ggplot(df1, aes(x=b, y=PseudoB))+
  geom_point()+
  geom_abline()

## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, ncol(set1))

for (i in 1:nrow(df1)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df1$PseudoA[i]*(x-df1$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df1$a[i]*(x-df1$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc1<-unlist(auc)
# hist1<-hist(auc1)
# mean(auc1)

########################################################################

irt_model<-mirt(set2, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set2, na.rm=TRUE))*-1#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set2)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations

pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df2<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df2)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df2$PseudoB<--0.000002895614 +(1.535589*df2$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB

ggplot(df2, aes(x=b, y=PseudoB))+
  geom_point()+
  geom_abline()

## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, ncol(set2))

for (i in 1:nrow(df2)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df2$PseudoA[i]*(x-df2$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df2$a[i]*(x-df2$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc2<-unlist(auc)
# hist2<-hist(auc2)
# mean(auc2)



#################################################################################################################################################



irt_model<-mirt(set3, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set3, na.rm=TRUE))*-1#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set3)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations

pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df3<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df3)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df3$PseudoB<--0.000002895614 +(1.535589*df3$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB

ggplot(df3, aes(x=b, y=PseudoB))+
  geom_point()+
  geom_abline()

## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, ncol(set3))

for (i in 1:nrow(df3)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df3$PseudoA[i]*(x-df3$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df3$a[i]*(x-df3$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc3<-unlist(auc)
# hist3<-hist(auc3)
# mean(auc3)

#######################################################################################################################################################


irt_model<-mirt(set4, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set4, na.rm=TRUE))*-1#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set4)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations

pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df4<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df4)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df4$PseudoB<--0.000002895614 +(1.535589*df4$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB

ggplot(df4, aes(x=b, y=PseudoB))+
  geom_point()+
  geom_abline()

## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, ncol(set4))

for (i in 1:nrow(df4)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df4$PseudoA[i]*(x-df4$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df4$a[i]*(x-df4$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc4<-unlist(auc)
# hist4<-hist(auc4)
# mean(auc4)

####################################################################################################################################################################


irt_model<-mirt(set5, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set5, na.rm=TRUE))*-1#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set5)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations

pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df5<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df5)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df5$PseudoB<--0.000002895614 +(1.535589*df5$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB

ggplot(df5, aes(x=b, y=PseudoB))+
  geom_point()+
  geom_abline()

## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, ncol(set5))

for (i in 1:nrow(df5)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df5$PseudoA[i]*(x-df5$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df5$a[i]*(x-df5$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc5<-unlist(auc)
# hist5<-hist(auc5)
# mean(auc5)

############################################################################################################################################################################



irt_model<-mirt(set6, 1, itemtype="2PL")
IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)#retrieving the IRT parameters from the mod object
irt <- IRT_parms$items
summary(irt_model)

pseudob<-qnorm(colMeans(set6, na.rm=TRUE))*-1#calculating our Zg
c<-0
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
  
  ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  
}#Formula taken from Kulas' 2017

alphas<-psych::alpha(set6)#using the psych package to run alpha

citcs<-data.frame(alphas$item.stats$r.drop)#getting the corrected-item total correlations

pseudoA<-data.frame(ahat(citcs))#Applying Kula's 2017 formula to our corrected-item totals
df6<-as.data.frame(cbind(citcs, pseudoA, pseudob, irt))

colnames(df6)<-c("CITC", "PseudoA", "PseudoB", "a", "b", "c1", "c2")

df6$PseudoB<--0.000002895614 +(1.535589*df6$PseudoB)#Using the regression coefficients computed on the simulations that converged on December 8 to modify our PseudoB

ggplot(df6, aes(x=b, y=PseudoB))+
  geom_point()+
  geom_abline()+
  ylim(-5,5)+
  xlim(-5,5)

## Lines 352-363 create curves using our parameters and calculate the area between curves plotted with CTT and IRT parameters
theta <- matrix(seq(-6,6, by=.1))
auc<-rep(NA, ncol(set6))

for (i in 1:nrow(df6)){
  eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df6$PseudoA[i]*(x-df6$PseudoB[i]))))))}
  cttB<-eq_CTT(seq(-6,6, by=.1))
  eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df6$a[i]*(x-df6$b[i]))))))}
  irtB<-eq_IRT(seq(-6,6, by=.1))
  f1 <- approxfun(theta, cttB-irtB)
  f2 <- function(x) abs(f1(x))          
  auc[i]<-integrate(f2, -6,6)
}
auc6<-unlist(auc)
#hist6<-hist(auc6)
#mean(auc6)

data0<-data.frame(
  diff=auc0,
  set="wingen_sim(k=100)"
)


data1<-data.frame(
  diff=auc1,
  set="TOEFL1-LC(k=40)"
)
data2<-data.frame(
  diff=auc2,
  set="TOEFL1-SW(k=35)"
)

data3<-data.frame(
  diff=auc3,
  set="TOEFL1-RD(k=39)"
)


data4<-data.frame(
  diff=auc4,
  set="TOEFL2-LC(k=40)"
)
data5<-data.frame(
  diff=auc5,
  set="TOEFL2-SW(k=35)"
)

data6<-data.frame(
  diff=auc6,
  set="TOEFL2-RD(k=39)"
)


df_plot2<-rbind(df1, df2, df3, df4, df5, df6)
df_plot<-cbind(
  rbind(data1, data2, data3, data4, data5, data6),
  ITEM_ID=rownames(df_plot2))

# df_plot2$r<-(((2.71828)^df_plot2$CITC)-(1/(2.71828)^df_plot2$CITC))/(2.71828-(2.71828)^df_plot2$CITC)
# 
# df_plot2$eq<-((0.51+(0.02*abs(df_plot2$PseudoB))+(0.301*df_plot2$PseudoB^2))*df_plot2$CITC)+((0.57-(0.009*abs(df_plot2$PseudoB))+(0.19*df_plot2$PseudoB^2))*df_plot2$r)



# ggplot(df_plot2, aes(x=a, y=eq))+
#   ylim(0,4)+
#   xlim(0,4)+
#   geom_point()
# 
# 
# ggplot(df5, aes(x=a, y=PseudoA))+
#   ylim(0,4)+
#   geom_point()
# 
# ggplot(df_plot2, aes(x=PseudoA, y=eq))+
#   ylim(0,4)+
#   geom_point()

# reg<-lm(PseudoA~a, data=df_plot2)
# summary(reg)

# df_plot2$temp[df_plot2$a>0.9 & df_plot2$a<=1.1]<-"1"
# df_plot2$temp[df_plot2$a>1.9 & df_plot2$a<=2.1]<-"2"
# df_plot2$temp[df_plot2$a>2.9 & df_plot2$a<=3.1]<-"3"
# df_plot2$temp[df_plot2$a>3.9 & df_plot2$a<=4.1]<-"4"
# avg_pseudoa<- df_plot2%>% group_by(temp)%>%summarise(PseudoA=mean(PseudoA))

# df_plot2$r<-(((2.71828)^(df_plot2$CITC))-(1/(2.71828)^(df_plot2$CITC)))/(2.71828-(2.71828)^(df_plot2$CITC))
# 
# df_plot2$eq<-((((0.51+(0.02*abs(df_plot2$PseudoB))+(0.301*df_plot2$PseudoB^2))*df_plot2$CITC)+((0.57-(0.009*abs(df_plot2$PseudoB))+(0.19*df_plot2$PseudoB^2))*df_plot2$r))*1.71633)
# 
# 
# 
# 
# ggplot(df_plot2, aes(x=b, y=PseudoB))+
#   ylim(0,2)+
#   xlim(0,2)+
#   geom_point()+
#   geom_abline()
# reg<-lm(a~eq, data=df_plot2)
# summary(reg)


###################### ############################################################################

# par(mfrow=c(2,4))
# plot(hist0)
# plot(hist1)
# plot(hist2)
# plot(hist3)
# plot(hist4)
# plot(hist5)
# plot(hist6)

tbl<-data.frame(
  auc=c("dif0","dif1","dif2","dif3","dif4","dif5","dif6"),
  mean=c(mean(auc0),mean(auc1),mean(auc2),mean(auc3),mean(auc4),mean(auc5),mean(auc6)),
  sd=c(sd(auc0),sd(auc1),sd(auc2),sd(auc3),sd(auc4),sd(auc5),sd(auc6))
)

#df_plot[df_plot$diff>0.1104068 & df_plot$diff<=0.1166488,]

eq_CTT<- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df1$PseudoA[3]*(x-df1$PseudoB[3]))))))}
cttB<-eq_CTT(seq(-6,6, by=.1))
eq_IRT<-function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(df1$a[3]*(x-df1$b[3]))))))}
irtB<-eq_IRT(seq(-6,6, by=.1))

curve(eq_IRT, xlim=c(-4,4), xlab="Level of Trait", ylab="p(1.0)", type="p")
curve(eq_CTT, xlim=c(-4,4), xlab="Level of Trait", ylab="p(1.0)", add=TRUE)

