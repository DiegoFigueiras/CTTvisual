library(ggplot2)
library(gridExtra)
pseudob<- -.25
ahat<-function(x){
  r<-(((2.71828)^x)-(2.71828)^-x)/(2.71828-(2.71828)^x)
  ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)
  
}

c<-0
pseudoa<-ahat(.8)
p <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob))))))}
pseudob2<- -.2
p2 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob2))))))}

theta <- matrix(seq(-6,6, by=.1))
cttB<-p(seq(-6,6, by=.1))
irtB<-p2(seq(-6,6, by=.1))
f1 <- approxfun(theta, cttB-irtB)     # piecewise linear function
f2 <- function(x) abs(f1(x))                 # take the positive value
auc<-integrate(f2, -6,6)
auc

graph1<-ggplot()+
  xlim(-1,1)+
  labs(x=sprintf("Area between curves: %f",auc$value),
       y="P(1)")+
  geom_function(fun=p, size=3, color="black")+
  geom_function(fun=p, size=1.5, color="cyan")+
  geom_function(fun=p2, size=3, color="black")+
  geom_function(fun=p2, size=1.5, color="#F72119")
graph1
######################################################################################
pseudob1.2<- 0
ahat<-function(x){
  r<-(((2.71828)^x)-(2.71828)^-x)/(2.71828-(2.71828)^x)
  ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)
  
}

c<-0
pseudoa<-ahat(.8)
p1.2 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob1.2))))))}
pseudob2.2<- -.2
p2.2 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob2.2))))))}


theta <- matrix(seq(-6,6, by=.1))
cttB<-p1.2(seq(-6,6, by=.1))
irtB<-p2.2(seq(-6,6, by=.1))
f1 <- approxfun(theta, cttB-irtB)     # piecewise linear function
f2 <- function(x) abs(f1(x))                 # take the positive value
auc<-integrate(f2, -6,6)
auc
graph2<-ggplot()+
  xlim(-1,1)+
  labs(x=sprintf("Area between curves: %f",auc$value),
       y="P(1)")+
  geom_function(fun=p1.2, size=3, color="black")+
  geom_function(fun=p1.2, size=1.5, color="cyan")+
  geom_function(fun=p2.2, size=3, color="black")+
  geom_function(fun=p2.2, size=1.5, color="#F72119")
graph2

#########################################################################################

pseudob1.3<- .2
ahat<-function(x){
  r<-(((2.71828)^x)-(2.71828)^-x)/(2.71828-(2.71828)^x)
  ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)
  
}

c<-0
pseudoa<-ahat(.8)
p1.3 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob1.3))))))}
pseudob2.3<- -.2
p2.3 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob2.3))))))}

theta <- matrix(seq(-6,6, by=.1))
cttB<-p1.3(seq(-6,6, by=.1))
irtB<-p2.3(seq(-6,6, by=.1))
f1 <- approxfun(theta, cttB-irtB)     # piecewise linear function
f2 <- function(x) abs(f1(x))                 # take the positive value
auc<-integrate(f2, -6,6)
auc

graph3<-ggplot()+
  xlim(-1,1)+
  labs(x=sprintf("Area between curves: %f",auc$value),
       y="P(1)")+
  geom_function(fun=p1.3, size=3, color="black")+
  geom_function(fun=p1.3, size=1.5, color="cyan")+
  geom_function(fun=p2.3, size=3, color="black")+
  geom_function(fun=p2.3, size=1.5, color="#F72119")
graph3

library(egg)
ggarrange(graph1, graph2, graph3, nrow=1, ncol=3)  
