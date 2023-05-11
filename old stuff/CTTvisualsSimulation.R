set.seed(3453)
exam<- data.frame(item1=sample(0:1,prob=c(0.6,0.4),6000,replace=TRUE),
                  item2=sample(0:1,prob=c(0.7,0.3), 6000,replace=TRUE),
                  item3=sample(0:1,prob=c(0.8,0.2),6000,replace=TRUE),
                  item4=sample(0:1,prob=c(0.9,0.1),6000,replace=TRUE),
                  item5=sample(0:1,prob=c(0.4,0.6),6000,replace=TRUE),
                  item6=sample(0:1,prob=c(0.2,0.8),6000,replace=TRUE)
)

# dif<- difGenLogistic(exam, group="group", focal.name="A")
# plot(dif, type="trace", face_items=FALSE)
# 
# dif2<-difLord(exam, group="group",focal.name="A", model="2PL")
# plot(dif2)
library(mirt)
library(psych)

## IRT METHOD

model<-"personality=1-6"
exam<-exam[1:6]
results <- mirt(data=exam, model=model, SE= TRUE)
plot(results, type='trace',face_items=FALSE)

## CTT METHOD

# exam$total<-rowMeans(exam)
# exam$zscore<-scale(exam$total)
# logreg<-glm(item1~ total, data=exam, family="binomial")
# summary(logreg)
# prob<-data.frame(logreg$coefficients)
# plot(exam$total, exam$item1)
# curve(predict(logreg,data.frame(total=x),type="resp"),add=TRUE)
alpha(exam)
## For item1
total<-rowMeans(exam)
p<-mean(exam$item1)
a<- -0.0044
x <- 1/(1+(exp(-a*(total-p))))
plot(total,x, type="l")

## For item5
total<-rowMeans(exam)
p<-mean(exam$item5)
a<- 0.0014
x <- 1/(1+(exp(-a*(total-p))))
plot(total,x, type="l")

## For item4 (high discrimination in IRT)
total<-rowMeans(exam)
p<-mean(exam$item4)
a<- 0.0091
x <- 1/(1+(exp(-a*(total-p))))
plot(total,x, type="l")

#For item2
total<-rowMeans(exam)
p<-mean(exam$item2)
a<- -0.0064
x <- 1/(1+(exp(-a*(total-p))))
plot(total,x, type="l")