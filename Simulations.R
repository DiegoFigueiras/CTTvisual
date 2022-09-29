library(tidyverse)
library(descr)
library(psych)
library(mirt)
library(ggplot2)
library(faux)
library(fGarch)
library(catIrt)
## to visualize p-value distributions

###################### SIMULATION 1: ALL 0.5 ##############################


# x<-data.frame(matrix(runif(10000, 0, 1), ncol=100, nrow=10000))
# sim1<-data.frame(ifelse(x<0.5, 0, 1))
# sim1means<-as.data.frame(colMeans(sim1))
# colnames(sim1means)[1] <- "pvalues"
# 
# sim1means$cond<-"One"


###################### SIMULATION 2: UNIFORM DISTRIBUTION ##############################

coeficients<-NULL#declare the dataframe were coefficients for all 10,000 simulations will be stored
all_sims2<-NULL
for(j in 1:1000){


        x<-data.frame(matrix(runif(10000, 0, 1), ncol=100, nrow=10000)) #creating a matrix of random numbers
        sim2<-matrix(ncol=100, nrow=10000) #declaring the dataframe where the simulated responses will be stored.
        random<-sample(10000, 500) #sampling 500 random numbers from a pool of 10,000
        for(i in 1:100){ #this loop simulates the data from the uniform distribution
          sim2[,i]<-ifelse(x[,i]<0.01*i,0,1) #if each value in x is less than 0.01*i, it stores a 0 on sim2. Otherwise, it stores a 1. 
          if(sum(sim2[,i])==0){ #this conditional statement was just added to account for the possibility there will be a column with all 0s or all 1s.
            sim2[,i][random]<-1 #if the column has all 0s, it chages a random row into a 1 (this was the point of creating the random object, to select a random row)
        
            
          }
          if(sum(sim2[,i])==10000){#same as before, but in this case, if all numbers of a column are 1, it changes a random row into 0. 
            sim2[,i][random]<-0
            
          }

        }
#         f<-1
#         while(f<ncol(sim2)){
#             if(mean(sim2[,f])<0.1){
#               sim2<-sim2[,-f]
#               
#             }
#             if(mean(sim2[,f])>0.9){
#               sim2<-sim2[,-f]
# 
#               
#             }
#           f<-f+1
# }
        
        
        sim2means<-data.frame(colMeans(sim2)) #calculating the mean of each column simulated in the above loop
        colnames(sim2means)[1]<-"pvalues" #renaming the header "pvalues"
        pseudob2<-data.frame((qnorm(sim2means$pvalues))) #getting Zg
        pvalues2<-data.frame(sim2means$pvalues) #getting simple pvalues (not really needed)
        
        ahat<-function(x){
          r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
          
          ((0.51+(0.02*pseudob2)+(0.301*pseudob2^2))*r)+((0.57-(0.009*pseudob2)+(0.19*pseudob2^2))*r)
          
        }

        alphas<-psych::alpha(sim2)
        pseudoA2<-data.frame(ahat(alphas$item.stats$r.drop)) #getting pseudoA
        
        
        mod2<-mirt(data.frame(sim2), 1, itemtype="2PL", technical = list(NCYCLES = 1000)) #getting IRT model
        IRT_parms2 <- coef(mod2, IRTpars = TRUE, simplify = TRUE) #getting IRT parameters
        irt2 <- IRT_parms2$items
        df2<-as.data.frame(cbind(pseudob2, pvalues2,pseudoA2, irt2)) #putting pseudob from line 41, pvalues from line 42, pseudoA from line 53 and IRT parameters from line 57 together in a DF.
        colnames(df2)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")#renaming the headers of the above DF.
        df2$skew<-as.numeric(psych::describe(df2$pvalues)[11])
        df2$kurtosis<-as.numeric(psych::describe(df2$pvalues)[12])
        all_sims2<-all_sims2%>%rbind(df2)#putting all 1,000 simulations at the item level into one dataframe
        
        df2<-df2%>%filter(b<3)%>%filter(b>-3)

        
        
        reg<-lm(b ~ pseudob, df2)#calculating regression of pseudob on b
        
        
        
        coeficients<-coeficients%>%rbind(data.frame( #putting the intercept, slope, pvalue and label of the above regression model into a DF.Rbinds with each iteration
          intercept=summary(reg)$coefficients[1,1],
          slope=summary(reg)$coefficients[2,1],
          seint=summary(reg)$coefficients[1,2],
          seslope=summary(reg)$coefficients[2,2],
          scrubbedn=nrow(df2),
          simulation="Simulation 2",
          mean_p=psych::describe(df2$pvalues))
          
        )

}


df2%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))


hist(sim2means$pvalues)

hist(df2$pvalues)
sd(df2$pvalues)
all_sims2$Simulations<-rep(1:1000, each=100)

write.csv(all_sims2, "Simulation 2/all_sims2.csv")

###################### SIMULATION 3: NORMAL DISTRIBUTION ##############################

#random<-sample(10000, 20)
all_sims<-NULL
for(j in 1:1000){
          
        skew<-rsnorm(100,0,1,xi=1) # xi=1 is the normal distribution specification
        b.params <- cbind(a = rnorm(100, 1.5, .5), b = skew, c = 0)
        theta<-rnorm(10000, 0, 1)
        b.mod <- simIrt(theta= theta, params = b.params, mod = "brm")
        sim3<-data.frame(b.mod$resp)
        g<-psych::describe(sim3)
        hist(g$mean)
        
        sim3means<-data.frame(colMeans(sim3))
        colnames(sim3means)[1]<-"pvalues"
        pseudob3<-data.frame(qnorm(sim3means$pvalues))
        pvalues3<-data.frame(sim3means$pvalues)
        
        ahat<-function(x){
          r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
          
          ((0.51+(0.02*pseudob3)+(0.301*pseudob3^2))*r)+((0.57-(0.009*pseudob3)+(0.19*pseudob3^2))*r)
          
        }
        
        alphas<-psych::alpha(sim3)
        pseudoA3<-data.frame(ahat(alphas$item.stats$r.drop))
        
        
        df3<-as.data.frame(cbind(pseudob3, pvalues3,pseudoA3)) #putting pseudob from line 41, pvalues from line 42, pseudoA from line 53 and IRT parameters from line 57 together in a DF.

        colnames(df3)<-c("pseudob", "pvalues","PseudoA")#renaming the headers of the above DF.
        df3<-df3%>%filter(pvalues<.9)%>%filter(pvalues>.1)
        
        mod3<-mirt(data.frame(sim3), 1, itemtype="2PL", technical = list(NCYCLES = 1000))
        IRT_parms3 <- coef(mod3, IRTpars = TRUE, simplify = TRUE)
        irt3 <- IRT_parms3$items
        df3<-as.data.frame(cbind(pseudob3, pvalues3,pseudoA3, irt3))
        colnames(df3)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")

        
        df3$skew<-as.numeric(psych::describe(df3$pvalues)[11])
        df3$kurtosis<-as.numeric(psych::describe(df3$pvalues)[12])
        all_sims<-all_sims%>%rbind(df3)#putting all 1,000 simulations at the item level into one dataframe
        df3<-df3%>%filter(b<3)%>%filter(b>-3)
        reg<-lm(b ~ pseudob, df3)
        summary(reg)
        coef(reg)
        coeficients<-coeficients%>%rbind(data.frame(
          intercept=summary(reg)$coefficients[1,1],
          slope=summary(reg)$coefficients[2,1],
          seint=summary(reg)$coefficients[1,2],
          seslope=summary(reg)$coefficients[2,2],
          scrubbedn=nrow(df3),
          simulation="Simulation 3",
          mean_p=psych::describe(df3$pvalues)
          
        ))

}

model<-lm(b~pseudob+skew+kurtosis, all_sims)
summary(model)

sim3means<-round(sim3means, 2)
df3%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))

hist(sim3means$pvalues)
write.csv(coeficients, "Simulation 3/new_coeficientsSim3.csv")
write.csv(all_sims, "Simulation 3/all_sims3_NEW.csv")

all_sims3<-read.csv("Simulation 3/all_sims3.csv")
all_sims3$Simulations<-rep(1:1000, each=100)


new_coefficients<-read.csv("Simulation 3/new_coeficientsSim3.csv")
hist(new_coefficients$slope)

###################### SIMULATION 4: INVERTED DISTRIBUTION ##############################
all_sims4<-NULL
for(j in 1:1000){

      
  skew<-rsnorm(50,0,1,xi=-50)
  b.params <- cbind(a = rnorm(50, 1.5, .5), b = skew, c = 0)
  theta<-rnorm(10000, 0, 1)
  b.mod <- simIrt(theta= theta, params = b.params, mod = "brm")
  sim5.1<-data.frame(b.mod$resp)
  
  skew<-rsnorm(50,0,1,xi=50)
  b.params <- cbind(a = rnorm(50, 1.5, .5), b = skew, c = 0)
  theta<-rnorm(10000, 0, 1)
  b.mod <- simIrt(theta= theta, params = b.params, mod = "brm")
  sim5.2<-data.frame(b.mod$resp)
  sim5<-cbind(sim5.1, sim5.2)
  g<-psych::describe(sim5)
  hist(g$mean)
  
        
        
        sim4<-cbind(sim4.1, sim4.2)#merging the two skewed distributions so we get that U-shaped distribution. 
        sim4<-data.frame(apply(sim4, 2, sort, decreasing=F))
        sim4means<-as.data.frame(colMeans(sim4))
        colnames(sim4means)[1] <- "pvalues"
        pseudob4<-data.frame(qnorm(sim4means$pvalues))
        pvalues4<-data.frame(sim4means$pvalues)
        
        ahat<-function(x){
          r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
          
          ((0.51+(0.02*pseudob4)+(0.301*pseudob4^2))*r)+((0.57-(0.009*pseudob4)+(0.19*pseudob4^2))*r)
          
        }
        
        alphas<-psych::alpha(sim4)
        pseudoA4<-data.frame(ahat(alphas$item.stats$r.drop))
        
        mod4<-mirt(data.frame(sim4), 1, itemtype="2PL", technical = list(NCYCLES = 1000))
        IRT_parms4 <- coef(mod4, IRTpars = TRUE, simplify = TRUE)
        irt4 <- IRT_parms4$items
        df4<-as.data.frame(cbind(pseudob4, pvalues4,pseudoA4, irt4))
        colnames(df4)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")
        df4$skew<-as.numeric(psych::describe(df4$pvalues)[11])
        df4$kurtosis<-as.numeric(psych::describe(df4$pvalues)[12])
        all_sims4<-all_sims4%>%rbind(df4)#putting all 1,000 simulations at the item level into one dataframe
        
        df4<-df4%>%filter(b<3)%>%filter(b>-3)
        
        reg<-lm(b ~ pseudob, df4)
        summary(reg)
        coef(reg)
        coeficients<-coeficients%>%rbind(data.frame(
          intercept=summary(reg)$coefficients[1,1],
          slope=summary(reg)$coefficients[2,1],
          seint=summary(reg)$coefficients[1,2],
          seslope=summary(reg)$coefficients[2,2],
          scrubbedn=nrow(df4),
          simulation="Simulation 4",
          mean_p=psych::describe(df4$pvalues)
          
        ))


}


df4%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))

hist(sim4means$pvalues)
write.csv(all_sims4, "Simulation 4/all_sims4.csv")

###################### SIMULATION 5: SKEWED NEGATIVE ##############################


library(fGarch)

all_sims<-NULL
for(j in 1:1000){
        skew<-rsnorm(100,0,1,xi=-3)
        b.params <- cbind(a = rnorm(100, 1.5, .5), b = skew, c = 0)
        theta<-rnorm(10000, 0, 1)
        b.mod <- simIrt(theta= theta, params = b.params, mod = "brm")
        sim5<-data.frame(b.mod$resp)
        g<-psych::describe(sim5)
        hist(g$mean)
  
        sim5means<-as.data.frame(colMeans(sim5))
        colnames(sim5means)[1] <- "pvalues"
        pseudob5<-data.frame(qnorm(sim5means$pvalues))
        pvalues5<-data.frame(sim5means$pvalues)
        
        ahat<-function(x){
          r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
          
          ((0.51+(0.02*pseudob5)+(0.301*pseudob5^2))*r)+((0.57-(0.009*pseudob5)+(0.19*pseudob5^2))*r)
          
        }
        
        alphas<-psych::alpha(sim5)
        pseudoA5<-data.frame(ahat(alphas$item.stats$r.drop))
        
        mod5<-mirt(data.frame(sim5), 1, itemtype="2PL", technical = list(NCYCLES = 1000))
        IRT_parms5 <- coef(mod5, IRTpars = TRUE, simplify = TRUE)
        irt5 <- IRT_parms5$items
        df5<-as.data.frame(cbind(pseudob5, pvalues5,pseudoA5, irt5))
        colnames(df5)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")
        
        df5<-df5%>%filter(b<3)%>%filter(b>-3)
        reg<-lm(b ~ pseudob, df5)
        summary(reg)
        coef(reg)
        coeficients<-coeficients%>%rbind(data.frame(
          intercept=summary(reg)$coefficients[1,1],
          slope=summary(reg)$coefficients[2,1],
          seint=summary(reg)$coefficients[1,2],
          seslope=summary(reg)$coefficients[2,2],
          scrubbedn=nrow(df5),
          simulation="Simulation 5",
          mean_p=psych::describe(df5$pvalues)
          
        ))
        
      

}



df5%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))


hist(sim5means$pvalues)

###################### SIMULATION 6: SKEWED POSITIVE ##############################
all_sims<-NULL
for(j in 1:1000){


        skew<-rsnorm(100,0,1,xi=4)
        b.params <- cbind(a = rnorm(100, 1.5, .5), b = skew, c = 0)
        theta<-rnorm(10000, 0, 1)
        b.mod <- simIrt(theta= theta, params = b.params, mod = "brm")
        sim6<-data.frame(b.mod$resp)
        g<-psych::describe(sim6)
        hist(g$mean)
        sim6means<-as.data.frame(colMeans(sim6))
        colnames(sim6means)[1] <- "pvalues"
        pseudob6<-data.frame(qnorm(sim6means$pvalues))
        pvalues6<-data.frame(sim6means$pvalues)
        
        ahat<-function(x){
          r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
          
          ((0.51+(0.02*pseudob6)+(0.301*pseudob6^2))*r)+((0.57-(0.009*pseudob6)+(0.19*pseudob6^2))*r)
          
        }
        
        alphas<-psych::alpha(sim6)
        pseudoA6<-data.frame(ahat(alphas$item.stats$r.drop))
        
        mod6<-mirt(data.frame(sim6), 1, itemtype="2PL", technical = list(NCYCLES = 1000))
        IRT_parms6 <- coef(mod6, IRTpars = TRUE, simplify = TRUE)
        irt6 <- IRT_parms6$items
        df6<-as.data.frame(cbind(pseudob6, pvalues6,pseudoA6, irt6))
        colnames(df6)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")
        
        df6<-df6%>%filter(b<3)%>%filter(b>-3)
        
        reg<-lm(b ~ pseudob, df6)
        summary(reg)
        coef(reg)
        coeficients<-coeficients%>%rbind(data.frame(
          intercept=summary(reg)$coefficients[1,1],
          slope=summary(reg)$coefficients[2,1],
          seint=summary(reg)$coefficients[1,2],
          seslope=summary(reg)$coefficients[2,2],
          scrubbedn=nrow(df6),
          simulation="Simulation 6",
          mean_p=psych::describe(df6$pvalues))
          
        )
        
        
}  


df6%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))


hist(sim6means$pvalues)


#write.csv(coeficients, "pvalue_to_b_estimates.csv")
new_coefficients<-read.csv("Simulation 3/new_coeficientsSim3.csv")
write.csv(coeficients, "pvalue_to_b_estimates2.csv")
coeficients<-read.csv("pvalue_to_b_estimates_original.csv")
coeficients<-coeficients%>%filter(simulation!="Simulation 3")%>%filter(simulation!="Simulation 4")
simulation3<-new_coefficients%>%select(1:7)
coefficients2<-rbind(coeficients, simulation3)
write.csv(coefficients2, "SIOP/temp_siop.csv")
library(ggplot2)
ggplot(data = coefficients2, aes(x = intercept)) + geom_histogram(bins = 500) + facet_grid(simulation~.)
ggplot(data = coefficients2, aes(x = slope)) + geom_histogram(bins = 500) + facet_grid(simulation~.)
ggplot(data = coefficients2, aes(x = mean_p.kurtosis)) + geom_histogram(bins = 500) + facet_grid(simulation~.)
ggplot(data = coefficients2, aes(x = mean_p.skew)) + geom_histogram(bins = 500) + facet_grid(simulation~.)


simulation4<-coeficients%>%filter(simulation=="Simulation 4")
hist(simulation4$slope, breaks=100)
axis(side=1, at=seq(-3.5,-1.5, .01), labels=seq(-3.5,-1.5,.01))
bump1<-simulation4%>%filter(slope< -1.89)
bump2<-simulation4%>%filter(slope> -1.89 & slope< -1.76)
bump3<-simulation4%>%filter(slope> -1.76 & slope< -1.68)
bump4<-simulation4%>%filter(slope> -1.68 & slope< -1.54)
bump5<-simulation4%>%filter(slope> -1.54)

bump1$bump<- "bump1"
bump2$bump<- "bump2"
bump3$bump<- "bump3"
bump4$bump<- "bump4"
bump5$bump<- "bump5"

temp<-rbind(bump1, bump2, bump3, bump4, bump5)
ggplot(data = temp, aes(x = intercept)) + geom_histogram() + facet_grid(bump~.) #looks consistent

ggplot(data = temp, aes(x = seint)) + geom_histogram() + facet_grid(bump~.)

ggplot(data = temp, aes(x = seslope)) + geom_histogram() + facet_grid(bump~.)

ggplot(data = temp, aes(x = scrubbedn)) + geom_histogram() + facet_grid(bump~.)

#Look at kurtosis and skeweness per bump of simulation4. Excluded cases might be what is driving the difference. Run simulations without excluding cases.
# Add slope graph from 5000 to the paper
# Tweak regression model based on average kurtosis of pvalues. Tweak also pseudob using kurtosis. 
#Check area between curves of each simulation
#

write.csv(all_sims, "all_sims3.csv")






plot(all_sims$b[1:100], all_sims$pseudob[1:100])
par(new=TRUE)
plot(all_sims$b[101:200], all_sims$pseudob[101:200], col="red")
par(new=TRUE)
plot(all_sims$b[201:300], all_sims$pseudob[201:300], col="blue")
par(new=TRUE)
plot(all_sims$b[301:400], all_sims$pseudob[301:400], col="green")
par(new=TRUE)
plot(all_sims$b[401:500], all_sims$pseudob[401:500], col="yellow")
model<-lm(b~pseudob+skew+kurtosis, all_sims)
summary(model)

colors<-rep(c("Red", "blue","yellow","orange","purple","brown","green","pink","black", "white"), 1000)
for (i in seq(from=1, to=100000, by=100)){
  plot(all_sims2$b[i:(i+100)], all_sims2$pseudob[i:(i+100)], col=sample(colors,1))
  par(new=TRUE)
}

for (i in seq(from=1, to=100000, by=100)){
  hist(all_sims$b[i:(i+100)], col=sample(colors,1))
  par(new=TRUE)
}

for (i in seq(from=1, to=100000, by=100)){
  hist(all_sims$pseudob[i:(i+100)], col=sample(colors,1))
  par(new=TRUE)
}

all_sims2<-read.csv('Simulation 2/all_sims2.csv')
all_sims3<-read.csv('Simulation 3/all_sims3.csv')
all_sims4<-read.csv('Simulation 4/all_sims4.csv')
all_sims5<-read.csv('Simulation 5/all_sims5.csv')
