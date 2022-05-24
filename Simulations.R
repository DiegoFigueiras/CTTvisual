library(tidyverse)
library(descr)
library(psych)
library(mirt)
library(ggplot2)
library(faux)
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

for(j in 1:3){


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
        
        
        mod2<-mirt(data.frame(sim2), 1, itemtype="2PL") #getting IRT model
        IRT_parms2 <- coef(mod2, IRTpars = TRUE, simplify = TRUE) #getting IRT parameters
        irt2 <- IRT_parms2$items
        df2<-as.data.frame(cbind(pseudob2, pvalues2,pseudoA2, irt2)) #putting pseudob from line 41, pvalues from line 42, pseudoA from line 53 and IRT parameters from line 57 together in a DF.
        colnames(df2)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")#renaming the headers of the above DF.
        
        df2<-df2%>%filter(b<3)%>%filter(b>-3)

        
        
        reg<-lm(b ~ pseudob, df2)#calculating regression of pseudob on b
        
        
        
        coeficients<-coeficients%>%rbind(data.frame( #putting the intercept, slope, pvalue and label of the above regression model into a DF.Rbinds with each iteration
          intercept=summary(reg)$coefficients[1,1],
          slope=summary(reg)$coefficients[2,1],
          seint=summary(reg)$coefficients[1,2],
          seslope=summary(reg)$coefficients[2,2],
          scrubbedn=nrow(df2),
          simulation="Simulation 2"
          
        ))

}


df2%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))


hist(sim2means$pvalues)

###################### SIMULATION 3: NORMAL DISTRIBUTION ##############################



for(j in 1:3){


        y<-data.frame(matrix(rnorm(100,.5,.2))) #creates a normal distribution that will be used later for creating the simulated binary data
        y<-data.frame(apply(y, 2, sort, decreasing=F))#creates a normal distribution that will be used later for creating the simulated binary data
        sim3<-matrix(ncol=100, nrow=10000) #declaring the dataframe where the simulated responses will be stored.
        
        for(i in 1:100){
          sim3[,i]<-sample(0:1, 10000, prob=c(abs(y[i,]), abs(y[i,]-1)), replace=TRUE) #using the sample() function to simulate the binary data and put it in sim3. Each iteration uses the values in y as the probabilities.
        }
        
        sim3<-data.frame(apply(sim3, 2, sort, decreasing=F))
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
        
        mod3<-mirt(data.frame(sim3), 1, itemtype="2PL")
        IRT_parms3 <- coef(mod3, IRTpars = TRUE, simplify = TRUE)
        irt3 <- IRT_parms3$items
        df3<-as.data.frame(cbind(pseudob3, pvalues3,pseudoA3, irt3))
        colnames(df3)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")
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
          simulation="Simulation 3"
          
        ))

}

sim3means<-round(sim3means, 2)
df3%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))

hist(sim3means$pvalues)

###################### SIMULATION 4: INVERTED DISTRIBUTION ##############################

for(j in 1:3){


        x<-data.frame(matrix(rbeta(10000, 5, 2), ncol=50, nrow=10000))#creating a matrix of skewed data that will be used later for simulating the binary distribution
        sim4.1<-matrix(ncol=50, nrow=10000)
        for(i in 1:ncol(x)){
          
          sim4.1[,i]<-ifelse(x[,i]<(0.02*i), 0,1)#if each number in x is less than 0.02*index, it outputs a 0, otherwise a 1. 
          if(sum(sim4.1[,i])==0){#lines 168-172 are just to scatter 1s into columns that have all 0s and to scatter 0s to columns that have all 1s.
            sim4.1[,i][random]<-1
          }
          if(sum(sim4.1[,i])==10000){
            sim4.1[,i][random]<-0
          }
        }
        sim4.2<-matrix(ncol=50, nrow=10000)#same process as the previos simulation, but changing the inequality sign in the ifelse so it is skewed in the opposite direction
        for(i in 1:ncol(x)){
          
          sim4.2[,i]<-ifelse(x[,i]>(0.02*i), 0,1)
          if(sum(sim4.2[,i])==0){
            sim4.2[,i][random]<-1
          }
          if(sum(sim4.2[,i])==10000){
            sim4.2[,i][random]<-0
          }
        }
        
        
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
        
        mod4<-mirt(data.frame(sim4), 1, itemtype="2PL")
        IRT_parms4 <- coef(mod4, IRTpars = TRUE, simplify = TRUE)
        irt4 <- IRT_parms4$items
        df4<-as.data.frame(cbind(pseudob4, pvalues4,pseudoA4, irt4))
        colnames(df4)<-c("pseudob", "pvalues","PseudoA", "a", "b", "g", "u")
        
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
          simulation="Simulation 4"
          
        ))


}


df4%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))

hist(sim4means$pvalues)

###################### SIMULATION 5: SKEWED NEGATIVE ##############################


for(j in 1:3){

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
        pseudob5<-data.frame(qnorm(sim5means$pvalues))
        pvalues5<-data.frame(sim5means$pvalues)
        
        ahat<-function(x){
          r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
          
          ((0.51+(0.02*pseudob5)+(0.301*pseudob5^2))*r)+((0.57-(0.009*pseudob5)+(0.19*pseudob5^2))*r)
          
        }
        
        alphas<-psych::alpha(sim5)
        pseudoA5<-data.frame(ahat(alphas$item.stats$r.drop))
        
        mod5<-mirt(data.frame(sim5), 1, itemtype="2PL")
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
          simulation="Simulation 5"
          
        ))
        
      

}



df5%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))


hist(sim5means$pvalues)

###################### SIMULATION 6: SKEWED POSITIVE ##############################

for(j in 1:3){


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
        pseudob6<-data.frame(qnorm(sim6means$pvalues))
        pvalues6<-data.frame(sim6means$pvalues)
        
        ahat<-function(x){
          r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
          
          ((0.51+(0.02*pseudob6)+(0.301*pseudob6^2))*r)+((0.57-(0.009*pseudob6)+(0.19*pseudob6^2))*r)
          
        }
        
        alphas<-psych::alpha(sim6)
        pseudoA6<-data.frame(ahat(alphas$item.stats$r.drop))
        
        mod6<-mirt(data.frame(sim6), 1, itemtype="2PL")
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
          simulation="Simulation 6"
          
        ))
        
        
}  


df6%>%ggplot(aes(x=b, y=pseudob))+
  geom_point()+
  geom_text(aes(label=pseudob))


hist(sim6means$pvalues)




