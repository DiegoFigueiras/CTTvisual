source("ETS/ETS_data2.R", local = knitr::knit_global()) 

df_plot2$item<-row.names(df_plot2)

df_item<-df_plot2%>%dplyr::select(a, b, PseudoA, PseudoB, item)
df_IRT<-df_item[1:40,]%>%dplyr::select(a,b)
df_IRT$group<-"IRT"
df_CTT<-df_item[1:40,]%>%dplyr::select(PseudoA,PseudoB)
df_CTT$group<-"CTT"
colnames(df_CTT)<-c("a","b", "group")
together<-rbind(df_IRT, df_CTT)

install.packages("difR")
library(difR)

chi_squares<-LordChi2(df_IRT, df_CTT)%>%as.data.frame()
#write.csv(chi_squares, "chi_squares.csv")
raju_sign <- RajuZ(df_IRT, df_CTT, signed=TRUE)%>%as.data.frame()  ## where's the AUC frame?
raju_unsign <- RajuZ(df_IRT, df_CTT, signed=FALSE)%>%as.data.frame()
together_signed <- cbind(df_CTT,raju_sign)
together_unsigned <- cbind(df_CTT,raju_unsign)
cor(together)

together_params<-together%>%select(a,b)
# Saving the output into the "RAJUresults.txt" file (and default path)
r <- difRaju(irtParam=together_params, group = together$group, focal.name = "IRT", model = "2PL",
             save.output = TRUE, output = c("RAJUresults","default"))

# Graphical devices
plot(r)

df_diff<-df_plot%>%dplyr::select(diff)
df_diff<-df_diff[1:40,]%>%as.data.frame()



pseudob<-df_item$PseudoB[8]

pseudoa<-df_item$PseudoA[8]
b<-df_item$b[8]
a<-df_item$a[8]
c <- 0
eq1 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob))))))}
curve(eq1, col="red", xlim=c(-4,4))

eq2 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(a*(x-b))))))}
curve(eq2, col="blue", add=T)


pseudob<-df_item$PseudoB[35]

pseudoa<-df_item$PseudoA[35]
b<-df_item$b[35]
a<-df_item$a[35]
c <- 0
eq1 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob))))))}
curve(eq1, col="red", xlim=c(-4,4))

eq2 <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(a*(x-b))))))}
curve(eq2, col="blue", add=T)

