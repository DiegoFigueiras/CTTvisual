#source("ETS/ETS_data2.R", local = knitr::knit_global()) RUN THIS SOURCE SCRIPT FIRST

df_plot2$item<-row.names(df_plot2)

df_item<-df_plot2%>%dplyr::select(a, b, PseudoA, PseudoB, item)
df_IRT<-df_item[1:40,]%>%dplyr::select(a,b)
df_CTT<-df_item[1:40,]%>%dplyr::select(PseudoA,PseudoB)

install.packages("difR")
library(difR)

chi_squares<-LordChi2(df_IRT, df_CTT)%>%as.data.frame()
#write.csv(chi_squares, "chi_squares.csv")
raju <- RajuZ(df_IRT, df_CTT)%>%as.data.frame()  ## where's the AUC frame?
together <- cbind(df_CTT,raju)
cor(together)

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

