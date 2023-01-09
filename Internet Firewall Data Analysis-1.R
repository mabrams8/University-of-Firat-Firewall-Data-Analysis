# Internet Firewall Data Analysis 
# Mia Abrams 

#Load Libraries 
library(readr) 
library (ggplot2)
install.packages("corrplot")
library(corrplot)

#Upload Firewall data logging file
log2 <- read_csv("log2.csv")
summary(log2)
FirewallData <- log2[-(1),-(5) ]
#cor(FirewallData$`Source Port`,FirewallData$`Destination Port`)
#install.packages("ggplot2")
#library (ggplot2)
#ggplot(data=log2)
corrplot(cor(FirewallData),tl.cex = 0.5)
cor(FirewallData$`Bytes Received`, FirewallData$Bytes)
# The number of bytes received is dependent on the action classification/type
# The NAT Source and Destination Port can predict where bytes are received by 
# by the recipient 
# Based on the corrplot, the bytes received has a moderately strong positive 
# correlation of 0.830225 with the transmitting message
install.packages("dplyr")
library(dplyr)
Actions <- pull(log2,Action)
install.packages("caret")
install.packages('tidyverse')
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(tidyverse)
Train <- createDataPartition(y=FirewallData$Bytes, p=0.8, list=F)
training <- FirewallData[Train,] 
testing <- FirewallData[-Train,]
decisionTreeBinary <- rpart(Action ~ . , data = log2, cp=0.1)
fancyRpartPlot(decisionTreeBinary)

#install.packages("tidyverse") # a set of data science tools including dplyr, tidyr and stringr
install.packages("skimr") # a package to facilitate data summaries
install.packages("Hmisc") # a package for data analysis


# Load Libraries
library(tidyverse)
library(skimr)
library(Hmisc)
library(ggplot2)
skim(Actions)
Denied<- filter(log2, log2$Action=="deny")
Allowed<- filter(log2, log2$Action=="allow")
a<-dim(Denied)
b<-dim(Allowed)
x<-dim(log2)
c <-(a[1]/(x[1]))
#Percentage of traffic denied (# number of attacks on network)
paste(round(100*c, 2), "%")


#Percentage of Not Allowed 
#install.packages("stringr")
#library(stringr)
#filter(Actions,"drop")
#filter(log2,log2$`Bytes Received`== 0)
# percentage of rejected transmission of data would mostly indicate the number 
# of attacks on the network

## Our theory: more bytes sent means longer elapsed time
## Null hypothesis - strong correlation relationship

FirewallDataModel1 <- lm(FirewallData$Bytes ~ FirewallData$`Elapsed Time (sec)`)

summary(FirewallDataModel1)

#Conclusion: Bytes are not a function of Elapsed Time(sec) 


install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("cluster")
install.packages("factoextra")
install.packages("mclust")

library(dplyr)
library(ggplot2)
library(stringr)
library(cluster)
library(factoextra)
library(mclust)
#Cluster plot of the action classes 
fitFirewallData <- Mclust(FirewallData,4)
plot(fitFirewallData, what=c("classification"))
summary(fitFirewallData)
