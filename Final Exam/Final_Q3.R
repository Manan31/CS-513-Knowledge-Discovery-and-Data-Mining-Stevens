# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# Final Question 3
# CWID : 104530306

rm(list=ls())
#install.packages("C50")
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
library(class)
library(C50)

#Data Preparation
ChooseFile <- file.choose()
Admission_Cat<- read.csv(ChooseFile,header = TRUE, na.strings='?')
Admission_Cat <- Admission_Cat[,-1]
data_Factor <- colnames(Admission_Cat)
Admission_Cat[data_Factor] <- lapply(Admission_Cat[data_Factor], factor)

#Splitting Data
split<-sort(sample(nrow(Admission_Cat),round(.30*nrow(Admission_Cat))))
Training_Data <- Admission_Cat[-split,]
Testing_Data <- Admission_Cat[split,]


Training_Data$ADMIT <- as.factor(Training_Data$ADMIT)

model<-C5.0(ADMIT~., data = Training_Data, method = "class")
prediction_data<-predict(model,Testing_Data,type="class")
plot(model)
table(Testing_Data$ADMIT,prediction_data)
tab <- table(Testing_Data$ADMIT,prediction_data)
#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
