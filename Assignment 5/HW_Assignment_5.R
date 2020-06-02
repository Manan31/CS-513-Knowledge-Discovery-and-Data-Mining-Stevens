# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# HW Assignment 4
# CWID : 104530306

rm(list=ls())

#install.packages("rpart")
#install.packages("RcolorBrewer")
#install.packages("rattle")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

ChooseFile <- file.choose()
Breast_Cancer_Data_NO_MISSING<- read.csv(ChooseFile,header = TRUE, na.strings='?')
data_Factor <- colnames(Breast_Cancer_Data_NO_MISSING)
Breast_Cancer_Data_NO_MISSING[data_Factor] <- lapply(Breast_Cancer_Data_NO_MISSING[data_Factor], factor)

split <- sort(sample(nrow(Breast_Cancer_Data_NO_MISSING), as.integer(.70*nrow(Breast_Cancer_Data_NO_MISSING))))
#split

Training_Data <- Breast_Cancer_Data_NO_MISSING[split,]
Testing_Data <- Breast_Cancer_Data_NO_MISSING[-split,]

Training_Data$Class <- as.factor(Training_Data$Class)

prediction_model<-rpart(Class~.,Training_Data[,-1])
rpart.plot(prediction_model,roundint = FALSE)
prediction_data<-predict(prediction_model,Testing_Data[,-1],type="class") 
table(Testing_Data[,11],prediction_data)

fancyRpartPlot(prediction_model)

