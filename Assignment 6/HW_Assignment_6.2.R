# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# HW Assignment 6.2
# CWID : 104530306

rm(list=ls())
#install.packages("randomForest")
library(randomForest)


ChooseFile <- file.choose()
Breast_Cancer_Data_NO_MISSING<- read.csv(ChooseFile,header = TRUE, na.strings=' ?')
View(Breast_Cancer_Data_NO_MISSING)
#Data Preparation
Breast_Cancer_Data_NO_MISSING <- Breast_Cancer_Data_NO_MISSING[,-1]
#View(Breast_Cancer_Data_NO_MISSING)
cols <- ncol(Breast_Cancer_Data_NO_MISSING)
cols
Breast_Cancer_Data_NO_MISSING[1:cols] <- lapply(Breast_Cancer_Data_NO_MISSING[1:cols], factor)
View(Breast_Cancer_Data_NO_MISSING)

#Splitting Data
split<-sort(sample(nrow(Breast_Cancer_Data_NO_MISSING),round(.25*nrow(Breast_Cancer_Data_NO_MISSING))))
Training_Data <- Breast_Cancer_Data_NO_MISSING[-split,]
Testing_Data <- Breast_Cancer_Data_NO_MISSING[split,]

#Model
  fit <- randomForest( Class~., data=Training_Data, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, Testing_Data)
table(actual=Testing_Data$Class,Prediction)

#Finding error rate
wrong<- (Testing_Data$Class!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate
