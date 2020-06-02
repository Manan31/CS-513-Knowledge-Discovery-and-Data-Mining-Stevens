# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# HW Assignment 7
# CWID : 104530306

rm(list=ls())
#install.packages("neuralnet")
library(neuralnet)

#Load the Data
ChooseFile <- file.choose()
Breast_Cancer_Data_NO_MISSING<- read.csv(ChooseFile,header = TRUE, na.strings='?')
Breast_Cancer_Data_NO_MISSING<-data.frame(lapply(Breast_Cancer_Data_NO_MISSING[,-1],as.numeric)) #Convert dataset in numeric
View(Breast_Cancer_Data_ON_MISSING)

#Normalize the Data
Breast_cancer_data_normalized <- as.data.frame(apply(Breast_Cancer_Data_NO_MISSING[,1:ncol(Breast_Cancer_Data_NO_MISSING)],2,function(x) (x - min(x))/(max(x)-min(x))))#Normalization
View(Breast_cancer_data_normalized)

split <- sort(sample(nrow(Breast_cancer_data_normalized), as.integer(.70*nrow(Breast_cancer_data_normalized))))
#split

Training_Data <- Breast_cancer_data_normalized[split,]
Testing_Data <- Breast_cancer_data_normalized[-split,]


#Fitting in the model
model<- neuralnet( diagnosis~. ,Training_Data, hidden=5, exclude = NULL,threshold=0.01)
plot(model)

#Predicting the categories
netpred <-predict(model, Testing_Data)
print(netpred)

pred_category <- ifelse(netpred<0.5,0,1)

table(Actual = Testing_Data$diagnosis, Prediction = pred_category)

wrong<- (Testing_Data$diagnosis!=pred_category)
error_rate<-sum(wrong)/length(wrong)
error_rate

