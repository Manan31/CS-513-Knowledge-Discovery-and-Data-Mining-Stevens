# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# Final Question 2
# CWID : 104530306

rm(list=ls())
#install.packages("randomForest")
library(randomForest)


ChooseFile <- file.choose()
Admission_Cat<- read.csv(ChooseFile,header = TRUE, na.strings=' ?')
View(Admission_Cat)
#Data Preparation
Admission_Cat <- Admission_Cat[,-1]
View(Admission_Cat)
cols <- ncol(Admission_Cat)
cols
Admission_Cat[1:cols] <- lapply(Admission_Cat[1:cols], factor)
View(Admission_Cat)

#Splitting Data
split<-sort(sample(nrow(Admission_Cat),round(.30*nrow(Admission_Cat))))
Training_Data <- Admission_Cat[-split,]
Testing_Data <- Admission_Cat[split,]

#Model
fit <- randomForest( ADMIT~., data=Training_Data, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, Testing_Data)
table(actual=Testing_Data$ADMIT,Prediction)
tab<-table(actual=Testing_Data$ADMIT,Prediction)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

