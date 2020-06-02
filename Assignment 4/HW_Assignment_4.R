# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# HW Assignment 4
# CWID : 104530306

rm(list=ls())

#install.packages("e1071")
#install.packages("class")



ChooseFile <- file.choose()
Breat_Cancer_Data<- read.csv(ChooseFile,header = TRUE, na.strings='?')

## omit Missing Values
Breast_Cancer_Data_NO_MISSING <- na.omit(Breat_Cancer_Data)
View(Breast_Cancer_Data_NO_MISSING)

##Normalize the Data
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

Breast_cancer_data_normalized<-as.data.frame (         
  cbind(  F1=mmnorm(Breast_Cancer_Data_NO_MISSING[,2],min(Breast_Cancer_Data_NO_MISSING[,2]),max(Breast_Cancer_Data_NO_MISSING[,2]))
          ,F2=mmnorm(Breast_Cancer_Data_NO_MISSING[,3],min(Breast_Cancer_Data_NO_MISSING[,3]),max(Breast_Cancer_Data_NO_MISSING[,3]))
          ,F3=mmnorm(Breast_Cancer_Data_NO_MISSING[,4],min(Breast_Cancer_Data_NO_MISSING[,4]),max(Breast_Cancer_Data_NO_MISSING[,4]))
          ,F4=mmnorm(Breast_Cancer_Data_NO_MISSING[,5],min(Breast_Cancer_Data_NO_MISSING[,5]),max(Breast_Cancer_Data_NO_MISSING[,5]))
          ,F5=mmnorm(Breast_Cancer_Data_NO_MISSING[,6],min(Breast_Cancer_Data_NO_MISSING[,6]),max(Breast_Cancer_Data_NO_MISSING[,6]))
          ,F6=mmnorm(Breast_Cancer_Data_NO_MISSING[,7],min(Breast_Cancer_Data_NO_MISSING[,7]),max(Breast_Cancer_Data_NO_MISSING[,7]))
          ,F7=mmnorm(Breast_Cancer_Data_NO_MISSING[,8],min(Breast_Cancer_Data_NO_MISSING[,8]),max(Breast_Cancer_Data_NO_MISSING[,8]))
          ,F8=mmnorm(Breast_Cancer_Data_NO_MISSING[,9],min(Breast_Cancer_Data_NO_MISSING[,9]),max(Breast_Cancer_Data_NO_MISSING[,9]))
          ,F9=mmnorm(Breast_Cancer_Data_NO_MISSING[,10],min(Breast_Cancer_Data_NO_MISSING[,10]),max(Breast_Cancer_Data_NO_MISSING[,10]))
          ,Class=as.character(Breast_Cancer_Data_NO_MISSING[,11])
          
  )
)

Breast_cancer_data_normalized
?sort()
?sample()

data_Factor <- colnames(Breast_cancer_data_normalized)
Breast_cancer_data_normalized[data_Factor] <- lapply(Breast_cancer_data_normalized[data_Factor], factor)

split <- sort(sample(nrow(Breast_cancer_data_normalized), as.integer(.70*nrow(Breast_cancer_data_normalized))))
#split

Training_Data <- Breast_cancer_data_normalized[split,]
Testing_Data <- Breast_cancer_data_normalized[-split,]

library(e1071)
library(class)

#Naive Bayes Model
?naiveBayes

Training_Data$Class <- as.factor(Training_Data$Class)
naivebayes_model<- naiveBayes(Class ~ ., data = Training_Data)
naivebayes_predict <- predict(naivebayes_model, Testing_Data, type = "class")
table(naivebayes_model = naivebayes_predict, Testing_Data$Class)



