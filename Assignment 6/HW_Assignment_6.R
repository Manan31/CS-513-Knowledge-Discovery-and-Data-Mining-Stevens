  # Name : Manan Bhatt
  # CS 513 B  - Knowledge Discovery And Data Mining
  # HW Assignment 6.1
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
  Breast_Cancer_Data_NO_MISSING<- read.csv(ChooseFile,header = TRUE, na.strings='?')
  data_Factor <- colnames(Breast_Cancer_Data_NO_MISSING)
  Breast_Cancer_Data_NO_MISSING[data_Factor] <- lapply(Breast_Cancer_Data_NO_MISSING[data_Factor], factor)
  
  #Splitting Data
  split <- sort(sample(nrow(Breast_Cancer_Data_NO_MISSING), as.integer(.70*nrow(Breast_Cancer_Data_NO_MISSING))))
  Training_Data <- Breast_Cancer_Data_NO_MISSING[split,]
  Testing_Data <- Breast_Cancer_Data_NO_MISSING[-split,]
  
  
  Training_Data$Class <- as.factor(Training_Data$Class)
  
  model<-C5.0(Class~., data = Training_Data, method = "class")
  prediction_data<-predict(model,Testing_Data,type="class")
  plot(model)
  table(test_data$Class,prediction_data)
  wrong_data <- sum(prediction_data!=Testing_Data$Class)
  wrong_data
  error_rate <- wrong_data/length(prediction_data)
  error_rate