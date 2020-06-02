# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# Mid Term Examination
# CWID : 104530306

rm(list=ls())

## Choose the csv file to load the data
ChooseFile <- file.choose()
COVID19 <- read.csv(ChooseFile,header = TRUE, na.strings=' ')

## Omit the missing values from the data
COVID19_No_Missing <- na.omit(COVID19)

## Discretize the Age and MonthAtHospital Columns by using the cut Function to convert continous variable to categorical variable
max_Month <- max(COVID19_No_Missing$MonthAtHospital)
max_Age <- max(COVID19_No_Missing$Age)
max_v
COVID19_No_Missing$MonthAtHospital <- cut(COVID19_No_Missing$MonthAtHospital, breaks = c(-1,6,max_Month), labels = c("Less than 6 Months","6 or More Months"))
COVID19_No_Missing$Age <- cut(COVID19_No_Missing$Age, breaks = c(-1,35,50,max_Age), labels = c("Less than 35","35 to 50","51 or over"))
View(COVID19_No_Missing)

# Split the data into training and testing data
split <- sort(sample(nrow(COVID19_No_Missing), as.integer(.70*nrow(COVID19_No_Missing))))

Covid_Training_Data <- COVID19_No_Missing[split,]
Covid_Testing_Data <- COVID19_No_Missing[-split,]


library(e1071)
library(class)
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

#CART model

Covid_Training_Data$Infected <- as.factor(Covid_Training_Data$Infected)

prediction_model<-rpart(Infected~.,Covid_Training_Data[,-1])
rpart.plot(prediction_model,roundint = FALSE)
prediction_data<-predict(prediction_model,Covid_Testing_Data[,-1],type="class") 
table(Covid_Testing_Data[,7],prediction_data)

fancyRpartPlot(prediction_model)

#Measuring the accuracy
wrong <- sum(Covid_Testing_Data[,7] != prediction_data)
error_rate <- wrong/length(Covid_Testing_Data[,7])
error_rate

