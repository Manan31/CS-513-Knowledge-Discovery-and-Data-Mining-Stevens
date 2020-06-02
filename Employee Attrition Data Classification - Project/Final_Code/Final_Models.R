#  Course          : Data Mining
#  Group Name      : Team Ducks$Indian
#  Team Member Name: Aakash Rami, Manan Bhatt, Rohan Ratwani, Hardik Patel

# FINAL PROJECT

# CODE

rm(list=ls())

# Loading file into csvfile
data_file <- file.choose()

# Replacing missing values with NA
attrition_data <- read.csv(data_file, na.string = c("","?","NA"," "))

# Data after replacing missing values with NA
View(attrition_data)

# Data Cleaning
attrition_dataset <- attrition_data[c(2,8,9,11,12,17,20,21,22,23,26)]

# Replacing empty values of REFERRAL_SOURCE with mode of it in order to get better accuracy rather than removing the missing values.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


attrition_dataset$REFERRAL_SOURCE<-as.character(attrition_dataset$REFERRAL_SOURCE)
result <- getmode(attrition_dataset$REFERRAL_SOURCE)
attrition_dataset$REFERRAL_SOURCE[which(is.na(attrition_dataset$REFERRAL_SOURCE))] <- result
attrition_dataset$REFERRAL_SOURCE<-as.factor(attrition_dataset$REFERRAL_SOURCE)

# Convert dataset in numeric
attrition_dataset<-data.frame(lapply(attrition_dataset,as.numeric)) 

# Numeric representation of dataset
View(attrition_dataset)

# Normalization of Data
normalized_attrition_data <- as.data.frame(apply(attrition_dataset[,1:ncol(attrition_dataset)],2,function(x) (x - min(x))/(max(x)-min(x))))
View(normalized_attrition_data)

# Converting dataset into factor
data_column <- colnames(normalized_attrition_data)
normalized_attrition_data[data_column]<-lapply(normalized_attrition_data[data_column], factor)


# Spliting normalized data into 75:25 for training and testing respectively.
split<-sort(sample(nrow(normalized_attrition_data),as.integer(.75*nrow(normalized_attrition_data))))

train_data<-normalized_attrition_data[split,]
test_data<-normalized_attrition_data[-split,]

# Used in order to prevent automatic removal of rows while model is trained and tested
options(max.print=999999)

#Model
#KNN

# Loading library
library(kknn)

# For k=3
# Training and Predicting the value
Attrition_Normalized_k3 <- kknn(formula=STATUS~., train_data, test_data[,-8], k=3,kernel ="triangular" )
fit1 <- fitted(Attrition_Normalized_k3)

# Confusion Matrix
table(Actual=test_data$STATUS,Fitted=fit1)
tab <- table(Actual=test_data$STATUS,Fitted=fit1)

# Calculating error rate 
error_rate1 = 100 - accuracy(table(Predict = fit1, Actual = test_data$STATUS))
error_rate1
# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


# For k=5
# Training and Predicting the value
Attrition_Normalized_k5 <- kknn(formula=STATUS~., train_data, test_data[,-8], k=5,kernel ="triangular" )
fit2 <- fitted(Attrition_Normalized_k5)

# Confusion Matrix
table(Actual=test_data$STATUS,Fitted=fit2)
tab2 <- table(Actual=test_data$STATUS,Fitted=fit2)

# Calculating error rate 
error_rate2 = 100 - accuracy(table(Predict = fit2, Actual = test_data$STATUS))
error_rate2

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab2)

# For k=10
# Training and Predicting the value
Attrition_Normalized_k10 <- kknn(formula=STATUS~., train_data, test_data[,-8], k=10,kernel ="triangular" )
fit3 <- fitted(Attrition_Normalized_k10)

# Confusion Matrix
table(Actual=test_data$STATUS,Fitted=fit3)
tab3 <- table(Actual=test_data$STATUS,Fitted=fit3)

# Calculating error rate 
error_rate3 = 100 - accuracy(table(Predict = fit3, Actual = test_data$STATUS))
error_rate3

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab3)

# For k=12
# Training and Predicting the value
Attrition_Normalized_k12 <- kknn(formula=STATUS~., train_data, test_data[,-8], k=12,kernel ="triangular" )
fit4 <- fitted(Attrition_Normalized_k12)

# Confusion Matrix
table(Actual=test_data$STATUS,Fitted=fit4)
tab4 <- table(Actual=test_data$STATUS,Fitted=fit4)

# Calculating error rate 
error_rate4 = 100 - accuracy(table(Predict = fit4, Actual = test_data$STATUS))
error_rate4

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab4)

# For k=15
# Training and Predicting the value
Attrition_Normalized_k15 <- kknn(formula=STATUS~., train_data, test_data[,-8], k=15,kernel ="triangular" )
fit5 <- fitted(Attrition_Normalized_k15)

# Confusion Matrix
table(Actual=test_data$STATUS,Fitted=fit5)
tab5 <- table(Actual=test_data$STATUS,Fitted=fit5)

# Calculating error rate 
error_rate5 = 100 - accuracy(table(Predict = fit5, Actual = test_data$STATUS))
error_rate5

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab5)

# For k=20
# Training and Predicting the value
Attrition_Normalized_k20 <- kknn(formula=STATUS~., train_data, test_data[,-8], k=20,kernel ="triangular" )
fit6 <- fitted(Attrition_Normalized_k20)

# Confusion Matrix
table(Actual=test_data$STATUS,Fitted=fit6)
tab6 <- table(Actual=test_data$STATUS,Fitted=fit6)

# Calculating error rate 
error_rate6 = 100 - accuracy(table(Predict = fit6, Actual = test_data$STATUS))
error_rate6

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab6)

# For k=25
# Training and Predicting the value
Attrition_Normalized_k25 <- kknn(formula=STATUS~., train_data, test_data[,-8], k=25,kernel ="triangular" )
fit7 <- fitted(Attrition_Normalized_k25)

# Confusion Matrix
table(Actual=test_data$STATUS,Fitted=fit7)
tab7 <- table(Actual=test_data$STATUS,Fitted=fit7)

# Calculating error rate 
error_rate7 = 100 - accuracy(table(Predict = fit7, Actual = test_data$STATUS))
error_rate7

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab7)

# Decision Tree

# Loading library
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

# Training and Predicting the value
prediction_model<-rpart(STATUS~.,train_data)
rpart.plot(prediction_model,roundint = FALSE)
prediction_data<-predict(prediction_model,test_data,type="class") 

# Confusion Matrix
table(test_data[,8],prediction_data)
tab8 <- table(test_data[,8],prediction_data)

# Plot 
fancyRpartPlot(prediction_model)

# Calculating error rate 
error_rate4 = 100 - accuracy(table(Predict = prediction_data, Actual = test_data$STATUS))
error_rate4

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab8)

#Naive Bayes

# Loading library
library(e1071)
library(class)

# Training and Predicting the value
naivebayes_model<- naiveBayes(STATUS ~ ., data = train_data)
naivebayes_predict <- predict(naivebayes_model, test_data, type = "class")

# Confusion Matrix
table(naivebayes_model = naivebayes_predict, test_data$STATUS)
tab9 <- table(naivebayes_model = naivebayes_predict, test_data$STATUS)

# Calculating error rate 
error_rate5 = 100 - accuracy(table(Predict = naivebayes_predict, Actual = test_data$STATUS))
error_rate5

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab9)

# C50

# Loading library
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
library(class)
library(C50)

# Training and Predicting the value
model<-C5.0(STATUS~., data = train_data, method = "class")
prediction_data<-predict(model,test_data,type="class")

# Plot
plot(model)

# Confusion Matrix
table(test_data$STATUS,prediction_data)
tab10 <- table(test_data$STATUS,prediction_data)

# Calculating error rate
error_rate6 = 100 - accuracy(table(Predict = prediction_data, Actual = test_data$STATUS))
error_rate6

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab10)


