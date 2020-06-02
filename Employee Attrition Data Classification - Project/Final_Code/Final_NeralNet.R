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

# Spliting normalized data into 75:25 for training and testing respectively.
split<-sort(sample(nrow(normalized_attrition_data),as.integer(.75*nrow(normalized_attrition_data))))

training_on_normalized<-normalized_attrition_data[split,]
test_on_normalized<-normalized_attrition_data[-split,]

# Used in order to prevent automatic removal of rows while model is trained and tested
options(max.print=999999)

# Loading library
library(neuralnet)

# Model

# For hidden=6, threshold=0.1,rep=2, learningrate=0.01

# Training and Predicting the value
model<- neuralnet( STATUS~. ,training_on_normalized, act.fct = "logistic",hidden=6, linear.output=F, threshold=0.1, stepmax = 100000,rep=2, learningrate = 0.01)
plot(model)
np <-predict(model, test_on_normalized)
pred<- ifelse(np<0.5,0,1)

# Confusion Matrix
table(Actual=test_on_normalized$STATUS,Prediction=pred)
tab<-table(Actual=test_on_normalized$STATUS,Prediction=pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=pred)
error_rate<-sum(wrong)/length(wrong)
error_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


# For hidden=6, threshold=0.1,rep=2, learningrate=0.2

# Training and Predicting the value
model<- neuralnet( STATUS~. ,training_on_normalized, act.fct = "logistic",hidden=6, linear.output=F, threshold=0.1, stepmax = 100000,rep=2, learningrate = 0.2)
plot(model)
np <-predict(model, test_on_normalized)
pred<- ifelse(np<0.5,0,1)

# Confusion Matrix
table(Actual=test_on_normalized$STATUS,Prediction=pred)
tab<-table(Actual=test_on_normalized$STATUS,Prediction=pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=pred)
error_rate<-sum(wrong)/length(wrong)
error_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# For hidden=c(6,4), threshold=0.1,rep=1, learningrate=0.1

# Training and Predicting the value
model<- neuralnet( STATUS~. ,training_on_normalized, act.fct = "logistic",hidden=c(6,4), linear.output=F, threshold=0.1, stepmax = 100000,rep=1, learningrate = 0.6)
plot(model)
np <-predict(model, test_on_normalized)
pred<- ifelse(np<0.5,0,1)

# Confusion Matrix
table(Actual=test_on_normalized$STATUS,Prediction=pred)
tab<-table(Actual=test_on_normalized$STATUS,Prediction=pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=pred)
error_rate<-sum(wrong)/length(wrong)
error_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# For hidden=C(6,4), threshold=0.1,rep=1, learningrate=0.2

# Training and Predicting the value
model<- neuralnet( STATUS~. ,training_on_normalized, act.fct = "logistic",hidden=6, linear.output=F, threshold=0.01, stepmax = 100000,rep=1, learningrate = 0.2)
plot(model)
np <-predict(model, test_on_normalized)
pred<- ifelse(np<0.5,0,1)

# Confusion Matrix
table(Actual=test_on_normalized$STATUS,Prediction=pred)
tab<-table(Actual=test_on_normalized$STATUS,Prediction=pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=pred)
error_rate<-sum(wrong)/length(wrong)
error_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)