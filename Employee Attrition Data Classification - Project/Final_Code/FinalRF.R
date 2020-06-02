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

# Converting decision column into factor
normalized_attrition_data$STATUS=as.factor(normalized_attrition_data$STATUS)

# Spliting normalized data into 75:25 for training and testing respectively.
split<-sort(sample(nrow(normalized_attrition_data),as.integer(.75*nrow(normalized_attrition_data))))

training_on_normalized<-normalized_attrition_data[split,]
test_on_normalized<-normalized_attrition_data[-split,]

# Used in order to prevent automatic removal of rows while model is trained and tested
options(max.print=999999)

# Loading library
library(randomForest)

# Model 

#ntree=500
# Training and Predicting the value
fit <- randomForest( STATUS~., data=training_on_normalized, importance=TRUE, ntree=500)
#importance(fit)
varImpPlot(fit)
default_pred <- predict(fit,test_on_normalized,type="class")

# Confusion Matrix
table(Actual = test_on_normalized$STATUS,Predicted = default_pred)
tab<-table(test_on_normalized$STATUS,default_pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=default_pred)
randomforest_rate<-sum(wrong)/length(wrong)
randomforest_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


#ntree=800
# Training and Predicting the value
fit <- randomForest( STATUS~., data=training_on_normalized, importance=TRUE, ntree=800)
#importance(fit)
varImpPlot(fit)
default_pred <- predict(fit,test_on_normalized,type="class")

# Confusion Matrix
table(Actual = test_on_normalized$STATUS,Predicted = default_pred)
tab<-table(test_on_normalized$STATUS,default_pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=default_pred)
randomforest_rate<-sum(wrong)/length(wrong)
randomforest_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



#ntree=1000
# Training and Predicting the value
fit <- randomForest( STATUS~., data=training_on_normalized, importance=TRUE, ntree=1000)
#importance(fit)
varImpPlot(fit)
default_pred <- predict(fit,test_on_normalized,type="class")

# Confusion Matrix
table(Actual = test_on_normalized$STATUS,Predicted = default_pred)
tab<-table(test_on_normalized$STATUS,default_pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=default_pred)
randomforest_rate<-sum(wrong)/length(wrong)
randomforest_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


#ntree=1200
# Training and Predicting the value
fit <- randomForest( STATUS~., data=training_on_normalized, importance=TRUE, ntree=1200)
#importance(fit)
varImpPlot(fit)
default_pred <- predict(fit,test_on_normalized,type="class")

# Confusion Matrix
table(Actual = test_on_normalized$STATUS,Predicted = default_pred)
tab<-table(test_on_normalized$STATUS,default_pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=default_pred)
randomforest_rate<-sum(wrong)/length(wrong)
randomforest_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


#ntree=1500
# Training and Predicting the value
fit <- randomForest( STATUS~., data=training_on_normalized, importance=TRUE, ntree=1500)
#importance(fit)
varImpPlot(fit)
default_pred <- predict(fit,test_on_normalized,type="class")

# Confusion Matrix
table(Actual = test_on_normalized$STATUS,Predicted = default_pred)
tab<-table(test_on_normalized$STATUS,default_pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=default_pred)
randomforest_rate<-sum(wrong)/length(wrong)
randomforest_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


#ntree=1600
# Training and Predicting the value
fit <- randomForest( STATUS~., data=training_on_normalized, importance=TRUE, ntree=1600)
#importance(fit)
varImpPlot(fit)
default_pred <- predict(fit,test_on_normalized,type="class")

# Confusion Matrix
table(Actual = test_on_normalized$STATUS,Predicted = default_pred)
tab<-table(test_on_normalized$STATUS,default_pred)

# Calculating error rate 
wrong<- (test_on_normalized$STATUS!=default_pred)
randomforest_rate<-sum(wrong)/length(wrong)
randomforest_rate

# Calculating accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
