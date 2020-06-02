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
attrition_dataset <- attrition_data[-c(1,4,14)]

# Replacing empty values of REFERRAL_SOURCE with mode of it in order to get better accuracy rather than removing the missing values.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


attrition_dataset$REFERRAL_SOURCE<-as.character(attrition_dataset$REFERRAL_SOURCE)
result <- getmode(attrition_dataset$REFERRAL_SOURCE)
attrition_dataset$REFERRAL_SOURCE[which(is.na(attrition_dataset$REFERRAL_SOURCE))] <- result
attrition_dataset$REFERRAL_SOURCE<-as.factor(attrition_dataset$REFERRAL_SOURCE)

attrition_dataset$ETHNICITY<-as.character(attrition_dataset$ETHNICITY)
result1 <- getmode(attrition_dataset$ETHNICITY)
attrition_dataset$ETHNICITY[which(is.na(attrition_dataset$ETHNICITY))] <- result1
attrition_dataset$ETHNICITY<-as.factor(attrition_dataset$ETHNICITY)

# Convert dataset in numeric
attrition_dataset<-data.frame(lapply(attrition_dataset,as.numeric)) 

# Numeric representation of dataset
View(attrition_dataset)

# Normalization of Data
normalized_attrition_data <- as.data.frame(apply(attrition_dataset[,1:ncol(attrition_dataset)],2,function(x) (x - min(x))/(max(x)-min(x))))
View(normalized_attrition_data)

# Used in order to prevent automatic removal of rows while model is trained and tested
options(max.print=999999)

# load the library
library(mlbench)
library(caret)
library(corrplot)

# Model
corrmat <- cor(normalized_attrition_data$STATUS,normalized_attrition_data)
pairs(corrmat)

# plot
corrplot(cor(normalized_attrition_data), other="hclust")

# calculate correlation matrix
correlationMatrix <- cor(normalized_attrition_data$STATUS,normalized_attrition_data)
correlationMatrix

# corr
cor(normalized_attrition_data)
