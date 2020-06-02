# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# HW Assignment 2
# CWID : 104530306

rm(list=ls())


EDA_DATA<- read.csv("F:/Stevens/SEM 2/KDD/breast-cancer-wisconsin.data.csv",header = TRUE, na.strings='?')
View(EDA_DATA)

## Question 1.1: Summary of the Data
summary(EDA_DATA)

## Question 1.2: Identify Missing Values
missing_values <-which(is.na(EDA_DATA$F6))
missing_values

##Question 1.3: Replacing Missing Values with Mean of the column

##mean_F6 <- mean(EDA_DATA$F6, na.rm = TRUE)
##mean_F6

EDA_DATA[is.na(EDA_DATA)] = mean(EDA_DATA$F6, na.rm = TRUE)

##Question 1.4: Generate Frequncey Table of Class vs F6

table(EDA_DATA$Class, EDA_DATA$F6)

##Question 1.5: Scatter plot of F1 to F6
pairs(EDA_DATA[2:7])

##Question 1.6:  Histogram and Box Plot
hist(EDA_DATA$F7)
hist(EDA_DATA$F8)
hist(EDA_DATA$F9)
boxplot(EDA_DATA[8:10], col = c('Blue','Red','Green'))



## Question 2

rm(list=ls())
EDA_DATA<- read.csv("F:/Stevens/SEM 2/KDD/breast-cancer-wisconsin.data.csv",header = TRUE, na.strings='?')
EDA_NO_MISSING <- na.omit(EDA_DATA)
View(EDA_NO_MISSING)
   


