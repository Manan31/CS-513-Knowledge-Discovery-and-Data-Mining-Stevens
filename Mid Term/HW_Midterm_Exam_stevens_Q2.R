# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# Mid Term Examination
# CWID : 104530306

rm(list=ls())

ChooseFile <- file.choose()
COVID19 <- read.csv(ChooseFile,header = TRUE, na.strings='?')
View(COVID19)

## Question 1.1: Summary of the Data
summary(COVID19)


## Question 1.2: Identify Missing Values

missing_values_Age <-which(is.na(COVID19$Age))
missing_values_Age

missing_values_MonthAtHospital <-which(is.na(COVID19$MonthAtHospital))
missing_values_MonthAtHospital

##Question 1.3: Generate Frequncey Table of Infected vs Marital Status

table(COVID19$Infected, COVID19$MaritalStatus)

##Question 1.4: Scatter plot of Age, Marital Status and MonthAtHospital
pairs(COVID19[,-c(1,3,5,7)])

##Question 1.5: Box plot of Age, Marital Status and MonthAtHospital
boxplot(COVID19[,-c(1,3,5,7)], col = c('Blue','Red','Green'))

##Question 1.6: Replace missing values of cases with mean cases
COVID19[is.na(COVID19)] = mean(COVID19$Cases, na.rm = TRUE)


