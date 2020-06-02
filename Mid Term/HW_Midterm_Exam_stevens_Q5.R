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
#View(COVID19_No_Missing)

##Normalize the Data using min max function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

COVID19_Normalized<-as.data.frame (         
  cbind(   Exposure=mmnorm(COVID19_No_Missing[,3],min(COVID19_No_Missing[,3]),max(COVID19_No_Missing[,3]))
          ,Cases=mmnorm(COVID19_No_Missing[,5],min(COVID19_No_Missing[,5]),max(COVID19_No_Missing[,5]))
          ,ID=as.character(COVID19_No_Missing[,1])
          ,MartialStatus=as.character(COVID19_No_Missing[,4])
          ,Infected=as.character(COVID19_No_Missing[,7])
          ,MonthAtHospital=as.character(COVID19_No_Missing[,6])
          ,Age=as.character(COVID19_No_Missing[,2])
          
          
  )
)


View(COVID19_Normalized)


# Split the data into training and testing data
split <- sort(sample(nrow(COVID19_Normalized), as.integer(.70*nrow(COVID19_Normalized))))

Covid_Training_Data <- COVID19_Normalized[split,]
Covid_Testing_Data <- COVID19_Normalized[-split,]


library(e1071)
library(class)

#Naive Bayes Model and confusion matrix
?naiveBayes

Covid_Training_Data$Infected <- as.factor(Covid_Training_Data$Infected)
naivebayes_model<- naiveBayes(Infected ~ ., data = Covid_Training_Data)
naivebayes_predict <- predict(naivebayes_model, Covid_Testing_Data, type = "class")
table(naivebayes_model = naivebayes_predict, Covid_Testing_Data$Infected)

#Measuring the accuracy
wrong <- sum(Covid_Testing_Data[,5] != naivebayes_predict)
error_rate <- wrong/length(Covid_Testing_Data[,5])
error_rate

