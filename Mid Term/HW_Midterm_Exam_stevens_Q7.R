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

##Normalize the Data using min max function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

COVID19_Normalized<-as.data.frame (         
  cbind(    Age=mmnorm(COVID19_No_Missing[,2],min(COVID19_No_Missing[,2]),max(COVID19_No_Missing[,2]))
           ,Exposure=mmnorm(COVID19_No_Missing[,3],min(COVID19_No_Missing[,3]),max(COVID19_No_Missing[,3]))
           ,Cases=mmnorm(COVID19_No_Missing[,5],min(COVID19_No_Missing[,5]),max(COVID19_No_Missing[,5]))
           ,MonthAtHospital=mmnorm(COVID19_No_Missing[,6],min(COVID19_No_Missing[,6]),max(COVID19_No_Missing[,6]))
           ,ID=as.character(COVID19_No_Missing[,1])
           ,MartialStatus=as.character(COVID19_No_Missing[,4])
           ,Infected=as.character(COVID19_No_Missing[,7])
          
           
           
  )
)

View(COVID19_Normalized)


# Split the data into training and testing data
split <- sort(sample(nrow(COVID19_Normalized), as.integer(.70*nrow(COVID19_Normalized))))

Covid_Training_Data <- COVID19_Normalized[split,]
Covid_Testing_Data <- COVID19_Normalized[-split,]

library(kknn)

#KNN Model

Covid_Training_Data$Infected <- as.factor(Covid_Training_Data$Infected)
Covid19_k5 <- kknn(formula=Infected~., Covid_Training_Data, Covid_Testing_Data[,-7], k=5,kernel ="triangular" )
fit <- fitted(Covid19_k5)
table(Actual=Covid_Testing_Data$Infected,Fitted=fit)

