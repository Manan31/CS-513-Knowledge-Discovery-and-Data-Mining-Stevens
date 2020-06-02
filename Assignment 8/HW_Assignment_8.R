# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# HW Assignment 8
# CWID : 104530306

#Load Libraries
rm(list=ls())
#install.packages("fpc")
#install.packages("cluster")
library(neuralnet)
library(cluster)
library(fpc)

filename <-file.choose()
Breast_Cancer_Data  <-  read.csv(filename, na.string=" ?")# Load the dataset from CSV file
View(Breast_Cancer_Data)# View breast cancer data

#Data Preperation
Breast_Cancer_Data<-Breast_Cancer_Data[-1]
Breast_Cancer_Data<-na.omit(Breast_Cancer_Data)#Remove any row with a missing value in any of the columns.
breastCancer_pred<-Breast_Cancer_Data[1]
View(breastCancer_pred)
Breast_Cancer_Data<-data.frame(lapply(Breast_Cancer_Data[,-1],as.numeric)) #Convert dataset in numeric
View(Breast_Cancer_Data)

hclust<-dist(Breast_Cancer_Data)
hclust_resutls<-hclust(hclust)
plot(hclust_resutls)
hclust_2<-cutree(hclust_resutls,2)
View(hclust_2)
#hclust_2 <- ifelse(hclust_2==1,"B","M")
table(hclust_2,t(breastCancer_pred))
plotcluster(Breast_Cancer_Data,hclust_2)


#Kmeans Clustering
Breast_Cancer_Data<-read.csv(filename,na.strings = ' ?')
Breast_Cancer_Data<-Breast_Cancer_Data[,-1]
kmeans_2<- kmeans(Breast_Cancer_Data[,-1],2,nstart = 10)
kmeans_pred <- kmeans_2$cluster
table(kmeans_pred,Breast_Cancer_Data[,1])
plotcluster(Breast_Cancer_Data[-1],kmeans_pred)


