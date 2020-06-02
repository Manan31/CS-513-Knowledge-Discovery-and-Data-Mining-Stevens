# Name : Manan Bhatt
# CS 513 B  - Knowledge Discovery And Data Mining
# Final Question 1
# CWID : 104530306

rm(list=ls())
library(cluster)
library(fpc)

ChooseFile <- file.choose()
Admission<- read.csv(ChooseFile,header = TRUE, na.strings='?')


Admission<-Admission[-1]
Admission<-na.omit(Admission)
admission_file_pred<-Admission[1]
View(Admission)


#Heirarchical Clustering
Admission_First_Two<- Admission[c(2,3)]
hclust<-dist(Admission_First_Two)
hclust_results<-hclust(hclust)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
View(hclust_2)
table(hclust_2,t(admission_file_pred))
plotcluster(Admission_First_Two,hclust_2)

#K- means Clustering

admission_file1<-read.csv(ChooseFile,na.strings = ' ?')
admission_file1<-admission_file1[,-1]
admission_file1<-admission_file1[c(2,3)]
kmeans_2<- kmeans(admission_file1,2,nstart = 10)
kmeans_pred <- kmeans_2$cluster
table(kmeans_pred,admission_file1[,1])
plotcluster(admission_file1,kmeans_pred)


