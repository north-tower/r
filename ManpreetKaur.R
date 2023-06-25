#Name : Manpreet Kaur


#Libraries
options(warn=-1) 
library(naivebayes)
library(tidyverse)
library(psych)
library(e1071)
library(caret)
library(class)
library(dplyr)
library(factoextra)
library(stats)
library(ROCR)



#importing the dataset
df <- read.csv("DatasetforFINAL.csv")

df <- subset(df , City == "Toronto")

df <- subset(df , select = c(FamilyIncome,FamilySize,boughtelectronics))


#Data Wrangling
df$boughtelectronics <- ifelse(df$boughtelectronics == "YES", 1, 0)
df$boughtelectronics <- as.factor(df$boughtelectronics)


#Question 1 
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

#Question 2a
x <- subset(df, select=c(FamilySize,FamilyIncome))
y <- df$boughtelectronics

model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10))


preds_naive <- predict(model, test)

#Question 3a
##run knn function
model_knn <- train(boughtelectronics ~ FamilyIncome + FamilySize,data=train, method = "knn",tuneLength = 10 )


#Question 3b
preds_knn <- predict(model_knn, test)

#Question 4a
#Confusion matrix for naives bayes classifier
conMat_naives <- confusionMatrix(preds_naive,test$boughtelectronics)
conMat_naives$byClass['Balanced Accuracy']

#Confusion matrix for knn
conMat_knn <- confusionMatrix(preds_knn,test$boughtelectronics)
conMat_knn$byClass['Balanced Accuracy']

#Navies bayes has higher accuracy

#Question 4b
#Naive Bayes ROC cHART
pred_naive <- prediction(as.numeric(preds_naive), as.numeric(test$boughtelectronics))
perf <- performance(pred_naive,"tpr","fpr")
plot(perf, main="ROC curve Naive Bayes", colorize=T)

# And then a lift chart
perf2 <- performance(pred_naive,"lift","rpp")
plot(perf2, main="lift curve Naive Bayes", colorize=T)

# And then a gain chart
perf3 <- performance(pred_naive, "tpr", "rpp")
plot(perf3, main="Gain curve Naive Bayes", colorize=T)

#Naive Bayes for knn classifier
pred_knn <- prediction(as.numeric(preds_knn), as.numeric(test$boughtelectronics))
perf4 <- performance(pred_knn,"tpr","fpr")
plot(perf4, main="ROC curve for knn classifier", colorize=T)

# And then a lift chart
perf5 <- performance(pred_knn,"lift","rpp")
plot(perf5, main="lift curve for knn classifier", colorize=T)

# And then a gain chart
perf6 <- performance(pred_knn, "tpr", "rpp")
plot(perf6, main="Gain curve  for knn classifier", colorize=T)

#The  hierarchical cluster model is better than Kmeans cluster model in clustering

#Question 5
#k means clustering
km.res <- kmeans(df, 2, nstart = 10)
fviz_cluster(km.res, df, ellipse.type = "norm")


#Question 6
#Hierarchical_clustering 
Hierarchical_clustering <-  fviz_nbclust(df, hcut, method = "silhouette")
Hierarchical_clustering

hc <- hclust(dist(df), "cen")
# Cut tree into 2 groups: 
sub_grp <- cutree(hc, k = 2)
# Create plot of clusters: 
fviz_cluster(list(data = df, cluster = paste0("Group", sub_grp)), 
             alpha = 1, 
             palette = "jco", 
             labelsize = 9, 
             ellipse.type = "norm") 




