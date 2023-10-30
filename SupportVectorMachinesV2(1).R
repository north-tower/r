# Support Vector Machine using Caret
library(caret)
data(iris)
#Create traning test split
intrain <- createDataPartition(iris$Species,p=0.75,list = FALSE)
train1 <- iris[intrain,]
test1 <- iris[-intrain,]
#Create cross validation
trctrl <- trainControl(method = "cv", number = 10)
#Fit the SVM model to training data
#svmRadial uses the Radial Kernel.  
#You can explicitly specify the cost parameter by using the C = option 
#and the gamma (sigma) parameter using the sigma = option
modSVMFit <- train(Species ~ .,method="svmRadial",sigma =.2,trControl=trctrl,data=train1)
#See model fit details
modSVMFit$finalModel
#See the tuning parametrs used (cost C, and sigma of the radial kernel function)
modSVMFit$bestTune
#See the results details by each optimization run
modSVMFit$results
#Predict test dataset
SVMpredict <- predict(modSVMFit,test1)
confusionMatrix(SVMpredict,test1$Species)

#Use a polynomial Kernel
modSVMFit <- train(Species ~ .,method="svmPoly",degree = 3, trControl=trctrl,data=train1)
#See model fit details
modSVMFit$finalModel
#The polynomial kernel is defined as (scale * crossprod(x, y) + offset)^degree
#See the tuning parametrs used (cost C, Scale of the kernel function)
modSVMFit$bestTune
#See the results details by each optimization run
modSVMFit$results
#Predict test dataset
SVMpredict <- predict(modSVMFit,test1)
confusionMatrix(SVMpredict,test1$Species)


#Use a tuning grid to tune parameters.  Need one column for each parameter that can be tuned
grid <- expand.grid(C=c(.1,1,5,10), degree=c(2,3,4), scale=c(1,2))
modSVMFit <- train(Species ~ .,method="svmPoly",tuneGrid=grid, trControl=trctrl,data=train1)
#See model fit details
modSVMFit$finalModel
#The polynomial kernel is defined as (scale * crossprod(x, y) + offset)^degree
#See the tuning parametrs used (cost C, Scale of the kernel function)
modSVMFit$bestTune
#See the results details by each grid search run
modSVMFit$results
#Predict test dataset
SVMpredict <- predict(modSVMFit,test1)
confusionMatrix(SVMpredict,test1$Species)


# Application to Gene Expression Data

library(ISLR)
train1 = data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
test1 = data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
#Create cross validation
trctrl <- trainControl(method = "cv", number = 10)
#Fit the SVM model to training data
#svmLinear uses the linear Kernel.  
modSVMFit <- train(y ~ .,method="svmLinear2",trControl=trctrl,data=train1)
SVMpredict <- predict(modSVMFit,test1)
confusionMatrix(SVMpredict,test1$y)
modSVMFit$finalModel

