library(ISLR)
library(caret)
set.seed(324)
Auto <- na.omit(Auto)

#Create training and test datasets. The p parameter determines the split(e.g. 75/25)
intrain <- createDataPartition(Auto$mpg,p=0.75,list = FALSE)
train1 <- Auto[intrain,]
test1 <- Auto[-intrain,]
#Setup the k fold cross validation. method is the type of cross validation and number is the number of folds
trctrl <- trainControl(method = "cv", number = 10)

OLS_fit <- train(mpg ~ horsepower+weight, data = train1, method = "lm",
                 trControl=trctrl, preProcess=c('scale', 'center'))
#To see the accuracy in the training dataset (average of the CV resamples)
OLS_fit$results
OLS_fit$results$RMSE^2
#Predict for the test dataset
predictions <- predict(OLS_fit, newdata = test1)
#Mean squared error in the test dataset
mean(( predictions - test1$mpg)^2)
#To see the results for each resample
OLS_fit$resample

#Setup the repeated k fold cross validation. 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

OLS_fit <- train(mpg ~ horsepower+weight, data = train1, method = "lm",
                 trControl=trctrl, preProcess=c('scale', 'center'))
#To see the accuracy in the training dataset (average of the CV resamples)
OLS_fit$results
OLS_fit$results$RMSE^2
predictions <- predict(OLS_fit, newdata = test1)
mean(( predictions - test1$mpg)^2)
#To see the results for each resample
OLS_fit$resample

#Setup the Leave One Out cross validation. 
trctrl <- trainControl(method = "LOOCV")
OLS_fit <- train(mpg ~ horsepower+weight, data = train1, method = "lm",
                 trControl=trctrl, preProcess=c('scale', 'center'))
OLS_fit$results
OLS_fit$results$RMSE^2
predictions <- predict(OLS_fit, newdata = test1)
mean(( predictions - test1$mpg)^2)

#Setup the bootstrap. 
trctrl <- trainControl(method = "boot", number = 100)
OLS_fit <- train(mpg ~ horsepower+weight, data = train1, method = "lm",
                 trControl=trctrl, preProcess=c('scale', 'center'))
OLS_fit$results
OLS_fit$results$RMSE^2
predictions <- predict(OLS_fit, newdata = test1)
mean(( predictions - test1$mpg)^2)
#To see the results for each resample
OLS_fit$resample

#Setup the Leave-group-out CV. p is the percentage to use for the training dataset 
trctrl <- trainControl(method = "LGOCV", number = 10, p=.75)
OLS_fit <- train(mpg ~ horsepower+weight, data = train1, method = "lm",
                 trControl=trctrl, preProcess=c('scale', 'center'))
OLS_fit$results
OLS_fit$results$RMSE^2
predictions <- predict(OLS_fit, newdata = test1)
mean(( predictions - test1$mpg)^2)
#To see the results for each resample
OLS_fit$resample

