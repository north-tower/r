#Model Performance
library(caret)
pima <- read.csv("C:/R Files/data/pima-indians-diabetes2.csv")
pima$Class <- as.factor(pima$Class)
#Split data
set.seed(12345) #Set the seed so every run produces the same result
intrain <- createDataPartition(pima$Class,p=0.75,list = FALSE)
train <- pima[intrain,]
test <- pima[-intrain,]
#Fit model
trctrl <- trainControl(method = "cv", number = 10, 
                       summaryFunction=twoClassSummary, 
                       classProbs=T, savePredictions = T)
modFit <- caret::train(Class ~ ., method='svmRadial',
                       trControl=trctrl,
                       data=train, metric='ROC'
                       )
#Predict
predictions <- predict(modFit, newdata = test)
#Performance measures
confusionMatrix(predictions,test$Class)
#Individual performance metrics functions
sensitivity(predictions,test$Class)
specificity(predictions,test$Class)
posPredValue(predictions,test$Class)
negPredValue(predictions,test$Class)
precision(predictions,test$Class)
recall(predictions,test$Class)
F_meas(predictions,test$Class)


#ROC
library(ROCR)
ROCRpred <- prediction(as.numeric(predictions), as.numeric(test$Class))
#ROC Curve
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf,colorize = TRUE)
abline(0, 1)
#Gains curve
ROCRgains <- performance(ROCRpred, 'tpr','rpp')
plot(ROCRgains, colorize = TRUE)
abline(0, 1)
#Lift Curve
ROCRlift <- performance(ROCRpred, 'lift','rpp')
plot(ROCRlift, colorize = TRUE)


library(pROC)
# ROC Curve
roccurve <- roc(test$Class ~ as.numeric(predictions))
roccurve$auc
roccurve$sensitivities
roccurve$specificities
plot(roccurve)

#Another library
library(MLeval)

# run MLeval
res <- evalm(modFit)

# get ROC
res$roc

# Other performance metrics assuming 0.5 cut-off
res$stdres

# get precision recall curve
res$proc

# get calibration curve
res$cc

# get precision recall gain curve
res$prg
