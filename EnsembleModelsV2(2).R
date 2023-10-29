library(ISLR)
Carseats <- data.frame(Carseats)
#Create the target variable High to indicate high or low sales for classification
Carseats$High <- as.factor(ifelse(Carseats$Sales<=8,"No","Yes"))
set.seed(3333)
library(caret)

# Fitting Classification Trees
intrain <- createDataPartition(Carseats$High,p=0.75,list = FALSE)
train1 <- Carseats[intrain,]
test1 <- Carseats[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
tree_fit <- train(High ~.-Sales, data = train1, method = "rpart",
                    trControl=trctrl)
#To see the tuned complexity parameter (Gini Coeff)
tree_fit$bestTune
#To see the tree splits
tree_fit$finalModel
#Plot complexity parameter tuning runs
plot(tree_fit)
#Plot the tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree_fit$finalModel)
#Predict
predictions <- predict(tree_fit, newdata = test1)
#Performance metrics
confusionMatrix(predictions,test1$High)
#To see the importance of the variables
treeImp <- varImp(tree_fit, scale = TRUE)
treeImp
plot(treeImp)

# Fitting Regression Trees
#Load library MASS to get Boston dataset
library(MASS)
intrain <- createDataPartition(Boston$medv,p=0.75,list = FALSE)
train1 <- Boston[intrain,]
test1 <- Boston[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
tree_fit <- train(medv ~., data = train1, method = "rpart",
                  trControl=trctrl)
#To see the tuned complexity parameter (Gini Coeff)
tree_fit$bestTune
#To see the tree splits
tree_fit$finalModel
#Plot complexity parameter tuning runs
plot(tree_fit)
#Plot the tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree_fit$finalModel)
#Predict
predictions <- predict(tree_fit, newdata = test1)
#Performance metrics
#Calculate Mean Square Error (MSE)
mean(( predictions - test1$medv)^2)
#To see the importance of the variables
treeImp <- varImp(tree_fit, scale = TRUE)
treeImp
plot(treeImp)

#Random Forest
intrain <- createDataPartition(Boston$medv,p=0.50,list = FALSE)
train1 <- Boston[intrain,]
test1 <- Boston[-intrain,]
#Note: if you want to use out of bag error estimation use method = "oob" below without the number parameter
trctrl <- trainControl(method = "cv", number = 10)
#Fit the random forest (method = "rf"). Set importance = TRUE to have the variable importance calculated.
#Parameter mtry in the train function lets you set how many variables are considered at each split
forest_fit <- train(medv ~., data = train1, method = "rf",importance = T,
                   trControl=trctrl)
#To see model details
forest_fit
#To see the tuned mtry parameter.  Mtry is the number of randomly selected predictors
forest_fit$bestTune
#To see the the % variance explained
forest_fit$finalModel
#Plot complexity parameter tuning runs
plot(forest_fit)
#Predict
predictions <- predict(forest_fit, newdata = test1)
#Calculate MSE
mean(( predictions - test1$medv)^2)
#To see the importance of the variables
forestImp <- varImp(forest_fit)
forestImp
plot(forestImp)

#Bagging
#No tuning parameters supported
bag_fit <- train(medv ~., data = train1, method = "treebag",
                    trControl=trctrl)
bag_fit
predictions <- predict(bag_fit, newdata = test1)
mean(( predictions - test1$medv)^2)
#To see the importance of the variables
bagImp <- varImp(bag_fit, scale=TRUE)
bagImp
plot(bagImp)

# Boosting
intrain <- createDataPartition(Carseats$High,p=0.75,list = FALSE)
train1 <- Carseats[intrain,]
test1 <- Carseats[-intrain,]
trctrl <- trainControl(method = "cv", number = 5)
#Fit Ada Boost
boost_fit <- train(High ~.-Sales, data = train1, method = "ada")
#To see model details
boost_fit
boost_fit$bestTune
#Plot complexity parameter tuning runs
plot(boost_fit)
#Predict
predictions <- predict(boost_fit, newdata = test1)
#Performance metrics
confusionMatrix(predictions,test1$High)
#To see the importance of the variables
boostImp <- varImp(boost_fit)
boostImp
plot(boostImp)

#XGBoost
library(xgboost)
intrain <- createDataPartition(Boston$medv,p=0.5,list = FALSE)
train1 <- Boston[intrain,]
test1 <- Boston[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
#Separate target variable and predictor variables
tr_label <- train1$medv 
ts_label <- test1$medv
#Convert dataframe to matrix as XGBoost only takes matrix.  Take out the target variable (dependent variable)
m_train <- as.matrix(train1[,-14])
m_test <- as.matrix(test1[,-14])
#Convert to XGBoost Matrix data structure
dtrain <- xgb.DMatrix(data = m_train,label = tr_label) 
dtest <- xgb.DMatrix(data = m_test,label = ts_label)
#Fit XGBoost (method = "xgbTree")
#nrounds (# Boosting Iterations), max_depth (Max Tree Depth), eta (learning rate), gamma(regularization)
#Create hyperparameter grid and pass into tuneGrid
grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
boost_fit <- train(x=dtrain, y=tr_label, method = "xgbTree",
                   tuneGrid=grid_default,trControl=trctrl,objective = "reg:squarederror")
#If you want caret to find the parameters by CV don't specify tuneGrid
boost_fit <- train(x=dtrain, y=tr_label, method = "xgbTree",
                   trControl=trctrl,objective = "reg:squarederror")
boost_fit
boost_fit$bestTune
#Plot parameter tuning runs
plot(boost_fit)
#Predict
predictions <- predict(boost_fit, newdata = dtest)
#Mean Squared Error
mean(( predictions - test1$medv)^2)

#Feature importance using caret
caret_imp <- varImp(boost_fit)
caret_imp
plot(caret_imp)

#Feature importance using xgBoost library
xgb_imp <- xgb.importance(feature_names = boost_fit$finalModel$feature_names,
                          model = boost_fit$finalModel)
xgb_imp
xgb.plot.importance(xgb_imp)

# Example of Stacking algorithms
# create submodels
library(caretEnsemble)
# Load the dataset
data(GermanCredit)
dataset <- GermanCredit
trctrl <- trainControl(method="cv", number=10, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('rpart', 'knn', 'svmRadial')
set.seed(123)
models <- caretList(Class~., data=dataset, trControl=trctrl, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
# correlation between results
modelCor(results)
# stack using glm
stackControl <- trainControl(method="cv", number=10, savePredictions=TRUE, classProbs=TRUE)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
# stack using random forest
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)

#Stacking without using caretEnsemble package
intrain <- createDataPartition(GermanCredit$Class,p=0.50,list = FALSE)
train1 <- GermanCredit[intrain,]
test1 <- GermanCredit[-intrain,]
trctrl <- trainControl(method="cv", number=10, savePredictions=TRUE, classProbs=TRUE)
rpart_fit <- train(Class ~., data = train1, method = "rpart",
                    trControl=trctrl)
pred_rpart <- predict(rpart_fit, newdata = test1)
confusionMatrix(pred_rpart, test1$Class)$overall[1]
knn_fit <- train(Class ~., data = train1, method = "knn",
                   trControl=trctrl)
pred_knn <- predict(knn_fit, newdata = test1)
confusionMatrix(pred_knn, test1$Class)$overall[1]
svm_fit <- train(Class ~., data = train1, method = "svmRadial",
                   trControl=trctrl)
pred_svm <- predict(svm_fit, newdata = test1)
confusionMatrix(pred_svm, test1$Class)$overall[1]
results <- resamples(list(mod1 = rpart_fit, mod2 = knn_fit, mod3=svm_fit)) 
modelCor(results)
#Construct data frame with predictions
predDF <- data.frame(pred_rpart, pred_knn, pred_svm, class = test1$Class)
#Combine models using random forest
combModFit.rf <- train(as.factor(class) ~ ., method = "rf", data = predDF, distribution = "multinomial")
combPred.rf <- predict(combModFit.rf, predDF)
confusionMatrix(combPred.rf, predDF$class)$overall[1]
