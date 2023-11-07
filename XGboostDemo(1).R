library(MASS)
library(caret)
library(xgboost)
set.seed(1)
#Regression example
head(Boston)
intrain <- createDataPartition(Boston$medv,p=0.5,list = FALSE)
train1 <- Boston[intrain,]
test1 <- Boston[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
#Separate target variable and predictor variables
tr_label <- train1$medv 
ts_label <- test1$medv
#Convert dataframe to matrix as XGBoost only takes matrix
m_train <- as.matrix(train1[,-14])
m_test <- as.matrix(test1[,-14])
#Convert to XGBoost Matrix data structure
dtrain <- xgb.DMatrix(data = m_train,label = tr_label) 
dtest <- xgb.DMatrix(data = m_test,label=ts_label)
#Cross validate to find which round gives minimum error
xgbcv <- xgb.cv( data = dtrain, booster = "gblinear", lambda=0,alpha=0,nrounds = 1000, 
                 nfold = 5, showsd = T, stratified = T, 
                 print_every_n = 10, early_stopping_rounds = 20, 
                 maximize = F)
bst <- xgb.train(data=dtrain, booster = "gblinear", 
                 nrounds=798)
pred <- predict(bst, dtest)
mean((pred-ts_label)^2)

#Binary Classification example
library(ISLR)
head(Carseats)
#Create a binary variable for high (1) or low (0) sales with a threshold at 8
High <- ifelse(Sales<=8,0,1)
Carseats <- data.frame(Carseats,High)
intrain <- createDataPartition(Carseats$High,p=0.5,list = FALSE)
train1 <- Carseats[intrain,]
test1 <- Carseats[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
#Convert target to numeric
tr_label <- train1$High
ts_label <- test1$High
#One hot encode predictor variables to convert categorical variables to numeric
m_train <- model.matrix(~.+0,data = train1[,-12]) 
m_test <- model.matrix(~.+0,data = test1[,-12])
dtrain <- xgb.DMatrix(data = m_train,label = tr_label) 
dtest <- xgb.DMatrix(data = m_test,label=ts_label)
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=2, min_child_weight=1, 
               subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, 
                 nfold = 5, showsd = T, stratified = T, 
                 print_every_n = 10, early_stop_rounds = 20, 
                 maximize = F)
bst <- xgb.train(params=params, data=dtrain, nrounds=100)
pred <- predict(bst, dtest)
pred <- ifelse (pred > 0.5,1,0)
pred <- as.factor(pred)
ts_label <- as.factor(ts_label)
confusionMatrix (pred, ts_label)
mat <- xgb.importance (feature_names = colnames(m_train),model = bst)
mat
xgb.plot.importance (importance_matrix = mat) 

#Multi Class Classification Example
attach(iris)
head(iris)
intrain <- createDataPartition(iris$Species,p=0.5,list = FALSE)
train1 <- iris[intrain,]
test1 <- iris[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
#Convert target to numeric
tr_label <- as.numeric(train1$Species) -1
ts_label <- as.numeric(test1$Species) - 1
#One hot encode predictor variables to convert categorical variables to numeric
m_train <- model.matrix(~.+0,data = train1[,-5]) 
m_test <- model.matrix(~.+0,data = test1[,-5])
dtrain <- xgb.DMatrix(data = m_train,label = tr_label) 
dtest <- xgb.DMatrix(data = m_test,label=ts_label)
params <- list(booster = "gbtree", objective = "multi:softmax",num_class=3, 
               eta=0.3, gamma=0, max_depth=2, min_child_weight=1, 
               subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 1000, 
                 nfold = 5, showsd = T, stratified = T, 
                 print_every_n = 10, early_stop_rounds = 20, 
                 maximize = F)
bst <- xgb.train(params=params, data=dtrain, nrounds=100)
pred <- predict(bst, dtest)
pred <- ifelse (pred > 0.5,1,0)
pred <- as.factor(pred)
ts_label <- as.factor(ts_label)
confusionMatrix (pred, ts_label)
mat <- xgb.importance (feature_names = colnames(m_train),model = bst)
xgb.plot.importance (importance_matrix = mat) 

#Another example from the XGBoost documentation
library(xgboost)
data(agaricus.train)
data(agaricus.test)
train <- agaricus.train
test <- agaricus.test
str(agaricus.train)
bst <- xgboost(data = train$data, label = train$label, 
               max.depth = 2, eta = 1, nthread = 2, nround = 2, 
               objective = "binary:logistic", verbose=2)
pred <- predict(bst, test$data)
prediction <- as.numeric(pred > 0.5)
err <- mean(as.numeric(pred > 0.5) != test$label)
err
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, 
                 nround=2, watchlist=watchlist,
                 eval.metric = "error", eval.metric = "logloss", 
                 objective = "binary:logistic")
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, 
                 nthread = 2, nround=2, watchlist=watchlist, 
                 eval.metric = "error", eval.metric = "logloss", 
                 objective = "binary:logistic")
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
xgb.plot.tree(model = bst)



