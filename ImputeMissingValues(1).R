library(mlbench)
data ("BostonHousing", package="mlbench")

# Introduce missing values
set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
sum(is.na(BostonHousing$ptratio))
sum(is.na(BostonHousing$ptratio))
library(Hmisc)
impute(BostonHousing$ptratio, mean)  # replace with mean
impute(BostonHousing$ptratio, median)  # median
impute(BostonHousing$ptratio, 20)  # replace specific number
# or if you want to impute manually
BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- mean(BostonHousing$ptratio, na.rm = T)
library(VIM)
#Plot missing values proportion
a <- aggr(BostonHousing)
summary(a)
plot(a)
#Impute using kNN
BostonHousing <- kNN(data = BostonHousing, variable = 'ptratio', k=6, imp_var=FALSE)  # perform knn imputation.
#Impute using regression
BostonHousing <- regressionImp(ptratio ~ crim +zn + indus, data = BostonHousing, imp_var = FALSE)
#Plot after imputing
a <- aggr(BostonHousing)
summary(a)
plot(a)


library(caret)
BostonHousing <- preProcess(BostonHousing, method='knnImpute')

intrain <- createDataPartition(BostonHousing$ptratio,p=0.75,list = FALSE)
train1 <- BostonHousing[intrain,]
test1 <- BostonHousing[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
reg_fit <- train(ptratio ~., data = train1, method = "lm",
                   trControl=trctrl, preProcess=c('knnImpute'))
