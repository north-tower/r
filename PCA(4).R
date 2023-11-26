data("USArrests")
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pr.out<-prcomp(USArrests, scale=TRUE)
#The centering mean used
pr.out$center
#The scaling factor that was used to scale the variables
pr.out$scale
#To see the eigen vectors (loadings for each variable for each PC)
pr.out$rotation
#To see the square root of the eigen values
pr.out$sdev
#To see the rotated data (i.e the centered/scaled data multiplied by the rotation matrix)
pr.out$x
#Plot biplot
biplot(pr.out, scale=0)
#Change the direction of the plot.  Does not alter the values.
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
#Compute percentage variance explained
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

#Kernel PCA
library(kernlab)
kpc <- kpca(~.,data=USArrests,kernel="rbfdot",
            kpar=list(sigma=0.2),features=5)
#print the principal component vectors
pcv(kpc)

#PCA using Caret
library(caret)
intrain <- createDataPartition(USArrests$Murder,p=0.75,list = FALSE)
train1 <- USArrests[intrain,]
test1 <- USArrests[-intrain,]
trctrl <- trainControl(method = "cv", number = 10, 
                       preProcOptions = list(thresh = 0.85)) #use list(pcaComp = 7) to specify number of principal components
OLS_fit <- train(Murder ~., data = train1, method = "lm",
                   trControl=trctrl, preProcess=c('scale', 'center','pca'))
OLS_fit
predictions <- predict(OLS_fit, newdata = test1)
mean(( predictions - test1$Murder)^2)
OLS_fit$preProcess
OLS_fit$preProcess$rotation