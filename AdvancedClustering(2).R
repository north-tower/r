data("iris")
iris <- as.matrix(iris[,1:4])

#DBSCAN
library(dbscan)
## find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!
kNNdistplot(iris, k = 5)
abline(h=.5, col = "red", lty=2)
res <- dbscan(iris, eps = .5, minPts = 5)
res
#Cluster 0 is noise
res$cluster
## plot clusters and add noise (cluster 0) as crosses.
plot(iris, col=res$cluster)
points(iris[res$cluster==0,], pch = 3, col = "grey")

#CLIQUE
library(subspace)
data("subspace_dataset")
res <- CLIQUE(subspace_dataset, xi = 5, tau = 0.2)
#Each of these clusters then holds a vector representing its subspace 
#and a vector with the indexes of the objects the belong in this cluster
res[[1]]
res[[1]]$subspace
res[[1]]$objects
res[[2]]
res[[2]]$subspace
res[[2]]$objects
plot(res,subspace_dataset)

#Spectral Clustering
# Create data
library(mlbench)
set.seed(111)
obj <- mlbench.spirals(100,1,0.025)
my.data <-  4 * obj$x
plot(my.data)

#Run spectral clustering
library(kernlab)
sc <- specc(my.data, centers=2)
sc
#Cluster centers
centers(sc)
#No of points in each cluster
size(sc)
#Within cluster sum of squares
withinss(sc)
plot(my.data, col=sc, pch=4)            # estimated classes (x)
points(my.data, col=obj$classes, pch=5) # true classes (<>)

# Gaussian Mixture Clustering
library(mclust)
fit <- Mclust(iris)
#Z is a matrix whose[i,k]th entry is the probability that observations in the 
#data belongs to the kth class.
fit$z
#To see the cluster numbers for each point
fit$classification
# display the best model
summary(fit)  

#Constrained K Means
library(conclust)
data = matrix(c(0, 1, 1, 0, 0, 0, 1, 1), nrow = 4)
mustLink = matrix(c(1, 2), nrow = 1)
cantLink = matrix(c(1, 4), nrow = 1)
k = 2
pred = ckmeans(data, k, mustLink, cantLink)
#See cluster labels
pred

#Cluster Valiation
library(clValid)
data(mouse)
express <- mouse[,c("M1","M2","M3","NC1","NC2","NC3")]
rownames(express) <- mouse$ID
#Internal Measures
intern <- clValid(express, 2:6, clMethods = c("hierarchical","kmeans"),
                  validation="internal")
summary(intern)
optimalScores(intern)
plot(intern)

# Stability Measures
stab <- clValid(express, 2:6, clMethods = c("hierarchical","kmeans"),
                  validation="stability")
summary(stab)
