load("data/TrainData.RData")

## Unsupervised

# Model 1: K-Means

x <- data.matrix(trainX)
pc <- prcomp(x)
k2 <- kmeans(pc$x[,1:10], centers = 2, iter.max = 100, nstart = 10, 
             algorithm = "Lloyd")
unsupModel1 <- function(testX) {
  return(clusters(x_new, k2$centers) - 1)
}



## Supervised

library(class)

supModel1 <- function(testX) {
  #knn
  knn_yhat <- knn(trainX, testX, trainY, k = 1)
  # model 2 yhat 
  # model 3 yhat
  # voting
}