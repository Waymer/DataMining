load("data/TrainData.RData")

## Unsupervised

# Model 1: K-Means



unsupModel1 <- function(x_new) {
  x <- data.matrix(x_new)
  pc <- prcomp(x)
  k2 <- kmeans(pc$x[,1:10], centers = 2, iter.max = 100, nstart = 10, 
               algorithm = "Lloyd")
  return(k2$cluster - 1)
}



# Hierarchical Clustering

unsupModel2 <- function(dat) {
  acluster <- hclust(dist(data.frame(dat)), method = "average")
  aclusterCut <- cutree(acluster, 2)
  aclusterCut[aclusterCut == 1] = 0
  aclusterCut[aclusterCut == 2] = 1
  return(aclusterCut)
}

unsupModel2Labels = unsupModel2(x)

## Supervised

library(class)

supModel1 <- function(testX) {
  #knn
  knn_yhat <- knn(trainX, testX, trainY, k = 1)
  # model 2 yhat 
  y <- factor(trainY)
  train <- cbind(trainX, y)
  tree.oj <- tree(y~., data = train)
  prune.oj <- prune.misclass(tree.oj, best = 2)
  tree_yhat <- predict(prune.oj, testX, type = "class")
  # model 3 yhat
  # voting
}
library(randomForest)

supModel2 <- function(testX) {
	bag.tree = randomForest(label~., data = train, mtry=248, importance = T)
	bag_yhat = predict(bag.tree, testX)
	return(bag_yhat)
}