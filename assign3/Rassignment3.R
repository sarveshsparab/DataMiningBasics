# dataset <- read.csv("C:/RData/assign3/dataset.csv")
dataset <- read.csv("C:/RData/assign3/randomData.csv")
dataset_features <- dataset
dataset_features$class <- NULL
original_plot <- plot(dataset[c("atr4","atr5")], col = dataset$class, main = "Original Data")


###################################################### KMEANS ALGORITHM ######################################################
my_kmeans <- function(dataset_features,k){

  results <- kmeans(dataset_features,k)
  print(results)
  
  table <- table(dataset$class, results$cluster)
  print(table)
  
  data_plot <- plot(dataset[c("atr4","atr5")], col = results$cluster, main="k means")
}

###################################################### KMEDOIDS ALGORITHM ######################################################
my_kmedoids <- function(dataset_features,k){
  
  results <- cluster::pam(dataset_features,k,diss=F)
  print(results)
  plot(results)
  plot(results$data, col = results$clustering)
  points(results$medoids, col = 1:4, pch = 4)
  
  table <- table(dataset$class, results$cluster)
  print(table)
  
  data_plot <- plot(dataset[c("atr4","atr5")], col = results$clustering, main="k medoids")
  points(results$medoids, col = 1:4, pch = 4)
}

###################################################### AGNES ALGORITHM ######################################################
my_agnes <- function(dataset_features){
  
  results <- cluster::agnes(dataset_features, metric = "manhattan", stand = TRUE)
  print(results)
  
  data_plot <- plot(results)
}

###################################################### DIANA ALGORITHM ######################################################
my_diana <- function(dataset_features){
  
  results <- cluster::diana(dataset_features, metric = "manhattan", stand = TRUE)
  print(results)
  
  data_plot <- plot(results)
}

###################################################### DB SCAN ALGORITHM ######################################################
my_dbscan <- function(dataset_features,e,m){
  
  results <- dbscan::dbscan(dataset_features,eps = e, minPts = m)
  print(results)
  
  data_plot <- plot(dataset_features, col = results$cluster + 1L)
}

###################################################### COMMAND CENTRAL ######################################################

my_kmeans(dataset_features,3)
#my_kmedoids(dataset_features,3)
#my_agnes(dataset_features)
#my_diana(dataset_features)
#my_dbscan(dataset_features,2,10)
