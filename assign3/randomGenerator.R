buildRandomCLuster <- function(clusterCount, m, meanInc , s, points){
  filePath <- "C:/RData/assign3/randomData.csv"
  if(file.exists(filePath)){
    file.remove(filePath)
  }
  for(i in 1:clusterCount){
    cluster = data.frame(atr4 = rnorm(points, mean=(m+meanInc*(i-1)),sd=s), atr5  = rnorm(points, mean=(m+meanInc*(i-1)),sd=s), class = i)
    if(i==1){
      write.table(cluster,filePath, append = T, sep =',' , row.names = F)
    }else{
      write.table(cluster,filePath, append = T, sep =',' , row.names = F, col.names = F)
    }
  }
}

buildRandomCLuster(3,5,8,3,100)

dataset <- read.csv("C:/RData/assign3/randomData.csv")
dataset_features <- dataset
dataset_features$class <- NULL
plot(dataset_features, col=dataset$class)
