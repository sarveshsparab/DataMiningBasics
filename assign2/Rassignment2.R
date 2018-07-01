
trans <- read.csv("C:/RData/assign2/big_data_input.csv")
#trans <- read.csv("C:/RData/assign2/small_data_input.csv")

support <- 2
partition <- 3

###################################################### APRIORI ALGORITHM ######################################################

genCandidateItemsets <- function(l){
  c <- list()
  iItemlist <- list()
  jItemlist <- list()
  newItemlist <- list()
  len <- length(l)
  if(len>1){
    for (i in 1:(len-1)) {
      iItemlist <- l[[i]]
      itemListLen <-length(iItemlist)
      for (j in (i+1):len) {
        jItemlist <- l[[j]]
        net <- 0
        if(itemListLen>1){
          for(k in 1:(itemListLen-1)){
            if(iItemlist[[k]]==jItemlist[[k]]){
              net <- net +1
            }
          }
        }
        if(net == (itemListLen-1)){
          newItemlist <- iItemlist
          newItemlist <- append(newItemlist,jItemlist[[itemListLen]])
          c <- append(c,list(newItemlist))
        }
      }
    }
  }
  return(c)
}

prune <- function(c,l){
  cp <- list()
  cLen <- length(c)
  lLen <- length(l)
  groupSet <- list()
  innerSubset <- list()
  if(cLen>0){
    for (i in 1:cLen) {
      groupSet <- c[[i]]
      groupSetLen <- length(groupSet)
      net <- 0
      for(j in 1:groupSetLen){
        innerSubset <- NULL
        innerSubset <- list()
        for (k in 1:groupSetLen) {
          if(j!=k){
            innerSubset <- append(innerSubset,groupSet[[k]])
          }
        }
        for (m in 1:lLen) {
          comRes <- identical(l[[m]],innerSubset)
          if(comRes == TRUE){
            net <- net+1
          }
        }
      }
      if(net == groupSetLen){
        cp <- append(cp,list(groupSet))
      }
    }
  }
  return(cp)
}

myApriori <- function(transDB , support, result){
  items <- ncol(transDB)
  trans <- nrow(transDB)
  colTitles <- colnames(transDB)
  supRows <- ceiling(support*trans/100)
  if(result != ""){
    cat("------------myApriori Statictics----------------", file = result, append=TRUE, sep = "\n")
    cat(c("No of items ",paste(" |-> ",items," ")), file = result, append=TRUE, sep = "\n")
    cat(c("No of transactions ",paste(" |-> ",trans," ")), file = result, append=TRUE, sep = "\n")
    cat(c("Min support percentage",paste(" |-> ",support," ")), file = result, append=TRUE, sep = "\n")
    cat(c("Min support rows",paste(" |-> ",supRows," ")), file = result, append=TRUE, sep = "\n")
  }
  answerList <- list()
  l <- list()
  c <- list()
  cp <- list()
  for(i in 1:items){
    itemsets <- list()
    itemsets[[1]] <-colTitles[[i]]
    c <- append(c,list(itemsets)) 
  }
  for(i in 1:items){
    colSum <- sum(transDB[[i]])
    if(colSum>=supRows){
      itemsets <- list()
      itemsets[[1]] <-colTitles[[i]]
      l <- append(l,list(itemsets)) 
    }
  }
  iter <- 2
  while (length(l)!=0) {
    c <- genCandidateItemsets(l)
    cp <- prune(c,l)
    answerList <- append(answerList,l)
    cpLen <- length(cp)
    l <- NULL
    if(cpLen>0){
      for (i in 1:cpLen) {
        itemSet <- cp[[i]]
        itemSetLen <- length(itemSet)
        net <- transDB[itemSet[[1]]]
        for (j in 1:itemSetLen) {
          net <- net & transDB[itemSet[[j]]]
        }
        netSup <- sum(net)
        if(netSup >= supRows){
          l <- append(l,list(itemSet))
        }
      }
    }
    iter <- iter+1
  }
  return(list(answerList,iter))
}

saveToFile <- function(answer,iter,result){
  cat(c("No of iterations",paste(" |-> ",iter," ")), file = result, append=TRUE, sep = "\n")
  cat(c("No of frequent itemsets",paste(" |-> ",length(answer)," ")), file = result, append=TRUE, sep = "\n")
  cat("Frequent ItemSets : ", file = result, append=TRUE, sep = "\n")
  ansLen <- length(answer)
  for (i in 1:ansLen) {
    itemSet <- answer[[i]]
    itemSetLen <- length(itemSet)
    cat(paste(" |-> "), file = result, append=TRUE, sep = " ")
    for (j in 1:itemSetLen) {
      cat(paste(itemSet[[j]]," "), file = result, append=TRUE, sep = " ")
    }
    cat("", file = result, append=TRUE, sep = "\n")
  }
  cat("------------------------------------------------", file = result, append=TRUE, sep = "\n")
}

resultApriori <- "C:/RData/assign2/apriori_output.txt"
answerApriori <- myApriori(trans,support,resultApriori)
saveToFile(answerApriori[[1]],answerApriori[[2]],resultApriori)
print("APRIORI DONE!!!!!!!")

###################################################### PARTITION ALGORITHM ######################################################


myPartition <- function(transDB,support,result,parts){
  items <- ncol(transDB)
  trans <- nrow(transDB)
  colTitles <- colnames(transDB)
  supRows <- ceiling(support*trans/100)
  transPerPart <- ceiling(trans/parts)
  if(result!=""){
    cat("------------myPartition Statictics----------------", file = result, append=TRUE, sep = "\n")
    cat(c("No of items ",paste(" |-> ",items," ")), file = result, append=TRUE, sep = "\n")
    cat(c("No of transactions ",paste(" |-> ",trans," ")), file = result, append=TRUE, sep = "\n")
    cat(c("Min support percentage",paste(" |-> ",support," ")), file = result, append=TRUE, sep = "\n")
    cat(c("Min support rows",paste(" |-> ",supRows," ")), file = result, append=TRUE, sep = "\n")
    cat(c("Partitions Made",paste(" |-> ",parts," ")), file = result, append=TRUE, sep = "\n")
    cat(c("Rows in each partition",paste(" |-> ",transPerPart," ")), file = result, append=TRUE, sep = "\n")
  }
  
  partAnswer <- list()
  iter<-0
  for (i in 0:(parts-1)) {
    lo <- 1+i*transPerPart
    hi <- transPerPart+i*transPerPart
    if(hi>trans)
      hi<-trans
    temp <- myApriori(transDB[lo:hi,],support,"")
    partAnswer <- append(partAnswer,temp[[1]])
    iter<-iter+temp[[2]]
  }
  partAnswer <- unique(partAnswer)
  answerList <- list()
  
  for (i in 1:length(partAnswer)) {
    itemSet <- partAnswer[[i]]
    itemSetLen <- length(itemSet)
    net <- transDB[itemSet[[1]]]
    for (j in 1:itemSetLen) {
      net <- net & transDB[itemSet[[j]]]
    }
    netSup <- sum(net)
    if(netSup >= supRows){
      answerList <- append(answerList,list(itemSet))
    }
  }
  
  return(list(answerList,iter))
}

resultPartition <- "C:/RData/assign2/partition_output.txt"
answerPartition <- myPartition(trans,support,resultPartition,partition)
saveToFile(answerPartition[[1]],answerPartition[[2]],resultPartition)
print("PARTITION DONE!!!!!!!")
