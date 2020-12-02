calinski_harabasz<-function(X,clust,distance){
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  #print(m)
  #wk <- sum((X-m)^2)
  test <- split(df, f = df$clust)
  list_wk <- sapply(test,function(x) wk_var(x,m))
  list <- sapply(test,function(x) B_var(x,m))
   B <- sum(unlist(list))
   wk <- sum(unlist(list_wk))
  DEM <- (N-numb_clust)*B
  #print(DEM)
  NUM <- (numb_clust - 1)*wk
  SCH <- (DEM)/(NUM)
  print(SCH)
}

ball_hall<-function(X,clust,distance){
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  test <- split(df, f = df$clust)
  list_wk <- sapply(test,function(x) wss(x,m))
  wk <- sum(unlist(list_wk))
  b_h <- wk/(numb_clust)
  print(b_h)
}

hartigan<-function(X,clust,distance){
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  test <- split(df, f = df$clust)
  list_wk <- sapply(test,function(x) wk_var(x,m))
  list <- sapply(test,function(x) B_var(x,m))
  B <- sum(unlist(list))
  wk <- sum(unlist(list_wk))
  har <- log(B/wk)
  print(har)
}
Xu <- function(X,clust,distance){
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  D <- ncol(X)
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  test <- split(df, f = df$clust)
  list_wk <- sapply(test,function(x) wk_var(x,m))
  wk <- sum(unlist(list_wk))
  xu <- D * log(square(wk/(D*N^2))) + log(numb_clust)
  
}

B_var <- function(x,m){
  y <- x[1:(length(x)-1)]
  mk <- apply(y,2,function(x){mean(x, na.rm=TRUE)})
  mk <- nrow(y)*(mk-m)^2
  #print(mk)
} 

wk_var <- function(x,m){
  y <- x[1:(length(x)-1)]
  mk <- apply(y,2,function(x){mean(x, na.rm=TRUE)})
  wk <- apply(y,1,function(y){(y-mk)^2})
  w <- sum(wk)
  
}

wss <- function(x,m){
  y <- x[1:(length(x)-1)]
  mk <- apply(y,2,function(x){mean(x, na.rm=TRUE)})
  wk <- apply(y,1,function(y){(y-mk)^2})
  w <- sum(wk)/nrow(y)
  
}



x <- c(1,2,4,2,7,7)
y <- c(0,1,1,3,0,1)

test <- data.frame(x, y)
matrix_test <- data.matrix(test)
print(test)
print(matrix_test)
cluster_test <- c("jaune","jaune","rouge","rouge","noir","noir")
cluster_name <- c(1,1,3,3,3,3)
test_1 <- as.integer(cluster_name)
library(clusterCrit)  
calinski_harabasz(test,cluster_name,"1")
intCriteria(matrix_test,test_1,c("Dunn","Silhouette","Davies_Bouldin","Calinski_Harabasz","Log_SS_Ratio","Ball_Hall"))
hartigan(test,cluster_name,"1")
ball_hall(test,cluster_name,"1")
