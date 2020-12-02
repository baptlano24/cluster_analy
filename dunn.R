dunn_mean<-function(X,clust,distance){
  "dunn evaluation par le rapport du min de la distance entre le point moyen de chaque cluster et la distance max intra cluster"
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  df_mean <-X[FALSE,]
  print(df_mean)
  vect_dist <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #coefficient de silhouette par point
    mean_clu <- apply(group_x,2,  function(x) { mean(x, na.rm=TRUE) })
    #print(mean_clu)
    max_dist <- max(dist(group_x))
    #print(mean_clu)
    df_mean[nrow(df_mean)+1,] <- mean_clu
    vect_dist <-append(vect_dist,max_dist)
  }
  #print(vect_dist)
  print(df_mean)
  dunn_val <- min(dist(df_mean))/max(vect_dist)
  print(dunn_val)
}

dunn_min<-function(X,clust,distance){
  "dunn evaluation par le rapport du min de la distance-min des points de chaque cluster et la distance max intra cluster"  
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  vect_min <-c()
  vect_dist <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #coefficient de silhouette par point
    mean_clu <- apply(group_x,1,function(x) distance_min(x1=x,X=group_x,Y=clus_out,distance=1))
    print(mean_clu)
    max_dist <- max(dist(group_x))
    #print(mean_clu)
    vect_min <- append(vect_min,mean_clu)
    vect_dist <-append(vect_dist,max_dist)
  }
  #print(vect_dist)
  print(vect_min)
  dunn_val <- min(vect_min)/max(vect_dist)
  print(dunn_val)
}

dunn_max<-function(X,clust,distance){
  "dunn evaluation par le rapport du min de la distance-min des points de chaque cluster et la distance max intra cluster"  
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  vect_min <-c()
  vect_dist <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #coefficient de silhouette par point
    mean_clu <- apply(group_x,1,function(x) distance_max(x1=x,X=group_x,Y=clus_out,distance=1))
    print(mean_clu)
    max_dist <- max(dist(group_x))
    #print(mean_clu)
    vect_min <- append(vect_min,mean_clu)
    vect_dist <-append(vect_dist,max_dist)
  }
  #print(vect_dist)
  print(vect_min)
  dunn_val <- min(vect_min)/max(vect_dist)
  print(dunn_val)
}


distance_min <- function(x1,X,Y,distance){
  #distance moyenne d'un point dans son cluster
  test <- split(Y, f = Y$clust)
  Ik <- nrow(X)
  #distance d'un point par rapport aux autres points de son cluster
  #distance moyenne d'un point dans son cluster
  #calculer la distance moyenne du point pour les autres points de chaque cluster voisin
  clust_list <- sapply(test,function(x) distance_min_voi(x1,x))
  #choisir le cluster voisin (distance moyenne) le plus proche
  b <- min(unlist(clust_list))
  #appliquer la formule de silhouette pour un point
}

distance_min_voi <- function(x1,X,distance){
  Y <- X[1:(length(X)-1)]
  b_vec <- apply(Y, 1, function(x) (dist(rbind(x,x1))))
  b_min <- min(b_vec)
  print(b_min)
} 

distance_max <- function(x1,X,Y,distance){
  #distance moyenne d'un point dans son cluster
  test <- split(Y, f = Y$clust)
  Ik <- nrow(X)
  #calculer la distance moyenne du point pour les autres points de chaque cluster voisin
  clust_list <- sapply(test,function(x) distance_max_voi(x1,x))
  #choisir le cluster voisin (distance moyenne) le plus proche
  b <- max(unlist(clust_list))
  #appliquer la formule de silhouette pour un point
}

distance_max_voi <- function(x1,X,distance){
  Y <- X[1:(length(X)-1)]
  b_vec <- apply(Y, 1, function(x) (dist(rbind(x,x1))))
  b_min <- max(b_vec)
} 


  
x <- c(1,2,4,2,7,7)
y <- c(0,1,1,3,0,1)
  
test <- data.frame(x, y)
matrix_test <- data.matrix(test)
print(test)
print(matrix_test)
cluster_test <- c("jaune","jaune","rouge","rouge","noir","noir")
cluster_name <- c(1,1,2,2,3,3)
test_1 <- as.integer(cluster_name)
library(clusterCrit)  
dunn_min(test,cluster_test,"1")
intCriteria(matrix_test,test_1,c("Dunn","Silhouette"))


