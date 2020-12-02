davies_bouldin<-function(X,clust,distance)
  {
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  df_mean <-X[FALSE,]
  vect_dist <- c()
  vect_max <- c()
  vect_db <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    mean_clu <- apply(group_x,2,function(x){mean(x, na.rm=TRUE)})
    print(mean_clu)
    dist_clu <- apply(group_x,1,function(x) { dist(rbind(x,mean_clu)) }  )
    print(dist_clu)
    mean_dist <- mean(dist_clu)
    df_mean[nrow(df_mean)+1,] <- mean_clu
    vect_dist <-append(vect_dist,mean_dist)
  }
  #print(df_mean)
  #print(vect_dist)
  #print(length(vect_dist))
  #print(nrow(df_mean))
  for (i in 1:length(vect_dist))
  {
    for(j in 1:nrow(df_mean)){
      if(i!=j){
        #print(vect_dist[i] + vect_dist[j])
        #print(dist(rbind(df_mean[i,],df_mean[j,])))
        db <-(vect_dist[i] + vect_dist[j])/dist(rbind(df_mean[i,],df_mean[j,]))
        vect_db <- append(vect_db,db)
      }
    }
    print(vect_db)
    max_db <- max(vect_db)
    print(max_db)
    vect_db <- c()
    vect_max <- append(vect_max,max_db)
  }
  #print(vect_db)
  db_max <- mean(vect_max)
  print(db_max)
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
davies_bouldin(test,cluster_test,"1")
intCriteria(matrix_test,test_1,c("Dunn","Silhouette","Davies_Bouldin"))

