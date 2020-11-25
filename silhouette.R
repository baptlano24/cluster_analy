

silhouette<-function(X,clust,distance){
  
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    Ik <- nrow(clus_in)
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #coefficient de silhouette par point
    s <- apply(group_x,1,function(x) distance_moyenne_a(x1=x,X=group_x,Y=clus_out,distance=1))
    print(s)
    s_sum <- sum(s)/Ik
    print(s_sum)#coefficient de silhouette par cluster
    }
  
}S

distance_moyenne_a <- function(x1,X,Y,distance){
  #distance moyenne d'un point dans son cluster
  test <- split(Y, f = Y$clust)
  Ik <- nrow(X)
  #distance d'un point par rapport aux autres points de son cluster
  a_vec <- apply(X, 1, function(x) (dist(rbind(x,x1))))
  #distance moyenne d'un point dans son cluster
  a <- sum(a_vec)/(Ik-1)
  #calculer la distance moyenne du point pour les autres points de chaque cluster voisin
  clust_list <- sapply(test,function(x) distance_moyenne_b(x1,x))
  #choisir le cluster voisin (distance moyenne) le plus proche
  b <- min(unlist(clust_list))
  #appliquer la formule de silhouette pour un point
  s <- (b-a)/max(b,a)
}

distance_moyenne_b <- function(x1,X,distance){
  Y <- X[1:(length(X)-1)]
  Ik <- nrow(Y)
  b_vec <- apply(Y, 1, function(x) (dist(rbind(x,x1))))
  b <- sum(b_vec)/Ik
} 

x <- c(1,2,1,2,7,7)
y <- c(0,1,0,3,0,1)

test <- data.frame(x, y)
print(test)
cluster_test <- c("jaune","jaune","rouge","rouge","noir","noir")

silhouette(test,cluster_test,"1")
#print(test)

value <- distance_moyenne_a(c(1,0),test,1)
value2 <- distance_moyenne_a(c(2,1),test,1)
value3 <- distance_moyenne_a(c(1,0),test,1)
value4 <- distance_moyenne_a(c(2,3),test,1)
print(value)
print(value2)
print(value3)
print(value4)
value+value2+value3+value4
#dist(rbind(x,x))
#dist(rbind(x,y))

  
