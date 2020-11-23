#Document realisant des analyses stat descriptive et de clustering :
library(ggplot2)
library("cowplot")
#install.packages("gridExtra")
library("gridExtra")

# #Statistiques univarie :
#Table et graphique univarie :
#Creation d'image pour chaque graphique

#Boucle qui parcours les colonnes du jeu de donnees et qui attribue pour chaque type de donnees le bon graphique
statunivarie<-function(df) {
  #Initialisation d'un ficher qui contiendra la liste de toutes les tables pour chaque variables
  tbl<-list(colnames(df))
  
  for (i in 1:length(df[1,])) {
    
  #Creation des tables d'effectifs et de frequence de chaque variables
    Total<-sum
    la<-cbind(addmargins(table(df[,i]), FUN = Total),addmargins(round(prop.table(table(df[,i]))*100 ,2)))
    colnames(la)=c("Effectifs","Pourcentage")
    
    #Si la variable a moins de 3 modalites :
    if (length(table(df[i]))<=3) {
      
      if (titre=="") {titre="Diagramme circulaire de la variable : "} 
      
      #Enregistrement du prochain graphique dans une image situÃ© dans le rÃ©pertoire courant
      jpeg(paste("Graph", colnames(df[i]), "jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)
        
        #Graph circulaire
        df[,i]<-factor(df[,i])
        g<-ggplot(df) +
          geom_bar(aes(x ="" , fill=df[,i]), position = "fill") +
          coord_polar(theta = "y") +
          labs(fill = colnames(df[i])) +
          ggtitle(titre,colnames(df[i])) +
          xlab("Fréquence") +
          ylab(colnames(df[i]))
        print(g)
        
      #fin d'enregistrement
      dev.off()
      
      #Sinon si la variable a moins de 10 modalites :
    } else if (length(table(df[,i]))<=10) {
      titre="Diagramme en barre de la variable : "
      
      #Enregistrement dans une image
      jpeg(paste("Graph", colnames(df[i]), "jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)
        
        #Graph en barre
        g<-ggplot(df) +
          aes(x =factor(df[,i]),fill=factor(df[,i])) +
          geom_bar() +
          geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(.5)) +
          ggtitle(titre,colnames(df[i])) +
          xlab(colnames(df[i])) +
          labs(fill = colnames(df[i])) +
          ylab("Effectifs")
        print(g)
        
      #fin d'enregistrement
      dev.off()
      
      #Sinon, si la variable est numÃ©rique :
      } else {
        if (is.numeric(df[,i])==T) {
          titre="Boxplot (boite à moustache) de la variable : "
          
          #Enregistrement d'image
          jpeg(paste("Graph", colnames(df[i]), "jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)
            
            #BoxPlot
            g<-ggplot(df,aes(x = colnames(df[i]), y = df[,i])) +
              geom_boxplot() +
              xlab("") +
              ylab("") +
              ggtitle(titre,colnames(df[i]))
            print(g)
            
          #fin d'enregistrement
          dev.off()
          
        }#fin de si 
      }#fin de else
  }#fin de for 
}#fin de function 

#Importation des donnees mtcars :
data("mtcars")
df<-mtcars

#appel de la fonction :
statunivarie(df)
