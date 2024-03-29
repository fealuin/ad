library(plotly)
library(cluster)
library(amap)
library(factoextra)
library(fpc)
library(NbClust)
library(clValid)
library(optpart)

library("cluster") # Instalar
library('clusteval') # Instalar
library('clues') # install.packages("clues")

# Reducir clases
# Balancear clases
# Reducir variables

# Normaliza entre 0 y 1 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

reClass <- function(x) {
  
  return (sapply(x,function(x){
    if(x>6){
      x=3
    } 
    else if (x<5){
      x=1
    }
    else{
      x=2
    }
  }))
}

# cantidad de grupos
k = 3

# Lectura de archivos

wineRed<-read.csv('./datasets/winequality-red.csv',sep=';')
wineWhite<-read.csv('./datasets/winequality-white.csv',sep=';')

#Se reducen las clases a 3: malo (<5), promedio(5 o 6) y bueno (>6) 
wineRed$quality<-reClass(wineRed$quality)
wineWhite$quality<-reClass(wineWhite$quality)

#se elimina clase 2 para distingir solo entre bueno y malo

wineRed<-wineRed[-which(wineRed$quality==2),]
wineWhite<-wineWhite[-which(wineWhite$quality==2),]
# PAM Vino Rojo
#seleccion de caracteristicas cortez 2009

#wineRed=wineRed[,c('sulphates','pH','total.sulfur.dioxide','alcohol','quality')]

wineRed.q<-as.numeric(as.factor(paste0('q' , as.character(wineRed$quality))))
wineWhite.q<-as.numeric(as.factor(paste0('q' , as.character(wineWhite$quality))))



# Normalización
#wineRed.norm<-as.data.frame(lapply(wineRed[,-12],normalize))
# Estandarización
wineRed.norm<-as.data.frame(scale(wineRed[,-ncol(wineRed)]))
#Cálculo de distancia
wineRed.dist<-Dist(wineRed.norm,method='correlation')

wineRed.pam<-pamk(wineRed.norm,krange = 2:50)
# Cálculos de pam (eclust)
wineRed.pam<-eclust(wineRed.norm,FUNcluster = 'pam',hc_metric='euclidean',graph = FALSE,k=4)

fviz_nbclust(wineRed.norm,FUNcluster=clara,method='silhouette',k.max=10,diss = wineRed.dist,verbose = TRUE)

#optimizacion de silueta
wineRed.optsil<-optsil(wineRed.pam$clustering,wineRed.dist,100)
wineRed.pam.clValid<-clValid(wineRed.norm, 2:10,clMethods = c('kmeans','pam'),validation = 'stability',metric = 'correlation')

plot(wineRed.pam.clValid)

# Gráfico de clusters

fviz_cluster(wineRed.pam,geom='point',show.clust.cent = TRUE,ggtheme = theme_minimal(),ellipse.type = 'norm')
fviz_silhouette(wineRed.pam)

# Medidas de calidad
wineRed.pam.cluster.stats<-cluster.stats(wineRed.dist,wineRed.q,as.vector(wineRed.pam$clustering))

# Matriz de confusión
table(wineRed$quality,wineRed.pam$cluster)

## PAM Vino Blanco 

# Normalización
#wineWhite.norm<-as.data.frame(lapply(wineWhite[,-12],normalize))
# Estandarización
wineWhite.norm<-as.data.frame(scale(wineWhite[,-12]))
#Cálculo de distancia
wineWhite.dist<-Dist(wineWhite.norm,method='correlation')

# Cálculos de pam (eclust)
wineWhite.pam<-eclust(wineWhite.norm,FUNcluster = 'pam',k,hc_metric='euclidean',graph = FALSE)




# Gráfico de clusters
fviz_nbclust(wineWhite.norm,FUNcluster=kmeans,method='silhouette',k.max=10,diss = wineWhite.dist,verbose = TRUE)
fviz_cluster(wineWhite.pam,geom='point',show.clust.cent = TRUE,ggtheme = theme_minimal(),ellipse.type = 'norm')
fviz_silhouette(wineWhite.pam)

# Medidas de calidad
wineWhite.pam.cluster.stats<-cluster.stats(wineWhite.dist,wineWhite.q,as.vector(wineWhite.pam$clustering))

# Matriz de confusión
table(wineWhite$quality,wineWhite.pam$clustering)

# jaccard
# estructura de cluster para cluster inicial
clusterPropuesto <- wineRed$quality
names(clusterPropuesto) <- rownames(wineRed)

jaccard=round(cluster_similarity(wineRed.pam$clustering,clusterPropuesto, similarity = 'jaccard', method = "independence"),6)
print(jaccard)


