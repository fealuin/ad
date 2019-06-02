library(plotly)
library(cluster)
library(amap)
library(factoextra)
library(fpc)
library(NbClust)
library(clValid)
library(optpart)
library(cluster) 
library(clusteval) 
library(clues) 


# Normaliza entre 0 y 1 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Reduce las clases a 2: calidad >=6 bueno <6 malo
reClass <- function(x) {
  
  return (sapply(x,function(x){
    if(x>=6){
      x=2
    } 
    else{
      x=1
    }
  }))
}

# Cantidad de clusters
k <- 2

# Distancia a utilizar
distance <- 'euclidean'

# Método de clustering
clustFUN <- 'kmeans'

# Lectura de archivos
wineWhite<-read.csv('./datasets/winequality-white.csv',sep=';')

## Preprocsamiento

# Reducción de clases 
wineWhite$quality<-reClass(wineWhite$quality)

# Se deja la calidad como factores
wineWhite.q<-as.numeric(as.factor(paste0('q' , as.character(wineWhite$quality))))

# Estandarización
wineWhite.std<-as.data.frame(scale(wineWhite[,-ncol(wineWhite)]))

# Normalizacion
wineWhite.norm<-as.data.frame(lapply(wineWhite[,-ncol(wineWhite)],normalize))


## Determinación de k en base a índice silueta medio

#kmeans
fviz_nbclust(wineWhite.norm, kmeans, method = "silhouette")

# Cálculo de distancia
wineWhite.dist<-Dist(wineWhite.norm,method=distance)

# Cálculos de clúster
wineWhite.clustering<-eclust(wineWhite.std,FUNcluster = clustFUN,hc_metric=distance,graph = FALSE,k=k)

# Plots clustering
fviz_cluster(wineWhite.clustering,geom='point',show.clust.cent = TRUE,ggtheme = theme_minimal(),ellipse.type = 'norm')
fviz_silhouette(wineWhite.clustering)

## Métricas de calidad

# Matriz de confusión
table(wineWhite$quality,wineWhite.clustering$cluster)

# Internas
wineWhite.clustering.stats<-cluster.stats(wineWhite.dist,wineWhite.q,as.vector(wineWhite.clustering$cluster))


# Externas
  
# jaccard
# estructura de cluster para cluster inicial
clusterPropuesto <- wineWhite$quality
names(clusterPropuesto) <- rownames(wineWhite)
jaccard=round(cluster_similarity(wineWhite.clustering$cluster,clusterPropuesto, similarity = 'jaccard', method = "independence"),6)
print(jaccard)

