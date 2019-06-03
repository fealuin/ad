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
library(gridExtra)
library(spatstat)
library(ggfortify)

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

#distancia euclidiana
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

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
fviz_nbclust(wineWhite.std, kmeans, method = 'silhouette',k.max=20,print.summary = TRUE)

# Cálculo de distancia
wineWhite.dist<-Dist(wineWhite.std,method=distance)

# Cálculos de clúster
wineWhite.clustering<-eclust(wineWhite.std,FUNcluster = clustFUN,hc_metric=distance,graph = FALSE,k=k)

# Plots clustering
fviz_cluster(wineWhite.clustering,geom='point',show.clust.cent = TRUE,ggtheme = theme_minimal(),ellipse.type = 'norm')
fviz_silhouette(wineWhite.clustering)

## Métricas de calidad

# Matriz de confusión
grid.table(table(wineWhite$quality,wineWhite.clustering$cluster),rows=c(1,2))

# Internas

# Conectividad
wineWhite.connectivity<-connectivity(wineWhite.dist,clusters=wineWhite.clustering$cluster,method=distance, neighbSize=10)

# Índice Dunn
wineWhite.dunn<-dunn(wineWhite.dist,clusters=wineWhite.clustering$cluster,method=distance)

# Índice siulueta 

wineWhite.silhouette<-summary(silhouette(wineWhite.clustering$cluster,dist=wineWhite.dist))

# Externas
  
# jaccard
# estructura de cluster para cluster inicial
clusterPropuesto <- wineWhite$quality
names(clusterPropuesto) <- rownames(wineWhite)
jaccard=round(cluster_similarity(wineWhite.clustering$cluster,clusterPropuesto, similarity = 'jaccard', method = "independence"),6)
print(jaccard)


#Otras
wineWhite.clustering.stats<-cluster.stats(wineWhite.dist,as.vector(wineWhite.clustering$cluster),wineWhite.q)


#Centros kmeans
grid.table(t(format(wineWhite.clustering$centers,digits = 1)))

# Centroides

ncentroides<-1

# cluster 1
cl1.nbs <- head(order(apply(wineWhite.std,1, function(x) euc.dist(as.numeric(x),wineWhite.clustering$centers[1,]))),ncentroides)

# cluster 2
cl2.nbs <- head(order(apply(wineWhite.std,1, function(x) euc.dist(as.numeric(x),wineWhite.clustering$centers[2,]))),ncentroides)

c1<-cbind(cluster=1,wineWhite[cl1.nbs,])
c2<-cbind(cluster=2,wineWhite[cl2.nbs,])

c1.std<-cbind(cluster=1,wineWhite.std[cl1.nbs,],quality=wineWhite$quality[cl1.nbs])
c2.std<-cbind(cluster=2,wineWhite.std[cl2.nbs,],quality=wineWhite$quality[cl2.nbs])

write.csv(rbind(c1.std,c2.std),file='centroides.txt')
