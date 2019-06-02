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


# Normaliza entre 0 y 1 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

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

# cantidad de grupos
k = 2

# Lectura de archivos
wineWhite<-read.csv('./datasets/winequality-white.csv',sep=';')

#Se reducen las clases a 4: < 4, < 5 y >= 4,  < 6 y >= 5, > 6 
wineWhite$quality<-reClass(wineWhite$quality)

wineWhite.q<-as.numeric(as.factor(paste0('q' , as.character(wineWhite$quality))))

# Estandarización
wineWhite.norm<-as.data.frame(scale(wineWhite[,-ncol(wineWhite)]))
#Cálculo de distancia
wineWhite.dist<-Dist(wineWhite.norm,method='euclidean')

# wineWhite.pam<-pamk(wineWhite.norm,krange = 2:7)
# Cálculos de pam (eclust)
wineWhite.pam<-eclust(wineWhite.norm,FUNcluster = 'pam',hc_metric='euclidean',graph = FALSE,k)

# optimizacion de silueta
# wineWhite.optsil<-optsil(wineWhite.pam$clustering,wineWhite.dist,100)

# fviz_cluster(wineWhite.pam,geom='point',show.clust.cent = TRUE,ggtheme = theme_minimal(),ellipse.type = 'norm')
# fviz_silhouette(wineWhite.pam)

# Medidas de calidad
wineWhite.pam.cluster.stats<-cluster.stats(wineWhite.dist,wineWhite.q,as.vector(wineWhite.pam$clustering))

# Matriz de confusión
table(wineWhite$quality,wineWhite.pam$clustering)

# jaccard
# estructura de cluster para cluster inicial
clusterPropuesto <- wineWhite$quality
names(clusterPropuesto) <- rownames(wineWhite)

jaccard=round(cluster_similarity(wineWhite.pam$clustering,clusterPropuesto, similarity = 'jaccard', method = "independence"),6)
print(jaccard)




# Cálculos de pam (eclust)
# wineWhite.metodo<-eclust(wineWhite.norm,FUNcluster = 'pam',hc_metric='euclidean',graph = FALSE,k)
# set.seed(123)
# km.res <- kmeans(df, k, nstart = 25)
wineWhite.kmeans = kmeans(as.dist(wineWhite.dist), k)

# Average silhouette for kmeans
fviz_nbclust(wineWhite.norm, pam, method = "silhouette")

# optimizacion de silueta
# wineWhite.optsil<-optsil(wineWhite.pam$clustering,wineWhite.dist,100)

# fviz_cluster(wineWhite.kmeans,geom='point',show.clust.cent = TRUE,ggtheme = theme_minimal(),ellipse.type = 'norm')
# fviz_silhouette(wineWhite.kmeans)

# Medidas de calidad
wineWhite.kmeans.cluster.stats<-cluster.stats(wineWhite.dist,wineWhite.q,as.vector(wineWhite.kmeans$cluster))

# Matriz de confusión
table(wineWhite$quality,wineWhite.kmeans$cluster)

# jaccard
# estructura de cluster para cluster inicial
clusterPropuesto <- wineWhite$quality
names(clusterPropuesto) <- rownames(wineWhite)

jaccard=round(cluster_similarity(wineWhite.kmeans$cluster,clusterPropuesto, similarity = 'jaccard', method = "independence"),6)
print(jaccard)
