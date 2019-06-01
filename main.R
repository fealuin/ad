library(plotly)
library(ggbiplot)
library(amap)
library(tidyr)
library(ggplot2)
library(mvnormtest)
library(corrplot)
library(randomForest)


wineRed<-read.csv('./datasets/winequality-red.csv',sep=';')
wineWhite<-read.csv('./datasets/winequality-white.csv',sep=';')



dataMDSRedWine<-as.data.frame(cbind(cmdscale(Dist(wineRed[,-12],method='correlation')),wineRed[,12],1:nrow(wineRed)))


names(dataMDSRedWine)<-c('x','y','quality','id')


p <- plot_ly(data = dataMDSRedWine, x = ~x, y = ~y,color= ~quality,type='scatter',mode='markers')
  layout(title='MDS distancia correlacional vino rojo')
p


dataMDSWhiteWine<-as.data.frame(cbind(cmdscale(Dist(wineWhite[,-12],method='correlation')),wineRed[,12],1:nrow(wineWhite)))

names(dataMDSWhiteWine)<-c('x','y','quality','id')
p <- plot_ly(data = dataMDSWhiteWine, x = ~x, y = ~y,color= ~quality,type='scatter',mode='markers')
p


p <- ggplot(wineRed, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

p<- plot_ly(data=as.data.frame(wineRed) type='box')
p



#PCA
wineRed.pca=prcomp(wineRed[,1:11],center=TRUE, scale. = TRUE)

ggplot2::autoplot(wineRed.pca,data=wineRed ,colour='quality',colours='quality',loadings=TRUE,loadings.label=TRUE)

#Correlación
autoplot(cor(wineRed))
corrplot(cor(wineRed),method = 'color',type='lower',addCoefasPercent = TRUE,diag=FALSE,tl.cex=0.5,tl.col='black',addCoef.col = 'black',cl.cex=0.5,number.cex = 0.5)
corrplot(cor(wineWhite),method = 'color',type='lower',addCoefasPercent = TRUE,diag=FALSE,tl.cex=0.5,tl.col='black',addCoef.col = 'black',cl.cex=0.5,number.cex = 0.5)

#Var
autoplot(var(wineRed))

#Covar


autoplot(summary(wineRed))


#Histogramas
ggplot(gather(wineRed), aes(value)) + 
      geom_histogram(bins = 10) + 
       facet_wrap(~key, scales = 'free_x')+
      ggtitle("Histograma variables vino rojo")+
  xlab("valores")+
  ylab("cuenta")

ggplot(gather(wineWhite), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+
  ggtitle("Histograma variables vino blanco")+
  xlab("valores")+
  ylab("cuenta")


#manova

  #test normality

mshapiro.test(t(as.matrix(wineRed)))

# Analisis RF
fit_rf = randomForest(quality~., data=wineRed)
# Importance based on mean decreasing gini
rfred=importance(fit_rf)
rfred=rfred[order(-rfred),]
rfredper=rfred/sum(rfred)
rfredper=as.data.frame(rfredper)
rfredper
ggplot(rfredper,aes(y=name,x=value))

# Analisis RF
fit_rf = randomForest(quality~., data=wineWhite)
# Importance based on mean decreasing gini
rfred=importance(fit_rf)
rfred=rfred[order(-rfred),]
rfredper=rfred/sum(rfred)
rfredper=as.data.frame(rfredper)
rfredper
