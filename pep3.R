library(farff)
library(cluster)
library(plotly)
library(arules)
library(arulesViz)


data=readARFF("./datasets/credit_fruad.arff")
#data.numeric=read.fwf('./datasets/german.data-numeric',widths = rep(4,25))
data.numeric=read.csv('./datasets/proc_german_num_02 withheader-2.tab.tsv',sep = '\t')

D<-daisy(data[,-21],metric='gower')
mds<-as.data.frame(cmdscale(D))
names(mds)<-c('x','y')
plot_ly(data=mds,x=~x,y=~y,type = 'scatter',color=data[,21])

discretizeDF(data,breaks=10,methods='frecuency')

rules<-apriori(data,parameter = list(support=0.01,confidence=0.01,minlen=2),appearance=list(rhs=c('class=bad')))

data.pca <- prcomp(data[,-21], center = TRUE,scale. = TRUE)
