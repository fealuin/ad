library(farff)
library(cluster)
library(plotly)
library(arules)
library(arulesViz)


data=readARFF("./datasets/credit_fruad.arff")
#data.numeric=read.fwf('./datasets/german.data-numeric',widths = rep(4,25))
data.numeric=read.csv('./datasets/proc_german_num_02 withheader-2.tab.tsv',sep = '\t')

D<-daisy(data[,-21],metric='gower')


#mds<-as.data.frame(cmdscale(D))
#names(mds)<-c('x','y')
#plot_ly(data=mds,x=~x,y=~y,type = 'scatter',color=data[,21])

#discretizeDF(data.numeric)

#Discretize
toDis<-c(
  'current_balance',
  'credit_usage',
  'cc_age')

toFactor<-c(  
  'location',
  'residence_since',
  'existing_credits',
  'num_dependents'
  )

data.dis<-data



data.dis[,toDis] <-discretizeDF(data[,toDis])
data.dis[,toFactor]<-data.frame(as.factor(data$location),as.factor(data$residence_since),as.factor(data$existing_credits),as.factor(data$num_dependents))


rules<-apriori(data.dis,parameter = list(support=0.02,confidence=0.8,minlen=2),appearance=list(rhs=c('class=bad')))

inspect(head(rules,10,by='lift'))

p<-plot(rules)
p<-plot(head(rules,10,by='lift'),method='paracoord',reorder=T)



prueba1=kruskal.test( class~credit_usage*current_balance   , data = data)
pvalue = prueba1$p.value
print(pvalue)
