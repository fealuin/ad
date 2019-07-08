library(farff)
library(cluster)
library(plotly)
library(arules)
library(arulesViz)




data=readARFF("./datasets/credit_fruad.arff")
#data.numeric=read.fwf('./datasets/german.data-numeric',widths = rep(4,25))
#data.numeric=read.csv('./datasets/proc_german_num_02 withheader-2.tab.tsv',sep = '\t')

D<-daisy(data.dis[,c(1,2,3)],metric='gower')


mds<-as.data.frame(cmdscale(D))
names(mds)<-c('x','y')
plot_ly(data=mds,x=~x,y=~y,type = 'scatter',color=data[,21])

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


# numeric

data.numeric<-as.data.frame(lapply(data, as.numeric))

# correlation

data.cor<-as.data.frame(cor(data.numeric))[,-21]
class.cor<-as.data.frame(t(data.cor['class',]),stringsAsFactors = F)
class.cor$names<-factor(row.names(class.cor),levels = row.names(class.cor)[order(class.cor$class)])

plot_ly(class.cor,x=~class,y=~names,type='bar',orientation='h') %>%
  layout(
    font=list(size=20),
    yaxis=list(
      title='Variables'
      ),
    xaxis=list(
      title='Correlación con riesgo crediticio'
    ))

# variance

data.var<-as.data.frame(lapply(data.numeric, var))

# rules

rules<-apriori(data.dis,parameter = list(support=0.02,confidence=0.8,minlen=2),appearance=list(rhs=c('class=bad')))

inspect(head(rules,10,by='lift'))

p<-plot(rules)
p<-plot(head(rules,10,by='lift'),method='paracoord',reorder=T)

data.kruskal=as.data.frame(lapply(data.numeric,function(x) kruskal.test(x~class,data=data.numeric)))#$p.value))

