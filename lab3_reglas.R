library(plotly)
library(arules)
library(arulesViz)


# Lectura de archivo

wineWhite<-read.csv('./datasets/winequality-white.csv',sep=';')

# Discretizar dataframe

wineWhite.discretized=discretizeDF(wineWhite,default = list(method='frequency',breaks=3))

# Buscar reglas 

rules<-apriori(wineWhite.discretized,parameter = list(support=0.01,confidence=0.01,minlen=2),appearance=list(rhs=c('quality=[3,5)')))

rules<-apriori(wineWhite.discretized,parameter = list(support=0.01,confidence=0.8,minlen=2),appearance=list(rhs=c('quality=[5,6)')))

rules<-apriori(wineWhite.discretized,parameter = list(support=0.1,confidence=0.8,minlen=2),appearance=list(rhs=c('quality=[6,9]')))

# Inspeccionar reglas con mayor lift

inspect(head(rules,10,by='lift'))

# Gráficos
p<-plot(rules)
p<-plot(head(rules,10,by='lift'),method='paracoord',reorder=T)

#p<-plot(head(rules,1,by='lift'),method='doubledecker',data=rules)
#p<-plot(head(rules,10,by='lift'),method='graph')
#p<-plot(head(rules,10,by='lift'),method='grouped')


