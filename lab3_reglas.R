library(plotly)
library(arules)
library(arulesViz)


# Lectura de archivo

wineWhite<-read.csv('./datasets/winequality-white.csv',sep=';')

# Discretizar dataframe

wineWhite.discretized=discretizeDF(wineWhite,default = list(method='frequency',breaks=3))

#c('quality=[3,5)','quality=[5,6)','quality=[6,9]')

# Buscar reglas

rules<-apriori(wineWhite.discretized,parameter = list(support=0.01,confidence=0.5,minlen=1,maxlen=20),appearance=list(rhs=c('quality=[3,5)','quality=[5,6)','quality=[6,9]')))

# Inspeccionar reglas con mayor lift

inspect(head(rules,20,by='lift'))

# Gráfico

p<-plot(rules)


