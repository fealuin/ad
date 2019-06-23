library(aod)
library(ggplot2)

wineWhite<-read.csv('./datasets/winequality-white.csv',sep=';')
reClass <- function(x) {
  
  return (sapply(x,function(x){
    if(x>=6){
      x=1
    } 
    else{
      x=0
    }
  }))
}
wineWhite$quality<-reClass(wineWhite$quality)

#ejemplo de https://stats.idre.ucla.edu/r/dae/logit-regression/

reg<-glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+ pH+sulphates+alcohol,data=wineWhite,family = 'binomial')


#Significancia
with(reg, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Odds Ratio
exp(coef(reg))


logLik(reg)
