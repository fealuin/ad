library(aod)
library(ggplot2)
library(caret)
library(survey)
library(lmtest)
library(pROC)
library(precrec)

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
## Preproceso

# Separar vino en 2 clases 
wineWhite$quality<-factor(reClass(wineWhite$quality))
# Estandarización
wineWhite.std<-as.data.frame(cbind(scale(wineWhite[,-ncol(wineWhite)]),wineWhite$quality))

names(wineWhite.std)<-names(wineWhite)
wineWhite.std$quality<-factor(wineWhite.std$quality)

#ejemplo de https://stats.idre.ucla.edu/r/dae/logit-regression/

reg<-glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+ pH+sulphates+alcohol,data=wineWhite.std,family = 'binomial')


#Significancia
with(reg, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Odds Ratio
exp(coef(reg))

#Coefficients
reg$coefficients

#Predict
plot()
predict(reg,newdata=wineWhite.std)
#Significancia
logLik(reg)

#Importancia de variables
varImp(reg)



autoplot(evalmod(scores=f1))

# ROC
prob<-predict(reg,type=c('response'))
f1 = roc(quality ~ prob, data=wineWhite.std) 
plot(f1, col="red")


# K Fold
set.seed(1)
Train <- createDataPartition(wineWhite$quality, p=0.6, list=FALSE)
training <- wineWhite.std[ Train, ]
testing <- wineWhite.std[ -Train, ]

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+ pH+sulphates+alcohol,  data=training, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, testing$quality)







