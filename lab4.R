library(aod)
library(ggplot2)
library(caret)
library(survey)
library(pROC)


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

# Se dejan los mismos nombres en el DF
names(wineWhite.std)<-names(wineWhite)

# Se agrega calidad como factor
wineWhite.std$quality<-factor(wineWhite.std$quality)

## Obtención de resultados

# Se aplica regresión logística al conjunto de datos

reg<-glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+ pH+sulphates+alcohol,data=wineWhite.std,family = 'binomial')

# Significancia
with(reg, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# Odds Ratio
exp(coef(reg))

# Coefficients
reg$coefficients

# Significancia
logLik(reg)

# Importancia de variables
varImp(reg)

## Validación de resultados

# ANOVA / Chi cuadrado

anova(reg,test='Chisq')

# ROC
prob<-predict(reg,type=c('response'))
f1 = roc(quality ~ prob, data=wineWhite.std) 
plot(f1, col="red")


# Validación cruzada K Fold 
set.seed(1)

# Se separa conjunto de datos de entrenamiento y de prueba
Train <- createDataPartition(wineWhite$quality, p=0.6, list=FALSE)
training <- wineWhite.std[ Train, ]
testing <- wineWhite.std[ -Train, ]

# Se define el numero de folds
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

# Se entrena el modelo de regresión
mod_fit <- train(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+ pH+sulphates+alcohol,  data=training, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
# Se predice la calidad del set de pruebas
pred = predict(mod_fit, newdata=testing)

# Se evalúa el resultado del clasificador
confusionMatrix(data=pred, testing$quality)







