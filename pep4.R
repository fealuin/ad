library(farff)
library(cluster)
library(plotly)
library(arules)
library(arulesViz)
library(aod)
library(ggplot2)
library(caret)
library(survey)
library(pROC)
library(lmtest)
library(precrec)
library(randomForest)
library(InformationValue)
library(pROC)
library( mboost)

data=readARFF("./datasets/credit_fruad.arff")
#data.numeric=read.fwf('./datasets/german.data-numeric',widths = rep(4,25))
#data.numeric=read.csv('./datasets/proc_german_num_02 withheader-2.tab.tsv',sep = '\t')


data$class<-as.factor(as.numeric(data$class)-1)
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

data.dis<-as.data.frame(apply(data,2,function(x) make.names(gsub("[[:punct:]]", " ", x)) ))



data.dis[,toDis] <-discretizeDF(data[,toDis])
data.dis[,toFactor]<-data.frame(as.factor(data$location),as.factor(data$residence_since),as.factor(data$existing_credits),as.factor(data$num_dependents))

data.dis<-as.data.frame(apply(data.dis,2,function(x) make.names(gsub("[[:punct:]]", " ", x)) ))

set.seed(1)

# Se separa conjunto de datos de entrenamiento y de prueba
Train <- createDataPartition(data.dis$class, p=0.6, list=FALSE)
training <- data.dis[ Train, ]
testing <- data.dis[ -Train, ]

# Se aplica regresión logística al conjunto de datos

reg<-glm(class~.,data=training,family = binomial)


backwards <-step(reg,trace = 0)

formula(backwards)
nothing <- glm(class ~ 1,data=training,family=binomial)
forwards = step(nothing,scope=list(lower=formula(nothing),upper=formula(reg)), direction="both",trace=0)
reg<-forwards
#reg<-backwards
# Significancia
with(reg, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# Odds Ratio
exp(coef(reg))

# Coefficients
summary(reg)

# Significancia
logLik(reg)

# Importancia de variables
varImp(reg)

## Validación de resultados

# ANOVA / Chi cuadrado

anova(reg,test='Chisq')

# ROC
prob<-predict(reg,type=c('response'))
f1 <- roc(class ~ prob, data=training,smoothed = TRUE,plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, show.thres=TRUE) 
plot(f1, col="red")


precrec_obj<-evalmod(scores = prob, labels = training$class)
autoplot(precrec_obj)

# p-value modelo
lrtest(reg)


# Se predice la calidad del set de pruebas
pred = predict(reg, newdata=testing,type='response')


optCutOff <- optimalCutoff(testing$class, pred)[1]
pred.num<-as.factor(make.names(as.numeric(pred>optCutOff)))

# Se evalúa el resultado del clasificador
caret::confusionMatrix(data=pred.num, as.factor(testing$class))


plotROC(testing$class, as.numeric(pred.num))

## Validación cruzada K Fold 

# Se define el numero de folds
ctrl <- trainControl(method = "repeatedcv", savePredictions = TRUE,number=10,classProbs=T)

# Se entrena el modelo de regresión
reg <- train(class ~ over_draft + credit_usage + credit_history + Average_Credit_Balance + 
                   purpose + property_magnitude + residence_since + foreign_worker + 
                   personal_status,  data=data.dis, method="glm",family=binomial, trControl = ctrl)
# Se predice la calidad del set de pruebas
pred = predict(reg, newdata=testing)
#pred.num<-as.factor(make.names(as.numeric(pred>optCutOff)))
# Se evalúa el resultado del clasificador
caret::confusionMatrix(data=pred,testing$class)

pred.roc<-pROC::roc(as.numeric(testing$class), as.numeric(pred))
pred.auc<-pROC::auc(pred.roc)


plot.roc(pred.roc,print.auc = T)


fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)


glmBoostModel <- train(class ~ ., data=training, method = "glmboost", metric="ROC", trControl = fitControl, tuneLength=5, center=TRUE, family=binomial(link = c("logit")))




#test random forest
rf<-randomForest(class~.,data=data.dis)
rf$importance[order(rf$importance[,1]),]
