library(readxl)
# importar el dataset
#establecer escritorio de trabajo
setwd("C:/Users/Walter/Desktop/MODELOS ESTADISTICOS/redes neuronales")


dataset = read_excel("CAP 400A - SALUD Y NUTRICION ADULTO.xlsx",sheet="CAP 400A")
str(dataset)
head(dataset)
summary(dataset)

## analizar el comportamiento de las variables
library(VIM)
aggr(dataset,
     col= c('green', 'red'), #colores del grafico
     numbers = TRUE,  #indicador de proporciones mostradas por numeros
     sortvars = TRUE, #ordena variables por apariencia de NAs de mas a menos
     labels = names(dataset), #pone las etiquetas de acuerdo al nombre de las columnas
     cex.axis = 0.75, #ancho de barras
     gap = 1, #distancia entre graficos
     ylab = c("histograma de NAs", "Patron")) #titulo de las variables

#convertir variables categoricas a factores
library(dplyr)
dataset <- dataset %>%
  mutate(
     PESO_BRUTO_ENTERO  = as.factor(PESO_BRUTO_ENTERO),
     TALLA_ENTERO = as.factor(TALLA_ENTERO),
     RESULTADO_FINAL_SISTOLICA = as.factor(RESULTADO_FINAL_SISTOLICA),
     RESULTADO_FINAL_DIASTOLICA = as.factor(RESULTADO_FINAL_DIASTOLICA),
     RESULTADO_HEMOGLOBINA_ENTERO = as.factor(RESULTADO_HEMOGLOBINA_ENTERO),
     SEXO = as.factor(SEXO)
  )
str(dataset)


dataset <- dataset[, c(11,21,54,76,77,81)] #Seleccionamos directamente por nombre de columna 

##analizar existencia de NAs con paquete mice
library(mice)
md.pattern(dataset, rotate.names = TRUE)

#Imputar datos faltantes (solo con 5 iteraciones)
dataset <-mice(dataset, m=5, maxit=5, meth="pmm", seed=500)
summary(dataset)
dataset <- complete(dataset,1)
md.pattern(dataset, rotate.names = TRUE)

str(dataset)

################ GRAFICOS MULTIPLES ####
#### grafico multiple (univariados, bivariados y correlaciones a la vez)
library(psych)
pairs.panels(dataset[,2:6], #dibujar las Xs
             gap=0,
             bg = c("red","yellow")[dataset$SEXO],
             pch=21)
#el grafico anterior muestra:
# la diagonal principal muestra el grafico para cada variable
#debajo de la diagonal, muestra graficos bivriados para cada par de variable
#encima de la diagonla, las correlaciones para cada par de variable


#### Graficos con Gpplot2####
##scater plot de peso y talla con puntos de las clases de y
#grafico bivariado para dos variables numericas con nube de puntos en el plano
library(ggplot2)
ggplot(dataset,aes(x=PESO_BRUTO_ENTERO, y=TALLA_ENTERO, color=SEXO)) +
     geom_point() + geom_rug()+
     ggtitle("Scatterplot de Peso y Talla")

## histograma
ggplot(data = dataset, mapping = aes(x = TALLA_ENTERO, fill = SEXO)) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.8) +
  ggtitle("histogramas de talla para las clases de sexo")

################
#convertir variables factores a numericas
library(dplyr)
dataset <- dataset %>%
  mutate(
    PESO_BRUTO_ENTERO  = as.numeric(PESO_BRUTO_ENTERO),
    TALLA_ENTERO = as.numeric(TALLA_ENTERO),
    RESULTADO_FINAL_SISTOLICA = as.numeric(RESULTADO_FINAL_SISTOLICA),
    RESULTADO_FINAL_DIASTOLICA = as.numeric(RESULTADO_FINAL_DIASTOLICA),
    RESULTADO_HEMOGLOBINA_ENTERO = as.numeric(RESULTADO_HEMOGLOBINA_ENTERO)
  )
str(dataset)
## Escalar variables numericas
library(dplyr)
dataset2 = mutate_if(dataset, is.numeric, scale)
class(dataset2)
str(dataset2)
str(dataset)

# dividir los datos en conjunto de entrenamiento y conjunto de test
library(caTools)
set.seed(123)
split = sample.split(dataset2, SplitRatio = 0.80)
train = subset(dataset2, split ==TRUE)
test = subset(dataset2, split == FALSE)

#data para crear el modelo
table(train$SEXO)
prop.table(table(train$SEXO))
summary(train)

##guardar en forma csv la data procesada, el training y test
write.table(dataset2, file = "Data_Procesada.csv",
            sep=",", row.names = FALSE)#exportamos la tabla para futuros analisis
write.table(test, file = "Data_Testing.csv",
            sep=",", row.names = FALSE)#exportamos la tabla para futuros analisis
write.table(train, file = "Data_Training.csv",
            sep=",", row.names = FALSE)#exportamos la tabla para futuros analisis


###### Crear los modelos de clasificacion con el conjunto de entrenamiento #####

#cuando sean demasiados casos y demore en compilarse los modelos, se creara una
#muestra aleatoria con por ejemplo 3000 casos, pero no sera cualquier muestra, sera una que aplica
#la tecnica de both sampling, que es la que permitira romper con el desbalance de clase
#haciendo que haya aproximadamente igual cantidad de casos para ambas clases
library(ROSE)
both <-ovun.sample(y~.,data=train, method="both",
                   p=0.5,
                   seed=222,
                   N=3000)$data

#creando modelo con Random Forest
library(randomForest)
rf <-randomForest(SEXO~.,
                  data=train,
                  ntree = 200,
                  mtry = 8,
                  importance = TRUE,
                  proximity = TRUE)
print(rf)
plot(rf)

#creando el modelo de Support vector machine
library(e1071)
SVM <- svm(formula = SEXO~.,
           data = train,
           type = "C-classification",
           kernel = "poly",
           cost = 10)
print(SVM)

#creando el modelo con naive bayes
library(naivebayes)
NB =naiveBayes(formula = SEXO ~.,
               data= train)

# creando el modelo de red neuronal artificial basica
library(caret)
RN = caret::train(SEXO~.,    #una formula del tipo y~x1+x2+..
                  train,  #dataset donde vas a entrenar el modelo
                  method = "nnet",  #tipo de modelo a usar
                  trace= FALSE,
                  )

#creando el modelo de regresion logistica
RL = glm(formula = SEXO~.,
         data = train,
         family = binomial)
prob_pred = predict(RL, type = "response",
                    newdata = test[,-94])
y_pred = ifelse(prob_pred>0.5,1,2) # vector con clases presichas

#creando el modelo de arbol de decision
library(rpart)
AD = rpart(formula = SEXO ~.,
           data = train,
           minsplit = 100, #numero minimo de observaciones
           minbucket = 10) #numero minimo de observaciones
y_pred2 = predict(AD, newdata = test[,-94],
                  type = "class")

#construir matriz de confusion para todos los modelos
#quien tiene el accuracy mas alto
#sensitivity cantidad de casos correctamente predichos para la clase de interes

library(caret)
library(e1071)
confusionMatrix(predict(rf, test), test$SEXO, positive = '1')
confusionMatrix(predict(SVM, test), test$SEXO, positive = '1')
confusionMatrix(predict(NB, test), test$SEXO, positive = '1')
confusionMatrix(predict(RN, test), test$SEXO, positive = '1')
confusionMatrix(as.factor(y_pred), test$SEXO, positive = '1')
confusionMatrix(as.factor(y_pred2), test$SEXO, positive = '1')

#determinar la importancia de las variables en el modelo
varImp(RN)
plot(varImp(RN))
