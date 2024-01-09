## clasificacion para una variable dicotomica####

library(caret)
library(dplyr)
library(readxl)
# preprocesamiento

#establecer escritorio de trabajo
setwd("C:/Users/Walter/Desktop/MODELOS ESTADISTICOS/redes neuronales")


# importar el dataset
 
entrenamiento = read_excel("helicoverpa.xlsx",sheet="datos")
str(entrenamiento)
head(entrenamiento) 

#dejamos los datos de columnas que nos importan
entrenamiento <- entrenamiento[2:5]
entrenamiento

# Eliminar filas con valores faltantes en el dataframe 'entrenamiento'
entrenamiento <- na.omit(entrenamiento)
entrenamiento

# Eliminar filas con 'Hybrid' en cualquier columna
entrenamiento <- entrenamiento[!apply(entrenamiento, 1, function(row) any(row == "Hybrid")), ]
entrenamiento

# Cambiar el nombre de la columna "....\r\...." a "....."
names(entrenamiento)[names(entrenamiento) == "Specimen\r\ndescription"] <- "Especie"
names(entrenamiento)[names(entrenamiento) == "Number\r\nof lobes"] <- "Numero_lobulos"
names(entrenamiento)[names(entrenamiento) == "Number\r\nof\r\ncornuti"] <- "Numero_cornuti"
names(entrenamiento)[names(entrenamiento) == "Length\r\nof valves\r\n(mm)"] <- "Longitud_valva"
head(entrenamiento)# Mostrar las primeras filas del DataFrame con el nuevo nombre de columna

#analizando el comportamiento de los NAs en las variables
install.packages("VIM")
library(VIM)
aggr(entrenamiento,
     col= c('green', 'red'), #colores del grafico
     numbers = TRUE,  #indicador de proporciones mostradas por numeros
     sortvars = TRUE, #ordena variables por apariencia de NAs de mas a menos
     labels = names(entrenamiento), #pone las etiquetas de acuerdo al nombre de las columnas
     cex.axis = 0.75, #ancho de barras
     gap = 1, #distancia entre graficos
     ylab = c("histograma de NAs", "Patron")) #titulo de las variables
    
#pasar a factor especie
entrenamiento$Especie = factor(entrenamiento$Especie,
                               levels = c("H. armigera", "H. zea"),
                               labels = c(0,1))
str(entrenamiento) #comprobar la conversion

#generar conjunto de trainig y testing
set.seed(42)
particion = createDataPartition(entrenamiento$Especie, p=0.75, list = FALSE)#75% para entrenar~25% para testear

training = entrenamiento[ particion, ] #75% para training
testing = entrenamiento[ -particion, ] #25% para testing

#escalado de variables numericas
#scale (variables a normalizar del dataset)
#acotando todas las variables en media 0 sd 1
training[,2:4] = scale(training[,2:4]) #escalado de conjunto de training
testing[,2:4] = scale(testing[,2:4]) #escalado de conjunto de testing

#implementar K Fold cross-validation
#del conjunto de trainig se coge una submuestra
#y se rota en los datis hasta completar el 100
#y hace un promedio de las rotaciones
#el objetivo es hacer mas significativa 
#la prediccion
fitControl = trainControl(method = "CV",
                          number = 10,
                          preProcOptions = list(thresh = 0.95), #umbral de proceso
                          summaryFunction = twoClassSummary)

####### Implementacion del modelo ######

modelNnet = caret::train(Especie~., #una formula del tipo y ~x1+x2+...
                         training,  #dataset donde vas a entrenar el modelo
                         method="nnet", #tipo del modelo a usar
                         trace=FALSE,
                         )
modelNnet
summary(modelNnet)
plot(modelNnet)

#generar las predicciones con el conjunto de testing
y_pred= predict(modelNnet, testing)
cnNnet = confusionMatrix(y_pred, as.factor(testing$Especie))
cnNnet

#determinar la importancia de las variables en el modelo
varImp(modelNnet)
plot(varImp(modelNnet))

