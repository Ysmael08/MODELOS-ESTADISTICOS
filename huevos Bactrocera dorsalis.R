install.packages("devRate")
library("devRate")
### script para analizar los datos de tasa de desarrollo de B. dorsalis en 
### función de la temperatura.
require("devRate") # para cargar el paquete devRate
c(55.0, 56.0, 57.0, 58.0, 60.0, 62.5, 65.0, 67.5, 70.0, 75.0, 80.0, 85.0, 87.5, 90.0, 92.5, 95.0, 96.0, 97.0, 97.5)
# las temperaturas estan en Fahrenheit, tenemos que convertirlas en Celsius con la formula T(°C) = (T(°F) - 32) / 1.8.
(c(55.0, 56.0, 57.0, 58.0, 60.0, 62.5, 65.0, 67.5, 70.0, 75.0, 80.0, 85.0, 87.5, 
   90.0, 92.5, 95.0, 96.0, 97.0, 97.5) - 32) / 1.8
#Si queremos guardar los valores de temperatura en un objeto llamado temp

temp <- (c(55.0, 56.0, 57.0, 58.0, 60.0, 62.5, 65.0, 67.5, 70.0, 75.0, 80.0, 
           85.0, 87.5, 90.0, 92.5, 95.0, 96.0, 97.0, 97.5) - 32) / 1.8
#La tasa de desarrollo corresponde al opuesto del numero de días. Aquí los valores estan expresados en horas, 
#entonces dividimos el numero de horas por 24 y despues calculamos el opuesto.

devRate <- 1/(c(263.0, 232.0, 170.5, 148.0, 121.3, 95.5, 74.0, 62.5, 51.5, 
                38.0, 30.5, 27.0, 25.0, 24.0, 23.5, 25.0, 26.5, 29.3, 34.3)/24)
#almacena los datos en un dataframe
datosLab <- data.frame(temp, devRate)
#Si queremos hacer la tabla y llenarla, hay que especificar el nombre que queremos dar a las 
#columnas con el simbolo = :

datosLab <- data.frame(
  temp = (c(55.0, 56.0, 57.0, 58.0, 60.0, 62.5, 65.0, 67.5, 70.0, 75.0, 80.0, 
            85.0, 87.5, 90.0, 92.5, 95.0, 96.0, 97.0, 97.5) - 32)/1.8, 
  devRate = 1/(c(263.0, 232.0, 170.5, 148.0, 121.3, 95.5, 74.0, 62.5, 51.5, 
                 38.0, 30.5, 27.0, 25.0, 24.0, 23.5, 25.0, 26.5, 29.3, 34.3)/24))
#Podemos verificar el contenido del objeto 

print(datosLab)

#seleccionar un modelo
#Para seleccionar los datos que vamos a usar, puede ser util visualizar los datos de laboratorio en un grafico.

plot(x = datosLab$temp, y = datosLab$devRate, 
     xlab = "Temperatura (°C)", ylab = "Tasa de desarrollo", 
     xlim = c(0, 40), ylim = c(0, 1.2))

#eliminar datos para la linearilidad
datosLab14 <- datosLab[1:14, ]
plot(x = datosLab14$temp, y = datosLab14$devRate, 
     xlab = "Temperatura (°C)", ylab = "Tasa de desarrollo", 
     xlim = c(0, 40), ylim = c(0, 1.2))

#ajustar al modelo lineal
modLin <- devRateModel(eq = campbell_74, dfData = datosLab14)
plot(x = datosLab$temp, y = datosLab$devRate, 
     xlab = "Temperatura (°C)", ylab = "Tasa de desarrollo", 
     xlim = c(0, 40), ylim = c(0, 1.2)) # datos del laboratorio
abline(modLin) # añadir modelo lineal en el grafico
print(modLin) # imprimir resultados del ajuste del modelo

#explorar otros modelos no lineales
names(devRateEqList)

briere1_99$startVal

devRateFind(species = "Bactrocera dorsalis")

#modelo no lineal 1
modNoLin_01 <- devRateModel(
  eq = briere1_99, # nombre del modelo
  dfData = datosLab, # nombre de los datos de laboratorio
  startValues = list(aa = 0.01, Tmin = 10, Tmax = 40)) # valores iniciales
print(modNoLin_01)

# modelo no lineal 2
modNoLin_02 <- devRateModel(
  eq = briere2_99, # nombre del modelo
  dfData = datosLab, # nombre de los datos de laboratorio
  startValues = list(
    aa = 0.01, Tmin = 10, Tmax = 40, bb = 2)) # valores iniciales
print(modNoLin_02)

# modelo no lineal 3
modNoLin_03 <- devRateModel(
  eq = lactin2_95, # nombre del modelo
  dfData = datosLab, # nombre de los datos de laboratorio
  startValues = list(
    aa = 0.03, Tmax = 30, deltaT = 5.0, bb = -1.5)) # valores iniciales
print(modNoLin_03)

# visualizacion de las graficas
par(mfrow = c(1, 3)) # para hacer tres graficos en la misma pagina
devRatePlot(eq = briere1_99, 
            nlsDR = modNoLin_01, 
            xlim = c(10, 40), ylim = c(0, 1.2))
devRatePlot(eq = briere2_99, 
            nlsDR = modNoLin_02, 
            xlim = c(10, 40), ylim = c(0, 1.2))
devRatePlot(eq = lactin2_95, 
            nlsDR = modNoLin_03, 
            xlim = c(10, 40), ylim = c(0, 1.2))

# El modelo con menor AIC es el que se ajusta mejor a los datos. 

c(AIC(modNoLin_01), AIC(modNoLin_02), AIC(modNoLin_03))

tempS <- seq(from = 0, to = 45, by = 0.1) # temperaturas simuladas
devRateS <- predict(modNoLin_02, newdata = list(T = tempS)) # predicciones
devRateS[devRateS < 0] <- 0
devRateS[is.na(devRateS)] <- 0
c(AIC(modNoLin_01), AIC(modNoLin_02), AIC(modNoLin_03))

#En este caso, el modelo de Briere-2 se ajusta mejor a los datos que los otros modelos. 
#Ahora podemos calcular los valores de CTmin, CTmax, y Topt.
Topt <- tempS[devRateS == max(devRateS)]
CTmin <- tempS[devRateS == min(devRateS[devRateS > 0 & 
                                          tempS < Topt])]
CTmax <- tempS[devRateS == min(devRateS[devRateS > 0 & 
                                          tempS > Topt])]
cat(paste0("Topt: ", Topt, "\nCTmin: ", CTmin, "\nTmax: ", CTmax))