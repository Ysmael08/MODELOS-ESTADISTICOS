#construir modelos fenologicos
## cargar datos del laboratorio
datosLabTS_egg <- data.frame(
  temp = c(10.0, 10.0, 13.0, 15.0, 15.0, 15.5, 16.0, 16.0, 17.0, 20.0, 20.0, 
           25.0, 25.0, 30.0, 30.0, 35.0), 
  devRate = c(0.031, 0.039, 0.072, 0.047, 0.059, 0.066, 0.083, 0.1, 0.1, 0.1, 0.143, 
              0.171, 0.2, 0.2, 0.18, 0.001))
datosLabTS_larva <- data.frame(
  temp = c(10.0, 10.0, 10.0, 13.0, 15.0, 15.5, 15.5, 15.5, 17.0, 20.0, 25.0, 
           25.0, 30.0, 35.0), 
  devRate = c(0.01, 0.014, 0.019, 0.034, 0.024, 0.029, 0.034, 0.039, 0.067, 0.05, 
              0.076, 0.056, 0.0003, 0.0002))
datosLabTS_pupa <- data.frame(
  temp = c(10.0, 10.0, 10.0, 13.0, 15.0, 15.0, 15.5, 15.5, 16.0, 16.0, 17.0, 
           20.0, 20.0, 25.0, 25.0, 30.0, 35.0), 
  devRate = c(0.001, 0.008, 0.012, 0.044, 0.017, 0.044, 0.039, 0.037, 0.034, 0.051, 
              0.051, 0.08, 0.092, 0.102, 0.073, 0.005, 0.0002))
## ajustar un modelo a los datos (Lactin-1)
## ver vignette quickUserGuide para mayor informacion
modTs01_egg <- devRateModel(
  eq = lactin1_95, 
  dfData = datosLabTS_egg, 
  startValues = list(aa = 0.177, Tmax = 36.586, deltaT = 5.631))
modTs01_larva <- devRateModel(
  eq = lactin1_95, 
  dfData = datosLabTS_larva, 
  startValues = list(aa = 0.177, Tmax = 36.586, deltaT = 5.631))
modTs01_pupa <- devRateModel(
  eq = lactin1_95, 
  dfData = datosLabTS_pupa, 
  startValues = list(aa = 0.177, Tmax = 36.586, deltaT = 5.631))

#A partir de los modelos ajustados a T. solanivora y una población de 50 individuos simulados, 
#podemos representar la distribución de las generaciones con el tiempo. Para agregar realismo a la simulación,
#añadimos variabilidad en la respuesta de los insectos a la temperatura que seguirá una distribución Normal 
#de parametro mu igual al valor de la tasa de desarrollo y de parametro sigma = 0.025.

simul01 <- devRateIBM(
  tempTS = rnorm(n = 100, mean = 15, sd = 1),
  timeStepTS = 1,
  models = list(modTs01_egg, modTs01_larva, modTs01_pupa),
  numInd = 50,
  stocha = 0.025,
  timeLayEggs = 1)
print(simul01)

# graficar la simulacion
par(mar = c(4, 4, 0, 0))
devRateIBMPlot(ibm = simul01)

# modificacion de la serie temporal de temperaturas introduciendo un aumento en las temperaturas, 
#por ejemplo pasando de un promedio de 15 a 17 grados.

simul02 <- devRateIBM(
  tempTS = rnorm(n = 100, mean = 17, sd = 1),
  timeStepTS = 1,
  models = list(modTs01_egg, modTs01_larva, modTs01_pupa),
  numInd = 50,
  stocha = 0.025,
  timeLayEggs = 1)
par(mar = c(4, 4, 0, 0))
devRateIBMPlot(ibm = simul02)

#Tambien podemos aumentar la variabilidad de las temperaturas al cambiar la desviación estándar de la 
#ley normal que representa las temperaturas, y pasar sigma de 1 a 2.

simul02 <- devRateIBM(
  tempTS = rnorm(n = 100, mean = 17, sd = 2),
  timeStepTS = 1,
  models = list(modTs01_egg, modTs01_larva, modTs01_pupa),
  numInd = 50,
  stocha = 0.025,
  timeLayEggs = 1)
par(mar = c(4, 4, 0, 0))
devRateIBMPlot(ibm = simul02)

#crear un entorno teórico para representar un mapa donde a cada punto corresponde 
#un dato de temperatura que vamos a usar para calcular el numero de generaciones potenciales 
#simulando 100 días a esta temperatura.

tempEspacio <- matrix(rnorm(100, mean = 15, sd = 1), ncol = 10) # mapa teorico
myDevRate <- 1/devRateMap(nlsDR = modTs01_egg, tempMap = tempEspacio) +
  1/devRateMap(nlsDR = modTs01_larva, tempMap = tempEspacio) +
  1/devRateMap(nlsDR = modTs01_pupa, tempMap = tempEspacio)
filled.contour(100 / myDevRate, # numero de generaciones en 100 dias
               col = rev(heat.colors(30)), 
               main = "#generaciones", plot.axes = FALSE)