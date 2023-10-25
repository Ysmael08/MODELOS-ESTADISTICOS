#ESTE CODIGO ES UNA MODIFICACION DEL PRESENTADO EN EL CANAL DE YOUTUBE ANDRES FARALL

# cargar paquete readxl
library(readxl)
#buscar la ruta del archivo de excel
file.choose()
# copiar ruta de la consola y guardar en variable
ruta_excel <- "C:\\Users\\Walter\\Desktop\\MODELOS ESTADISTICOS\\PREDICCION DEPORTES\\BD.xlsx"
# mirar las hojas del excel
excel_sheets(ruta_excel)
#cargar la base de datos
partidos <- read_excel(ruta_excel, sheet= 'PREMIER_LEAGUE')
partidos

#dejamos los datos de columnas que nos importan
partidos <- partidos[3:8]
partidos
#borramos la columna 2
partidos <- partidos[-2]
partidos
#borramos las filas que no tengan datos
partidos <- na.omit(partidos[, c(1:5)])
partidos
#asignamos una fecha para hacer la prediccion
fechaModelo <-"2023-08-20" 
#armamos un dataframe con todos los datos del modelo
partidos.modelo <-data.frame(
  Ataque = as.factor(c(as.character(partidos$LOCAL), as.character(partidos$VISITANTE))),
  Defensa = as.factor(c(as.character(partidos$VISITANTE), as.character(partidos$LOCAL))),
  GolesAFavor = c(partidos$`GOLES LOCAL`,partidos$`GOLES VISITANTE`),
  
  #factor de depreciacion
  Diffecha = c(as.integer(difftime(fechaModelo, partidos$FECHA, units = "weeks")), as.integer(difftime(fechaModelo,partidos$FECHA, units = "weeks"))),
  Depreciacion = exp(-0.0065* (c(as.integer(difftime(fechaModelo, partidos$FECHA, units = "weeks")), as.integer(difftime(fechaModelo, partidos$FECHA, units = "weeks")))))
)
partidos.modelo

#modelo GLM Poisson
modelo <- glm(GolesAFavor ~ 0 + Ataque + Defensa, data= partidos.modelo, family = poisson(), weights = partidos.modelo$Depreciacion)
summary(modelo)

#calcular el estadistico de desviacion
estadistico_desviacion <- deviance(modelo) / modelo$df.residual

# Calcular el valor p correspondiente al estadístico de desviación
valor_p <- 1 - pchisq(estadistico_desviacion, modelo$df.residual)

# Comparar con un nivel de significancia predefinido
nivel_significancia <- 0.05
if (valor_p < nivel_significancia) {
  print("El modelo presenta sobredispersión")
} else {
  print("El modelo no presenta sobredispersión")
}


#prueba del modelo
#goles en promedio del equipo local
datos_partido_ataca_West_Ham = data.frame(Ataque = 'West_Ham', Defensa = 'Chelsea')
datos_partido_ataca_West_Ham
lambda <- predict(modelo,datos_partido_ataca_West_Ham, type='response') #"response" nos devuelve el lambda y no el ajuste de la regresion
lambda

#goles en promedio equipo visitante
datos_partido_ataca_Chelsea= data.frame(Ataque='Chelsea', Defensa ='West_Ham')
datos_partido_ataca_Chelsea
mu <- predict(modelo,datos_partido_ataca_Chelsea, type='response') #"response" nos devuelve el lambda y no el ajuste de la regresion
mu

maxgol <- 5 #Max cant goles

#queremos devolver la probabilidad de cada resultado asumiendo que tienen distribucion Poisson
#generamos una matrix con la probabilidad de que el partido termine con el resultado i-j, empezando desde el 0 goles

resultados <- dpois(0:maxgol, mu) %*% t(dpois(0:maxgol, lambda))
resultados*100

#tunning
#le sacamos proba a 1-0 y 0-1
resultados[1,2] <- resultados[1,2] * 0.90 #1-0 gana local
resultados[2,1] <- resultados[2,1] * 0.90 #0-1 gana visitante

#distribuimos todo lo que efectivamente sume 1 (acuerdense que la tabla esta truncada en la maxcantgoles)
sobrante <- 1-sum(resultados)
sobrante
# se la pasamos a los empates 0-0 1-1
resultados[1,1]<- resultados[1,1] * sobrante*0.5
resultados[2,2]<- resultados[2,2] * sobrante*0.5
resultados * 100

#sumamos probganarlocal y probganarvisitante

probganarlocal <- resultados[1,2] + resultados[1,3] + resultados[1,4] + resultados[1,5] + resultados [1,6] +
  resultados[2,3] + resultados[2,4] + resultados[2,5] + resultados [2,6] +
  resultados[3,4] + resultados[3,5] + resultados [3,6] +
  resultados[4,5] + resultados [4,6] +
  resultados [5,6]
probganarvisitante <- resultados[2,1] +
  resultados[3,1] + resultados[3,2] +
  resultados[4,1] + resultados[4,2] + resultados[4,3] +
  resultados[5,1] + resultados[5,2] + resultados[5,3] + resultados [5,4] +
  resultados[6,1] + resultados[6,2] + resultados[6,3] + resultados [6,4] + resultados [6,5]

#se calcula la probabilidad de ganar tanto del visitante y del local, ademas se calcula la cuota de apuesta
probganarlocal * 100; 1/probganarlocal
probganarvisitante *100; 1/probganarvisitante

