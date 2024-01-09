install.packages("e1071")
install.packages("snow")
install.packages("rpart")
install.packages("randomForest")

suppressMessages(library(raster))
suppressMessages(library(sp))
suppressMessages(library(RStoolbox))
suppressMessages(library(rgdal))
suppressMessages(library(e1071))
suppressMessages(library(snow))
suppressMessages(library(rpart))
suppressMessages(library(randomForest))

imgs2 <- brick("delta.tif")
nlayers(imgs2)



names(imgs2)
landsat <- subset(imgs2, 2:7)
plot(landsat)

#La letra c significa "concatenar"
names(landsat) <- c("BLUE","GREEN","RED","NIR","SWIR1","SWIR2")
names(landsat)


######## Tasseled cap########

lsat_tasseledcap <- tasseledCap(landsat, sat = "Landsat8OLI") 
par(mfrow = c(1,2))
plotRGB(landsat, 5,4,3, axes=TRUE, stretch="lin")
plotRGB(lsat_tasseledcap, 1,2,3, axes=TRUE, stretch="lin",main ="Tasseled Cap - Landsat 8")



######### Spectral Mixture Analysis######

df <- as.matrix(landsat)

# Lectura puntos espectrales.

puntos <- readOGR("PIXEL.shp")

# Extracción Espectral.

values <- extract(landsat, puntos)
ploteo <- function(point){
  plotRGB(landsat, 5,4,3, stretch="lin")
  plot(point,add=TRUE,col="red",lwd=3, cex=2)
  text(point,labels=point$CLASE,pos=1, lwd=6, cex=0.8,col="black")
}
ploteo(puntos)

values
#Reorganizar la matriz de datos 6 Bandas y 3 Clases 

M<-t(values)

#Obtención del Spectral Mixture Análisis 

sma <- lapply(1:dim(df)[1], function(i) f<-((solve(t(M)%*%M))%*%t(M))%*%df[i,])

#Extracción las fracciones para cada clase.

forest <- lapply(1:dim(df)[1], function(i) sma[[i]][1,1])
water <- lapply(1:dim(df)[1], function(i) sma[[i]][2,1])
urban <- lapply(1:dim(df)[1], function(i) sma[[i]][3,1])

#Almacenamiento de fracciones en la imagen raster.

frac_forest <- landsat[[1]]
frac_water <- landsat[[1]]
frac_urban <- landsat[[1]]

frac_forest[]<- as.numeric(forest)
frac_water[]<- as.numeric(water)
frac_urban[]<- as.numeric(urban)

#Stack de las fracciones y ploteo del mapa.

apilacion<-stack(frac_water, frac_forest, frac_urban)
par(mfrow=c(1,2))
plotRGB(landsat, 5,4,3, stretch="lin", axes = TRUE)
plotRGB(apilacion, 3, 1, 2, axes=TRUE, stretch="lin", main= "titulo 1")


##Desmezcla de la imagen para las fracciones.
#mesma realiza un análisis de mezcla espectral de múltiples miembros finales en una imagen ráster multibanda.
#NNLS:aplica una regresión de mínimos cuadrados no negativos (NNLS) que utiliza un algoritmo de coordenadas secuenciales (SCA) basado en Franc et al. (2005).


Unmix <- mesma(landsat, values, method = "NNLS")
raster::hist(Unmix$layer.1)
raster::plot(Unmix$layer.1, col = c("white","Blue"))

par(mfrow=c(2,2))
plotRGB(apilacion, 3, 1, 2, main= "titulo 1", axes=TRUE, stretch="lin")
raster::plot(Unmix$layer.1, col = c("white","blue"))
raster::plot(Unmix$layer.2, col = c("white","green"))
raster::plot(Unmix$layer.3, col = c("white","brown"))
plotR