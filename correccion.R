#instalación de paquetes para Teledetección
# en esta computadora no correr el bloque 1 sino el bloque 0 linea a linea

install.packages("sp")

install.packages("sf")

install.packages("terra")

install.packages("raster")

install.packages("rgdal", repos="https://cloud.r-project.org/",type="source")

install.packages("rgeos")

install.packages("ggplot2")

install.packages("devtools")

install.packages("usethis")

install.packages("reshape")

install.packages("randomcoloR")

#Instalación fuera del repositorio de CRAN de RStoolbox. (Ejecutar las siguientes lineas de codigo (19-21) posterior a la instalación de Rtools 4.2)

library(usethis)

library(devtools)

install_github("bleutner/RStoolbox",force = TRUE)
1

install_github("RcppCore/RcppArmadillo",force=TRUE)
1

install_github("HenrikBengtsson/parallelly",force=TRUE)

#bloque 1

install.packages("sp")
install.packages("raster")
install.packages("RStoolbox")
install.packages("rgdal")
install.packages("rgeos")
install.packages("ggplot2")
install.packages("factoextra")

#utilizar paquetes para teledetecion

suppressMessages(library(raster))
suppressMessages(library(RStoolbox))
suppressMessages(library(rgdal))
suppressMessages(library(sp))
suppressMessages(library(rgeos))
suppressMessages(library(ggplot2))
suppressMessages(library(factoextra))

# bloque 2

mtlfile <-"LC08_L1TP_011063_20230928_20231003_02_T1_MTL.txt"
metadata <- readMeta(mtlfile)
lsat <- stackMeta(mtlfile)
plotRGB (lsat, 5,4,3 ,stretch="lin")
plotRGB (lsat, 6,5,4 ,stretch="lin")
shp<- readOGR(".","area")
plot(shp)
projection(lsat)
projection(shp)

#recorte de una capa raster con un shp
corte <- crop(lsat, extent(shp))
mascara <- mask(corte, shp)
plotRGB(mascara, 5,4,3, stretch= "lin")

#bloque 3
#correcion atmosferica landsat8-preprocesamiento
lsta_dos <- radCor(mascara,metaData = metadata, method = "dos")

nir <- lsta_dos[[5]]
red <- lsta_dos[[4]]

ndvi <- (nir - red)/(nir + red)

layout(matrix(c(1),1,1))
plot(ndvi,axes=TRUE, main="NDVI - LANDSAT 8")

#ndvi para cuerpos de agua
green <- lsta_dos[[3]]
ndwi <- (green - nir)/(green + nir)
layout(matrix(c(1),1,1))
plot(ndwi,col=rev(topo.colors(1000)), main="NDWI - LANDSAT 8")

#bloque 4 proyeccion de un raster y extraccion de bandas
 ndwi_wgs<- projectRaster(ndwi, crs=CRS("+init=epsg:4326"), method = "bilinear")
 ndvi_wgs<- projectRaster(ndvi, crs=CRS("+init=epsg:4326"), method = "bilinear")  
 
 projection(ndwi_wgs)

 plot(ndwi_wgs,axes=TRUE, main="NDWI - LANDSAT8") 
 plot(ndvi_wgs,axes=TRUE, main="NDVI - LANDSAT8") 
 
 lsta_dos
 plot (lsta_dos)
 landsat2 <- subset(lsta_dos, 1:7)
 plot(landsat2)

 dtf = as.data.frame(landsat2) 
 cor(na.omit(dtf))
 acp =prcomp(na.omit(dtf))
 summary(acp)

fviz_pca_var(acp, col.var = "contrib", bgradient.cols =c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)  

PCA <-rasterPCA(landsat2)
projection(landsatFaca)
projection(PCA)

par(mfrow = c(1,2))
plotRGB(PCA$map, 1,2,3, stretch="lin", main="PCA - LANDSAT8")
plotRGB(landsat2,5,4,3, stretch="lin", main="landsat8")

#exportar mapa PCA, puede ser cualquier mapa
writeRaster(lsta_dos,"delta.tif",overwrite=TRUE)


