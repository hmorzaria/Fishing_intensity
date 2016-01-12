# Hem Nalini Mozaria Luna                                                                                                            # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
# July 30, 2014
# Extract biomass values for specific cells

library(gdata)
library(ggplot2)
library(reshape)
library(plyr)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(rasterVis)

rm(list=ls())

graphics.off()

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation")

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"

#this is the empty shapefile with for the Northern Gulf

#base.file.name = "propuesta_corredor_costero"
base.file.name = "propuesta_corredor_3"

all.rg <- readOGR(".", base.file.name)
proj4string(all.rg)
X11()
plot(all.rg)
poly.area = gArea(all.rg)


poly.area.km = round((poly.area/1000000),3)
#This function can be used to obtain the area of multiple polygons
# sapply(data@polygons, function(x) x@Polygons[[1]]@area)

print(paste("Area",poly.area.km,"km2", sep=" "))



