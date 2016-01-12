# Hem Nalini Mozaria Luna                                                                                                            # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
# January, 28, 2015
# Takes a preexisting polygon grid and extracts polygons based on point data

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

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Fishing_Intensity")


pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Fishing_Intensity/CEDO_data_lm_fishery/CEDO_as_polygon"


base.file.name = "Square_grid_NG_1kmsq"
#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

all.rg <- readOGR(".", base.file.name)


setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Fishing_Intensity/CEDO_data_lm_fishery/")

files <- list.files(pattern = "\\.shp$")

for(eachfile in 1:length(files))
  
{
  this.shape.file = files[eachfile]
  
  shape.file.name = unlist(strsplit(this.shape.file,"[.]"))[1]
  
  ogrInfo(".", shape.file.name)
  shape.rg <- readOGR(".", shape.file.name)
  
  overlay = over(all.rg, shape.rg)  
  ogrInfo(".", overlay)
}


