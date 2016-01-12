                                                                                                            # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
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

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/allinterviews_by_community")

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/allinterviews_by_community/By_species"

#this is the empty shapefile with for the Northern Gulf

base.file.name = "Atlantis12_AggregatePolygons"
#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")


all.rg <- readOGR(".", "all_interviews_v032710")


#corridor.com <- all.rg[all.rg$sitio_code == "STO" | all.rg$sitio_code == "DDC" | all.rg$sitio_code == "PJA" | all.rg$sitio_code == "PLO" | all.rg$sitio_code == "PPE" | all.rg$sitio_code =="SJO",]

corridor.com = all.rg

shape.rg <- readOGR(".", base.file.name)

e.shape <- extent(shape.rg)
mask.raster = raster(e.shape)
res(mask.raster) = 1000 # The log books used maps of resolution 2700 m
#Marcia used resolution of 1km2 (1000 x 1000 m)
#set background values to 0
mask.raster[is.na(mask.raster)] <- 0
proj4string(mask.raster) = crs.geo

#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

sp.levels = levels(corridor.com$spp_code)

for (eachspecies in 1:length(sp.levels))
{
  this.species = as.character(sp.levels[eachspecies])
  
  #test the species is still present
  
  test.sp = corridor.com$spp_code == this.species
  
  if(TRUE %in% test.sp==TRUE) {
    
    sp.corridor.com <- corridor.com[corridor.com$spp_code == this.species,]
    
    setwd(pathToSaveShapes)
    writeOGR(sp.corridor.com, ".", paste("all_interviews",this.species,sep="_"),driver="ESRI Shapefile",overwrite_layer=TRUE)
    
    value = 1
    
    out.r <- rasterize(sp.corridor.com, field="sitio_code", mask.raster)
    
    # set the cells associated with the shapfile to the specified value
    out.r[!is.na(out.r)] <- value
    out.r[is.na(out.r)] <- 0
    
    # export to the working directory as a tif file
    setwd(pathToSaveShapes)
    writeRaster(out.r, filename=paste("all_interviews",this.species,"corridor",sep="_"), format="GTiff", overwrite=TRUE)  
    
  }
  
 
}



# list all raster files and make stack

setwd(pathToSaveShapes)
raster.species=stack(list.files(getwd(),  pattern = ".*all.*tif$", full.names=FALSE))

raster.species[is.na(raster.species)] <- 0
#sum to get fishing intensity

fishing.intensity.com= sum(raster.species)


writeRaster(fishing.intensity.com, filename="PANGAS_fishing_intensity_corridor", format="GTiff", overwrite=TRUE)  

coast.rg <- readOGR(".", "GOC_Clip_polygon_Project")

X11()
plot(fishing.intensity.com, main="Frequencia pesquerias en base a datos entrevistas")
plot(coast.rg, add=TRUE)
