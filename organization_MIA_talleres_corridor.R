# Hem Nalini Morzaria Luna                                                                                         # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
# February 27, 2015
# Read shape files and rasterize

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

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Datos_MIA")

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Datos_MIA"

location.code = "ppe"
#this is the empty shapefile with for the Northern Gulf

base.file.name = "Atlantis12_AggregatePolygons"
#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

#make mask raster
base.rg <- readOGR(".", base.file.name)

e.shape <- extent(base.rg)
mask.raster = raster(e.shape)
res(mask.raster) = 3000 # The log books used maps of resolution 2700 m
#Marcia used resolution of 1km2 (1000 x 1000 m)
#set background values to 0
mask.raster[is.na(mask.raster)] <- 0
proj4string(mask.raster) = crs.geo



file.list = list.files(getwd(),  pattern = ".*_ppe.shp$", full.names=FALSE)


for (eachfile in 1:length(file.list))
  
{
  
shape.file.name = unlist(strsplit(file.list[eachfile],"[.]"))[1]
  

ogrInfo(".", shape.file.name)
shape.rg <- readOGR(".", shape.file.name)
proj4string(shape.rg) = crs.geo
shape.rg$sorting_id <- sapply(slot(shape.rg, "polygons"), function(x) slot(x, "ID"))
shape.rg$sorting_id = as.numeric(shape.rg$sorting_id)

orig.table <- as(shape.rg, "data.frame")
new.table = orig.table
new.table$LOC = location.code
new.table$value = 1

#check if TIPO column is present
# if so remove it
my.names = colnames(new.table)
check.names = grepl("TIPO",my.names)

result.test = is.element('TRUE', check.names)

if(result.test == TRUE)
{
  new.table=subset(new.table, select = -TIPO)
}

#check if TIPO column is present
# if so remove it
my.names = colnames(new.table)
check.names = grepl("TIEMPO",my.names)

result.test = is.element('TRUE', check.names)

if(result.test == TRUE)
{
  new.table=subset(new.table, select = -TIEMPO)
}

new.table.ordered <- new.table[order(new.table$sorting_id), ]
row.names(new.table.ordered) <- row.names(orig.table)
shape.new = shape.rg
shape.new@data = new.table.ordered

X11()
plot(mask.raster)
plot(shape.new, add=TRUE)

    value = 1
    
    out.r <- rasterize(shape.new, mask.raster, shape.new$value)
    
    # set the cells associated with the shapfile to the specified value
    out.r[!is.na(out.r)] <- value
    out.r[is.na(out.r)] <- 0
    
    # export to the working directory as a tif file
    setwd(pathToSaveShapes)
    writeRaster(out.r, filename=paste("MIA",shape.file.name,sep="_"), format="GTiff", overwrite=TRUE)  
    
  }
  
 




# list all raster files and make stack

setwd(pathToSaveShapes)
raster.species=stack(list.files(getwd(),  pattern = ".*tif$", full.names=FALSE))

raster.species[is.na(raster.species)] <- 0
#sum to get fishing intensity

fishing.intensity.com= sum(raster.species)


writeRaster(fishing.intensity.com, filename="MIA_fishing_intensity_corridor", format="GTiff", overwrite=TRUE)  

coast.rg <- readOGR(".", "GOC_Clip_polygon_Project")

X11()
plot(fishing.intensity.com, main="Frequencia pesquerias en base a talleres MIA")
plot(coast.rg, add=TRUE)
