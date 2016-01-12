                                                                                                            # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
# Oct 31, 2015
# Organize datafiles and calculate fishing intensity

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
      "rgdal","rgeos","maptools","raster","data.table","wesanderson", "plyr")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()
setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity")
pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/CEDO/"
mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/"

#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

#read data
all.rg <- readOGR(".", "all_interviews_v032710")

setwd(mainpath)

#make empty raster to extent of Marcia's results

north.r = raster("ie_index_pangas_ProjectRaste.tif")

#define projection of data
proj.lcc = proj4string(north.r)
crs.geo <- CRS(proj.lcc)

e.shape <- extent(north.r)
ras.rows = nrow(north.r)
ras.cols = ncol(north.r)

mask.raster = raster(e.shape,ncol=ras.cols, nrow=ras.rows,crs=crs.geo)

#set background values to 0
mask.raster[is.na(mask.raster)] <- 0

loc.levels = unique(all.rg$sitio_code) %>% as.character

for (eachloc in 1:length(loc.levels))
{
  this.loc = loc.levels[eachloc]

  dir.create(paste(pathToSaveShapes,this.loc,sep=""))
  
  loc.corridor.com <- all.rg[all.rg$sitio_code == this.loc,]

  sp.levels = unique(loc.corridor.com$spp_code) %>% as.character

for (eachspecies in 1:length(sp.levels))
{
  this.species = sp.levels[eachspecies]
  
    sp.corridor.com <- loc.corridor.com[loc.corridor.com$spp_code == this.species,]
    
    setwd(paste(pathToSaveShapes,this.loc,sep=""))
    
    unique.id <- unique(sp.corridor.com@data$Id) %>% as.character
    
    uid = 1
    
    for (i in 1:length(unique.id)) {
      poly.data <- sp.corridor.com[sp.corridor.com$Id == unique.id[i], ] 
      n <- length(slot(poly.data, "polygons"))
      poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1)))
      uid <- uid + n
      
      pol.area= gArea(poly.data)
      poly.data$area = pol.area
      
      revalue(poly.data$CODE, c("-" = 1, "A" = 1,"ABC"= 1, "B"  = 1, "C"= 1,   "Y"= 0.5,   "Y1"= 0.5,  "Y2"= 0.5,  "Y3"= 0.5,  "YA" = 0.5, "YB"= 0.5,  "YC" = 0.5, "YD"= 0.5))
     
      poly.data$W= as.numeric(poly.data$CODE) / pol.area
    }

    # mapunit polygon: combin remaining  polygons with first polygoan
    
    for (i in 2:length(unique.id)) {
      temp.data <- sp.corridor.com[sp.corridor.com$Id == unique.id[i], ] 
      n <- length(slot(temp.data, "polygons"))
      temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
      uid <- uid + n
      
      pol.area= gArea(temp.data)
      temp.data$area = pol.area
      
      revalue(temp.data$CODE, c("-" = 1, "A" = 1,"ABC"= 1, "B"  = 1, "C"= 1,   "Y"= 0.5,   "Y1"= 0.5,  "Y2"= 0.5,  "Y3"= 0.5,  "YA" = 0.5, "YB"= 0.5,  "YC" = 0.5, "YD"= 0.5))
      
      temp.data$W= as.numeric(temp.data$CODE) / pol.area
      poly.data <- spRbind(poly.data,temp.data)
      
    } #end other polygons

  names(poly.data)
  proj4string(poly.data)
  
  poly.data$Indx = (poly.data$W - min(poly.data$W)) / (max(poly.data$W) - min(poly.data$W))
  
  writeOGR(poly.data, ".", paste(this.loc,this.species,"pangas_Ix",sep="_"),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  out.r <- rasterize(poly.data, field="Indx", mask.raster)
    
    # set the cells associated with the shapfile to the specified value
    out.r[is.na(out.r)] <- 0
    
    # export to the working directory as a tif file
    setwd(pathToSaveShapes)
    writeRaster(out.r, filename=paste(this.loc,this.species,"pangas_Ix",sep="_"), format="GTiff", overwrite=TRUE)  
    
  } # end species
  
 } # end folder



