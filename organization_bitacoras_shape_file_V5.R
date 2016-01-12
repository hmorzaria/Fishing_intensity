# July 2015
# organize shapefiles from bitacoras
#for fishery intensity project
#hmorzarialuna@gmail.com

#install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
"rgdal","rgeos","maptools","raster","pipeR","data.table")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")

#List of fishers and acronyms
names_fishers = fread("Fishers_names.csv", header=TRUE)

#Bitacora_database
bitacoras_data = fread("Datos_bitacoras.csv", header=TRUE)
#CHECK THERE IS A PROBLEM WITH THE NA VALUES IN THIS FILE

#Species names
species_data = fread("species_names.csv", header=TRUE)


setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_original")

mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_original"

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_revisado"

#this is the empty shapefile with for the Northern Gulf

base.file.name = "Atlantis12_AggregatePolygons"



#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

table.codes = matrix(0,0,1) %>% 
  data.frame %>% 
  setnames ("shape.file.name")

table.species.folders = matrix(0,0,2) %>% 
  data.frame %>% 
setnames(c("name.species","sp.name"))

table.fishers = matrix(0,0,1) %>% 
  data.frame %>% 
setnames("Fisher_name")

table.missing.codes = matrix(0,0,1) %>% 
  data.frame %>% 
setnames("new.shape.file.name")

table.duplicate.codes = matrix(0,0,37) %>% 
  data.frame 

  for (each.folder in 1:length(location.folders))
    
    {
    
    name.location = location.folders[each.folder] %>% 
      strsplit("/") %>% 
      unlist %>% 
      .[2]
      
    
   paste(mainpath,name.location,sep="/") %>% 
     setwd
    
    species.folders <- list.dirs(full.names = TRUE, recursive=FALSE)
    
    for(each.species.folder in 1:length(species.folders))
 { 
  
      name.species = species.folders[each.species.folder] %>% 
        strsplit("/") %>% 
        unlist %>% 
        .[2]

path.this.species.folder = paste(mainpath,name.location,name.species,sep="/")

setwd(path.this.species.folder)

files <- list.files(pattern = "\\.shp$")

  for(each.file in 1:length(files)){
    
   
  print(paste("Analyzing file ",each.file," of ",length(files)," named ",files[each.file],sep=""))
      
    this.shape.file = files[each.file]
    shape.file.name = files[each.file] %>% 
      strsplit("[.]") %>% 
      unlist %>% 
      .[1]
    
    sp.name.string = shape.file.name %>% 
      strsplit("-") %>% 
      unlist
    
    if(length(sp.name.string) ==3){
      
      sp.name = sp.name.string %>% 
        .[3] 
      newcode.name = species_data$NewCode[species_data$SppCode==sp.name & species_data$Folder==name.species]
      newcode.name = drop.levels(newcode.name)
      
      new.shape.file.name = paste((unlist(strsplit(shape.file.name,"-"))[1]),(unlist(strsplit(shape.file.name,"-"))[2]),newcode.name,sep="-")
      
      #write fisher code to table
      name.fisher = sp.name.string %>% 
        .[2] 
      
      name.fisher2 = sp.name.string %>% 
        .[2] %>% 
        data.frame %>% 
        setnames("Fisher_name")
      
      table.fishers = rbind(table.fishers, name.fisher2)
    }
    
    
    if(length(sp.name.string) ==4){
      
      loc.name.code = sp.name.string[1]
      
      name.code = sp.name.string[2]
      sp.name = sp.name.string[4]
      
      loc.code.name = paste(loc.name.code,name.code,sep="")
      newcode.name = species_data$NewCode[species_data$SppCode==sp.name & species_data$Folder==name.species]
      newcode.name = drop.levels(newcode.name)
      
      new.shape.file.name = paste(loc.code.name,sp.name.string[3],newcode.name, sep="-")
      
     
      #write fisher code to table
      name.fisher = sp.name.string[3]
      name.fisher2 = name.fisher %>% 
        data.frame %>% 
      setnames("Fisher_name")
      table.fishers = rbind(table.fishers, name.fisher2)
    }
    
    #Save code name to its own table
    file.name = new.shape.file.name %>% 
      data.frame %>% 
      setnames("CODE")
    
    table.codes = rbind(table.codes,file.name)
      
    #read shapefile
    #ogrInfo(".", shape.file.name)
    shape.rg <- readOGR(".", shape.file.name)
    shape.rg$sorting_id <- sapply(slot(shape.rg, "polygons"), function(x) slot(x, "ID"))
    shape.rg$sorting_id = as.numeric(shape.rg$sorting_id)
    
    orig.table <- shape.rg %>% 
      data.frame
    
    new.table = orig.table %>% 
tbl_df %>% 
      dplyr::select(Id,XMIN,XMAX,YMIN,YMAX,Y_Code,X_Code,sorting_id)
   
      print(paste("Datatable in file ",this.shape.file, ' has ',ncol(new.table), " columns", sep=""))
     
      #get common name
    
    #common.name = species_data$Common[species_data$SppCode==sp.name]
    
    common.name = species_data$Common[species_data$SppCode==sp.name & species_data$Folder==name.species]
    group.name = species_data$Group[species_data$SppCode==sp.name & species_data$Folder==name.species]  
    sci.name = species_data$Scientific[species_data$SppCode==sp.name & species_data$Folder==name.species]  
    
    common.name = common.name %>% 
      drop.levels %>% 
      as.character
    
    print(common.name)
    
      bitacoras.list.codes = unique(bitacoras_data$Code)
    
    if (is.element(new.shape.file.name, bitacoras.list.codes) == TRUE)
    {
      # Get record from full bitacoras spreadsheet
      
      this.file.data= subset(bitacoras_data, Code%in% new.shape.file.name)
      
      #add catch for double entries
      
      if(nrow(this.file.data) > 1)
{
  print("duplicate entry")
  
  setnames(table.duplicate.codes, names(this.file.data))
        
  table.duplicate.codes = rbind(table.duplicate.codes,this.file.data)
  
  new.table = new.table %>%
        dplyr::mutate(LOC = name.location, CODE = shape.file.name,
                  SPPCODE = newcode.name, GROUP = group.name, SCINAME = sci.name,
                  COMMON = common.name, NOFISHER = NA,
                  GEAR = NA, DEPTH_A = NA,
                  FTIME = NA, EXPENSE = NA,
                  WEIGHT = NA, FISHER = NA,
                  FISHERCODE = name.fisher, MONTH = NA, DAY = NA,
                  YR = NA)
  }
      


      #this.file.data = bitacoras_data[bitacoras_data$Code==shape.file.name]
      
if(nrow(this.file.data) == 1)
{
  new.table = new.table %>% 
    dplyr::mutate(LOC = name.location, CODE = shape.file.name,
      SPPCODE = newcode.name, GROUP = group.name, SCINAME = sci.name,
      COMMON = common.name, NOFISHER = this.file.data$Fisher_No,
      GEAR = this.file.data$Fishing_gear,DEPTH_A = this.file.data$Depth_A,
      FTIME = this.file.data$Fishing_time,EXPENSE = this.file.data$Fishing_time,
      WEIGHT = this.file.data$Weight, FISHER = this.file.data$Fisher_name,
      FISHERCODE = name.fisher, MONTH = this.file.data$Month, DAY = this.file.data$Day,
      YR = this.file.data$Year)
}
    }
    
    
    if (is.element(new.shape.file.name, bitacoras.list.codes) == FALSE) 
    {
     
      
      new.shape.file.name2 = new.shape.file.name %>% 
        data.frame %>%
        setnames("missing_codes")
      
      table.missing.codes %>% 
        setnames("missing_codes")
      
       table.missing.codes = rbind(table.missing.codes,new.shape.file.name2)
      
        new.table = new.table %>% 
        dplyr::mutate(LOC = name.location, CODE = shape.file.name,
                      SPPCODE = newcode.name, GROUP = group.name, SCINAME = sci.name,
                      COMMON = common.name, NOFISHER = NA,
                      GEAR = NA, DEPTH_A = NA,
                      FTIME = NA, EXPENSE = NA,
                      WEIGHT = NA, FISHER = NA,
                      FISHERCODE = name.fisher, MONTH = NA, DAY = NA,
                      YR = NA)
     
    }
    
      name.index = cbind(name.species,sp.name) %>% 
        data.frame %>% 
setnames(c("FolderName","Code"))
      
      table.species.folders =  rbind(table.species.folders,name.index)
      
        new.table.ordered <- new.table[order(new.table$sorting_id), ]
    row.names(new.table.ordered) <- row.names(orig.table)
    shape.new = shape.rg
   table.dim = ncol(new.table.ordered)
   
   new.table.ordered = new.table.ordered %>%  
     data.frame
   
   print(paste("Datatable in file ",this.shape.file, ' has ',ncol(new.table.ordered), " columns", sep=""))
   print(head(new.table.ordered))
   
   shape.new@data = new.table.ordered
    
    
    dir.create(paste(pathToSaveShapes,newcode.name,sep="/"))
    
    paste(pathToSaveShapes,newcode.name,sep="/") %>% 
      setwd
    
    writeOGR(shape.new, ".", new.shape.file.name,driver="ESRI Shapefile",overwrite_layer=TRUE)
    
    setwd(path.this.species.folder)    
    
    
    
  }


}

  }

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")

bitacoras.list.codes = as.data.frame(bitacoras.list.codes)
names(bitacoras.list.codes) = "Code"

write.csv(bitacoras.list.codes, file="bitacoras_list_codes.csv")


names(table.codes) = "Code"

write.csv(table.codes, file="table_codes_shapes.csv")

names(table.fishers) = "Fisher_name"
fisher.names = unique(table.fishers$Fisher_name)
write.csv(fisher.names, file="table_fishers.csv")

names(table.missing.codes) = "Missing_code"

write.csv(table.missing.codes, file="table_missing_codes.csv")

write.csv(table.duplicate.codes, file="table_duplicate_codes.csv")

names(table.species.folders) = c("FolderName","Code")
write.csv(table.species.folders, file="table_codesinfolders.csv")

#get bitacora data without shapefiles
table.codes = table.codes %>% 
  tbl_df

missing.shapes = bitacoras.list.codes %>% 
  tbl_df %>% 
  anti_join(table.codes, by="Code")
  
write.csv(missing.shapes, "missing_shapes.csv")

## Now merge all polygon layers
##Based on code by Zia Ahmed zua3 at cornell.edu 
###############################################################################

# Get list of all shape files from directory
#------------------------------------------------------------

setwd(pathToSaveShapes)


species.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

for (eachfolder in 1:length(species.folders)) 
  
{
  this.folder = species.folders[eachfolder]
  
  name.sp.folder = unlist(strsplit(this.folder,"/"))[2]
  
  print(name.sp.folder)
paththisSpecies = paste(pathToSaveShapes,name.sp.folder,sep="/")

  setwd(paththisSpecies)

  files = list.files(pattern=".shp$", recursive=FALSE)

  uid=1

# Get polygons from first file 
#-------------------------------------

  first.shape= unlist(strsplit(files[1],"[.]"))[1]
  
poly.data<- readOGR(".",first.shape)

n <- length(slot(poly.data, "polygons"))
poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1)))
uid <- uid + n

# mapunit polygon: combine remaining  polygons with first polygon
#-----------------------------------------------------------------
  
  
  if(length(files)>1){
   
    for (eachfile in 2:length(files)) {
    
    this.shape= unlist(strsplit(files[eachfile],"[.]"))[1]
    
    temp.data <- readOGR(".",this.shape)
    print(paste("Analyzing ",name.sp.folder," file ",eachfile,"of ",length(files),this.shape,sep=""))
    n <- length(slot(temp.data, "polygons"))
    temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
    uid <- uid + n
    poly.data <- spRbind(poly.data,temp.data)
  }
  }
  

names(poly.data)
proj4string(poly.data) = crs.geo

setwd(pathToSaveShapes)
writeOGR(poly.data, ".", paste(name.sp.folder,"_merge",sep=""),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  }



#now make merged file into raster
#-----------------------------------------------------------------

setwd(pathToSaveShapes)

#make empty raster to extent of Northern Gulf
shape.rg <- readOGR(".", base.file.name)

e.shape <- extent(shape.rg)
mask.raster = raster(e.shape)
res(mask.raster) = 1000 # The log books used maps of resolution 2700 m
#Marcia used resolution of 1km2 (1000 x 1000 m)
#set background values to 0
mask.raster[is.na(mask.raster)] <- 0
proj4string(mask.raster) = crs.geo

merged.species=list.files(getwd(),  pattern="_merge.shp$", full.names=FALSE)

for(eachmergedfile in 1:length(merged.species))
  
{
  this.merged.species = merged.species[eachmergedfile]
  merged.file.name = unlist(strsplit(this.merged.species,"[.]"))[1]
  ogrInfo(".", merged.file.name)
  merge.rg <- readOGR(".", merged.file.name)
  
   
  value = 1
  
  out.r <- rasterize(merge.rg, field="LOC", mask.raster)
  
  # set the cells associated with the shapfile to the specified value
  out.r[!is.na(out.r)] <- value
  out.r[is.na(out.r)] <- 0
  
  # export to the working directory as a tif file
  
  writeRaster(out.r, filename=merged.file.name, format="GTiff", overwrite=TRUE)  
  
}



# Make a raster stack
#-----------------------------------------------------------------

#Go back to main directory
setwd(pathToSaveShapes)

# list all raster files and make stack

raster.species=stack(list.files(getwd(),  pattern=".tif$", full.names=FALSE))

raster.species[is.na(raster.species)] <- 0
#sum to get fishing intensity

fishing.intensity= sum(raster.species)



writeRaster(fishing.intensity, filename="fishing_intensity", format="GTiff", overwrite=TRUE)  

coast.rg <- readOGR(".", "GOC_Clip_polygon_Project")

X11()
plot(fishing.intensity, main="Frequencia pesquerias en base a bitacoras")
plot(coast.rg, add=TRUE)


#NOW merge all species together
##Based on code by Zia Ahmed zua3 at cornell.edu 
###############################################################################

# Get list of all shape files from directory
#------------------------------------------------------------

setwd(pathToSaveShapes)


files = list.files(pattern="_merge.shp$", recursive=FALSE)
  
  uid=1
  
  # Get polygons from first file 
  #-------------------------------------
  
  first.shape= unlist(strsplit(files[1],"[.]"))[1]
  
  poly.data<- readOGR(".",first.shape)
  
  n <- length(slot(poly.data, "polygons"))
  poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1)))
  uid <- uid + n
  
  # mapunit polygon: combin remaining  polygons with first polygon
  #-----------------------------------------------------------------
  
    
    for (eachfile in 2:length(files)) {
      
      this.shape= unlist(strsplit(files[eachfile],"[.]"))[1]
      
      temp.data <- readOGR(".",this.shape)
      n <- length(slot(temp.data, "polygons"))
      temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
      uid <- uid + n
      poly.data <- spRbind(poly.data,temp.data)
    }
  
  
  
  names(poly.data)
  proj4string(poly.data) = crs.geo
  
  setwd(pathToSaveShapes)
  writeOGR(poly.data, ".", "All_species_bitacoras",driver="ESRI Shapefile",overwrite_layer=TRUE)
  

#Calculate fishing frequency just for species in the corridor

all.rg <- readOGR(".", "All_species_bitacoras")


corridor.com <- all.rg[all.rg$LOC == "PLO" | all.rg$LOC == "PPE" | all.rg$LOC =="SJO",]

sp.levels = levels(corridor.com$NOM_ESP)

for (eachspecies in 1:length(sp.levels))
{
  this.species = as.character(sp.levels[eachspecies])
  
  #test the species is still present
  
  test.sp = corridor.com$NOM_ESP == this.species
  
  if(TRUE %in% test.sp==TRUE) {
    sp.corridor.com <- corridor.com[corridor.com$NOM_ESP == this.species,]
    
    writeOGR(sp.corridor.com, ".", paste(this.species,"_corridor",sep=""),driver="ESRI Shapefile",overwrite_layer=TRUE)
    
    value = 1
    
    out.r <- rasterize(sp.corridor.com, field="LOC", mask.raster)
    
    # set the cells associated with the shapfile to the specified value
    out.r[!is.na(out.r)] <- value
    out.r[is.na(out.r)] <- 0
    
    # export to the working directory as a tif file
    
    writeRaster(out.r, filename=paste(this.species,"_corridor",sep=""), format="GTiff", overwrite=TRUE)  
    
  }
  
}



# list all raster files and make stack

raster.species=stack(list.files(getwd(),  pattern="corridor.tif$", full.names=FALSE))

raster.species[is.na(raster.species)] <- 0
#sum to get fishing intensity

fishing.intensity.com= sum(raster.species)



writeRaster(fishing.intensity.com, filename="fishing_intensity_corridor", format="GTiff", overwrite=TRUE)  

coast.rg <- readOGR(".", "GOC_Clip_polygon_Project")

X11()
plot(fishing.intensity.com, main="Frequencia pesquerias en base a bitacoras")
plot(coast.rg, add=TRUE)