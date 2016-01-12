#' July 2015
#' last edited Sep 2015
#' organize shapefiles from bitacoras
#' for fishery intensity project
#' hmorzarialuna@gmail.com
#' install.packages(c("ggplot2","reshape","RColorBrewer","classInt","maptools","rgeos"))

x = c("gdata","ggplot2","reshape","dplyr","RColorBrewer","classInt",
"rgdal","rgeos","maptools","raster","pipeR","data.table","wesanderson")

lapply(x, require, character.only = TRUE)

rm(list=ls())

graphics.off()

datapath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_original"

mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/"

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_revisado_loc"

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")


#List of fishers and acronyms
names_fishers = fread("Fishers_names.txt", header=TRUE) %>% 
  data.frame

#List of locations and acronyms
names_locations = fread("locations.csv", header=TRUE)

#Bitacora_database
bitacoras_data = fread("Datos_bitacoras.txt", header=TRUE)

#Species names
species_data = fread("species_names.csv", header=TRUE)



#this is the empty shapefile with for the Northern Gulf

setwd(datapath)

base.file.name = "GOC_Clip_polygon"


location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

#make matrices to store outputs
table.duplicate.codes = matrix(0,0,39) %>% 
  data.frame 

table.bitacoras = matrix(0,0,40) %>% 
  data.frame

#' Run loop that will search through folders and fix shapefiles

for (each.folder in 1:length(location.folders))
{
  name.location = location.folders[each.folder] %>% 
    strsplit("/") %>% 
    unlist %>% 
    .[2]
  
  paste(datapath,name.location,sep="/") %>% 
    setwd
  
  species.folders <- list.dirs(full.names = TRUE, recursive=FALSE)
  
  for(each.species.folder in 1:length(species.folders))
  { 
    name.species = species.folders[each.species.folder] %>% 
      strsplit("/") %>% 
      unlist %>% 
      .[2]
    
    path.this.species.folder = paste(datapath,name.location,name.species,sep="/")
    
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
        
        newcode.name = species_data %>% 
          tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% .$NewCode
        
        new.shape.file.name = paste((unlist(strsplit(shape.file.name,"-"))[1]),(unlist(strsplit(shape.file.name,"-"))[2]),newcode.name,sep="-")
        old.shape.file.name = paste((unlist(strsplit(shape.file.name,"-"))[1]),(unlist(strsplit(shape.file.name,"-"))[2]),sp.name,sep="-")
        
        #write fisher code to table
        name.fisher = sp.name.string %>% 
          .[2] 
        
        date.string = sp.name.string %>% 
          .[1] %>% 
          strsplit("")  
        date.string = date.string[[1]]
        
        month.string = paste0(date.string[6],date.string[7])
        day.string = paste0(date.string[4],date.string[5])
        year.string = paste0(date.string[8],date.string[9])
        
        date.string = paste(month.string,day.string,year.string,sep="/")
        
      }
      
      
      if(length(sp.name.string) ==4){
        
        loc.name.code = sp.name.string[1]
        
        name.code = sp.name.string[2]
        sp.name = sp.name.string[4]
        
        loc.code.name = paste(loc.name.code,name.code,sep="")
        
        newcode.name = species_data %>% 
          tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% .$NewCode
        
        new.shape.file.name = paste(loc.code.name,sp.name.string[3],newcode.name, sep="-")
        old.shape.file.name = paste(loc.code.name,sp.name.string[3],sp.name, sep="-")
        
        
        #write fisher code to table
        name.fisher = sp.name.string[3]
        
        date.string = sp.name.string %>% 
          .[2] %>% 
          strsplit("")  
        date.string = date.string[[1]]
        
        month.string = paste0(date.string[3],date.string[4])
        day.string = paste0(date.string[1],date.string[2])
        year.string = paste0(date.string[5],date.string[6])
        
        date.string = paste(month.string,day.string,year.string,sep="/")
        
        
      }
      #get full fisher.name
      fisher.name.full = names_fishers$Fisher_name[names_fishers$Initials==name.fisher]
      
      if(identical(fisher.name.full, character(0))==TRUE)
      {
        fisher.name.full = "NA"
      }
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
      
      #get taxonomy for species folder
      
      common.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% .$Common
      
      group.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% .$Group
      
      sci.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% 
        .$Scientific
      
      class.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% 
        .$Class
      
      order.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% 
        .$Order
      
      family.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% 
        .$Family
      
      genus.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% 
        .$Genus 
      
      species.name = species_data %>% 
        tbl_df %>% filter(Folder==name.species & SppCode == sp.name) %>% 
        .$Species
      
      common.name = common.name %>% 
        drop.levels %>% 
        as.character
      
      print(common.name)
      
      bitacoras.list.codes = unique(bitacoras_data$Code)
      
      if (is.element(old.shape.file.name, bitacoras.list.codes) == TRUE)
      {
        
        # Get record from full bitacoras spreadsheet
        
        this.file.data= subset(bitacoras_data, Code%in% old.shape.file.name)
        
        #add new shape file name, because the shapefiles will now have this
        this.file.data$New_Code = new.shape.file.name
        this.file.data$Record = "YES"
        
        #add catch for double entries
        
        if(nrow(this.file.data) > 1)
        {
          print("duplicate entry")
          
          setnames(table.duplicate.codes, names(this.file.data))
          
          table.duplicate.codes = rbind(table.duplicate.codes,this.file.data)
          
          new.table = new.table %>%
            dplyr::mutate(LOC = name.location, CODE = new.shape.file.name,
                          SPPCODE = newcode.name, GROUP = group.name, SCINAME = sci.name,
                          COMMON = common.name, NOFISHER = NA,
                          GEAR = NA, DEPTH_A = NA,
                          FTIME = NA, EXPENSE = NA,
                          WEIGHT = NA, FISHER = fisher.name.full,
                          FISHERCODE = name.fisher, MONTH = NA, DAY = NA,
                          YR = NA)
          
          this.file.data$Duplicate = "YES"
          datanames = names(this.file.data)
          setnames(table.bitacoras,datanames)
          table.bitacoras = rbind(table.bitacoras,this.file.data)
        }
        
        
        
        
        if(nrow(this.file.data) == 1)
        {
          new.table = new.table %>% 
            dplyr::mutate(LOC = name.location, CODE = new.shape.file.name,
                          SPPCODE = newcode.name, GROUP = group.name, SCINAME = sci.name,
                          COMMON = common.name, NOFISHER = this.file.data$Fisher_No,
                          GEAR = this.file.data$Fishing_gear,DEPTH_A = this.file.data$Depth_A,
                          FTIME = this.file.data$Fishing_time,EXPENSE = this.file.data$Fishing_time,
                          WEIGHT = this.file.data$Weight, FISHER = fisher.name.full,
                          FISHERCODE = name.fisher, MONTH = this.file.data$Month, DAY = this.file.data$Day,
                          YR = this.file.data$Year)
          
          this.file.data$Duplicate = "NO"
          datanames = names(this.file.data)
          setnames(table.bitacoras,datanames)
          table.bitacoras = rbind(table.bitacoras,this.file.data)
          
        }
      }
      
      
      if (is.element(old.shape.file.name, bitacoras.list.codes) == FALSE) 
      {
        
        new.table = new.table %>% 
          dplyr::mutate(LOC = name.location, CODE = new.shape.file.name,
                        SPPCODE = newcode.name, GROUP = group.name, SCINAME = sci.name,
                        COMMON = common.name, NOFISHER = NA,
                        GEAR = NA, DEPTH_A = NA,
                        FTIME = NA, EXPENSE = NA,
                        WEIGHT = NA, FISHER = fisher.name.full,
                        FISHERCODE = name.fisher, MONTH = NA, DAY = NA,
                        YR = NA)
        
        this.file.data = data.frame(old.shape.file.name)
        setnames(this.file.data,"Code")
        this.file.data = this.file.data %>% 
          tbl_df %>% 
          dplyr::mutate(Location = name.location, 
                        Date = date.string, Month = month.string, Day= day.string,
                        Year = year.string, Group = group.name, Class = class.name,
                        Order = order.name, Family = family.name, Genus = genus.name,
                        Species = species.name, Scientific = sci.name, Common = common.name,
                        Fisher_name = fisher.name.full, Work= NA, Fisher_No = NA, 
                        Fishing_site_A = NA, Site_A_Lat = NA, Site_A_Long = NA, 
                        Depth_A = NA, Fishing_site_B = NA, Site_B_Lat = NA,
                        Site_B_Long = NA, Depth_B = NA, Fishing_Site_C = NA, Site_C_Lat = NA,
                        Site_C_Long = NA, Depth_C = NA, Fishing_gear = NA, Gear_details = NA,
                        Fishing_time = NA, Expenses = NA, Weight = NA, Condition = NA,
                        Notes = NA, Coordinates = NA, 
                        New_Code= new.shape.file.name, Record = "NO",Duplicate = "NO")
        
        datanames = names(this.file.data)
        setnames(table.bitacoras,datanames)
        table.bitacoras = rbind(table.bitacoras,this.file.data)
        
        
        
      }
      
      
      new.table.ordered <- new.table[order(new.table$sorting_id), ]
      row.names(new.table.ordered) <- row.names(orig.table)
      shape.new = shape.rg
      table.dim = ncol(new.table.ordered)
      
      new.table.ordered = new.table.ordered %>%  
        data.frame
      
      print(paste("Datatable in file ",this.shape.file, ' has ',ncol(new.table.ordered), " columns", sep=""))
      print(head(new.table.ordered))
      
      shape.new@data = new.table.ordered
      
      
      dir.create(paste(pathToSaveShapes,name.location,sep="/"))
      
      paste(pathToSaveShapes,name.location,sep="/") %>% 
        setwd
      
      writeOGR(shape.new, ".", new.shape.file.name,driver="ESRI Shapefile",overwrite_layer=TRUE)
      
      setwd(path.this.species.folder)    
      
      
      
    }
    
    
  }
  
}

## make each polygon a raster
##Based on code by Zia Ahmed zua3 at cornell.edu 
###############################################################################

# Get list of all shape files from directory
#------------------------------------------------------------

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

species.codes = unique(species_data$NewCode)

setwd(pathToSaveShapes)
location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

for (eachfolder in 1:length(location.folders)) 
  
{
  this.folder = location.folders[eachfolder]
  
  name.sp.folder = unlist(strsplit(this.folder,"/"))[2]
  
  print(name.sp.folder)
paththisLocation = paste(pathToSaveShapes,name.sp.folder,sep="/")

  setwd(paththisLocation)

  shape.species=list.files(getwd(),  pattern=".shp$", full.names=FALSE)

for(eachshapefile in 1:length(shape.species))
  
{
  
  setwd(paththisLocation)
  
  this.shape.species = shape.species[eachshapefile]
  shape.file.name = unlist(strsplit(this.shape.species,"[.]"))[1]
  ogrInfo(".", shape.file.name)
  shape.rg = readOGR(".", shape.file.name) 
 
  #rasterize using mask raster
   out.r <- rasterize(shape.rg, mask.raster,field=shape.rg@data$LOC) 
   
  # set the cells associated with the shapfile to the specified value
  out.r[is.na(out.r)] <- 0
  
  # export to the working directory as a tif file
  
  writeRaster(out.r, filename=shape.file.name, format="GTiff", overwrite=TRUE)  
  
}
}

#' Make a raster stack to calculate fishing events per pixel all species
#' for each community

#-----------------------------------------------------------------
  setwd(pathToSaveShapes)
  
  location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)
  
  for (eachfolder in 1:length(location.folders)) 
    
  {
    this.folder = location.folders[eachfolder]
    
    name.sp.folder = unlist(strsplit(this.folder,"/"))[2]
    
    print(paste("Analyzing",name.sp.folder))
    
    paththisLocation = paste(pathToSaveShapes,name.sp.folder,sep="/")
    
    setwd(paththisLocation)
    
  raster.list=list.files(getwd(),  pattern=".tif$", full.names=FALSE)

  print(raster.list)
  raster.species=stack(raster.list)
  print("Done creating stack")  
  raster.species[is.na(raster.species)] <- 0
print("Done assigning 0 to NA in stack")
#sum to get total effort per community per cell
allsp.events= sum(raster.species)
print("Done summing all rasters") 
# list all raster files for each species and make stack

for(eachspecies in 1:length(species.codes))
{
  this.species = species.codes[eachspecies]
  print(paste("Analyzing",name.sp.folder,this.species))
  
  raster.files = list.files(getwd(),  pattern=paste(this.species,".tif$",sep=""), full.names=FALSE)
  
  if(length(raster.files)==1)
  {
    
    raster.species=stack(raster.files)
    
    raster.species[is.na(raster.species)] <- 0
    #sum to get fishing intensity
    
    sp.events = raster.species
    
    sp.comm.intensity = sp.events / allsp.events
    
    setwd(pathToSaveShapes)
    
    writeRaster(sp.comm.intensity, filename=paste(name.sp.folder,"_",this.species,sep=""), format="GTiff", overwrite=TRUE)  
  }
  
  if(length(raster.files)>1)
  {
    
raster.species=stack(raster.files)

raster.species[is.na(raster.species)] <- 0
#sum to get fishing intensity

sp.events= sum(raster.species)

sp.comm.intensity = sp.events / allsp.events

setwd(pathToSaveShapes)

writeRaster(sp.comm.intensity, filename=paste(name.sp.folder,"_",this.species,sep=""), format="GTiff", overwrite=TRUE)  

}

}
}

  
  #Now average rasters per species across communities
  
  setwd(pathToSaveShapes)
  
  for(eachspecies in 1:length(species.codes))
  {
    
    this.species = species.codes[eachspecies]
    
  raster.files = list.files(getwd(),  pattern=paste(this.species,".tif$",sep=""), full.names=FALSE)
  
  #get average price
  
  species.price = species_data %>% 
    filter(SppCode==this.species) %>% 
    .$Price %>% 
    as.numeric
  
  if(length(raster.files)==1)
  {
    
    raster.species=stack(raster.files)
    
    raster.species[is.na(raster.species)] <- 0
    #sum to get fishing intensity
    
    average.intensity= raster.species
    writeRaster(average.intensity, filename=paste(this.species,"_average",sep=""), format="GTiff", overwrite=TRUE)  
    #calculate economic index
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"_economic",sep=""), format="GTiff", overwrite=TRUE)  
  }
  
  if(length(raster.files)>1)
  {
    
    raster.species=stack(raster.files)
    
    raster.species[is.na(raster.species)] <- 0
    #sum to get fishing intensity
    
    average.intensity= mean(raster.species)
    
    writeRaster(average.intensity, filename=paste(this.species,"_average",sep=""), format="GTiff", overwrite=TRUE)  
  #calculate economic index
    average.economic = average.intensity * species.price
    writeRaster(average.economic, filename=paste(this.species,"_economic",sep=""), format="GTiff", overwrite=TRUE)  
    
    }
  
  
  }
  
# Now sum all rasters
  
  setwd(pathToSaveShapes)
  
  
    raster.files = list.files(getwd(),  pattern="average.tif$", full.names=FALSE)
    raster.species.comm = stack(raster.files)
    index.intensity = sum(raster.species.comm)
    
    #write raster data
    writeRaster(index.intensity, filename="intensity_index", format="GTiff", overwrite=TRUE)  
    writeRaster(index.intensity, "intensity_index.asc", format="ascii")
    
     
    norm.data = raster("intensity_index.tif") %>% 
      reclassify(cbind(0, NA)) 
    
    #normalize raster 0-1 and save
    norm.data = (index.intensity - cellStats(index.intensity,"min")) / (cellStats(index.intensity,"max")-cellStats(index.intensity,"min"))
    
    norm.data2 = norm.data %>% 
      rasterToPoints %>% 
      data.frame %>% 
      setnames(c('Longitude', 'Latitude', 'Intensity'))
    
    norm.data2[norm.data2 == 0] <- NA
    norm.data2 =  norm.data2[complete.cases(norm.data2),]#eliminate rows with NA
    
    #create color palette
    
    pal <- wes_palette(10, name = "FantasticFox", type = "continuous")
    
    #plot richness model and robustness
    non.plot = ggplot(data=norm.data2, aes(y=Latitude, x=Longitude)) +
      geom_raster(aes(fill=Intensity)) +
      theme_bw() +
      coord_equal() + 
      scale_fill_gradientn(colours = pal)+
      theme(axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12, angle=90),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size=10),
            legend.title = element_text(size=12))
    
    setwd(mainpath)
    #export as png
    png("log_books_intensity.png")
    non.plot
    dev.off()
    
    # Sum all economic rasters
    
    setwd(pathToSaveShapes)
    
    
    raster.files = list.files(getwd(),  pattern="economic.tif$", full.names=FALSE)
    raster.species.comm = stack(raster.files)
    index.intensity = sum(raster.species.comm)
    
    #write raster data
    writeRaster(index.intensity, filename="economic_index", format="GTiff", overwrite=TRUE)  
    writeRaster(index.intensity, "economic_index.asc", format="ascii")
    
    
    norm.data = raster("economic_index.tif") %>% 
      reclassify(cbind(0, NA)) 
    
    #normalize raster 0-1 and save
    norm.data = (index.intensity - cellStats(index.intensity,"min")) / (cellStats(index.intensity,"max")-cellStats(index.intensity,"min"))
    
    norm.data2 = norm.data %>% 
      rasterToPoints %>% 
      data.frame %>% 
      setnames(c('Longitude', 'Latitude', 'Intensity'))
    
    norm.data2[norm.data2 == 0] <- NA
    norm.data2 =  norm.data2[complete.cases(norm.data2),]#eliminate rows with NA
    
    #create color palette
    
    pal <- wes_palette(10, name = "FantasticFox", type = "continuous")
    
    #plot richness model and robustness
    non.plot = ggplot(data=norm.data2, aes(y=Latitude, x=Longitude)) +
      geom_raster(aes(fill=Intensity)) +
      theme_bw() +
      coord_equal() + 
      scale_fill_gradientn(colours = pal)+
      theme(axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12, angle=90),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size=10),
            legend.title = element_text(size=12))
    
    setwd(mainpath)
    #export as png
    png("log_books_economic.png")
    non.plot
    dev.off()