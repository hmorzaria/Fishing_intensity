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
names_fishers = fread("Fishers_names.txt", header=TRUE) %>% 
  data.frame

#List of locations and acronyms
names_locations = fread("locations.csv", header=TRUE)

#Bitacora_database
bitacoras_data = fread("Datos_bitacoras.txt", header=TRUE)

#Species names
species_data = fread("species_names.csv", header=TRUE)

#Set working directories
setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_original")

mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_original"

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Fishing_intensity/Bitacoras_revisado"

#this is the empty shapefile with for the Northern Gulf

base.file.name = "Atlantis12_AggregatePolygons"

#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

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
  print(paste('Bitacoras database has ',nrow(table.bitacoras), ' rows', sep=""))
  
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
  print(paste('Bitacoras database has ',nrow(table.bitacoras), ' rows', sep=""))
  
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
                        Notes = NA, Coordinates = NA, New_Code= new.shape.file.name,
                        Record = "NO", Duplicate = "NO")
        
        datanames = names(this.file.data)
        setnames(table.bitacoras,datanames)
        table.bitacoras = rbind(table.bitacoras,this.file.data)
        
        print(paste('Bitacoras database has ',nrow(table.bitacoras), ' rows', sep=""))
        
     
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
    
    
    #dir.create(paste(pathToSaveShapes,newcode.name,sep="/"))
    
    #paste(pathToSaveShapes,newcode.name,sep="/") %>% 
      setwd
    
    #writeOGR(shape.new, ".", new.shape.file.name,driver="ESRI Shapefile",overwrite_layer=TRUE)
    
    setwd(path.this.species.folder)    
    
    
    
  }


}

  }

setwd("E:/Archivos/1Archivos/Articulos/En preparacion/Fisheries_intensity")

missing.shapes = bitacoras_data %>% 
  tbl_df%>% anti_join(table.bitacoras, by = "Code")

table.duplicate.codes = table.duplicate.codes %>% mutate(Duplicate="YES",shapefile="YES")

new.missing.shapes = matrix(0,0,40) %>% 
  data.frame

code.list = unique(missing.shapes$Code)


for(each.code in 1:length(code.list))
{
  shape.file.name = code.list[each.code]
  this.file.data= subset(missing.shapes, Code%in% shape.file.name)
  
  if(nrow(this.file.data) > 1)
  {
    print("duplicate entry")
    
  sp.name = this.file.data %>% 
    .$Scientific 
  
  newcode.name = species_data %>% 
    tbl_df %>% filter(Scientific==sp.name) %>% .$NewCode
  
  newcode.name = newcode.name[1] 
  new.shape.file.name = paste((unlist(strsplit(shape.file.name,"-"))[1]),(unlist(strsplit(shape.file.name,"-"))[2]),newcode.name,sep="-")
  
  this.row2 = this.file.data %>%
    tbl_df %>% 
    dplyr::mutate(New_Code = new.shape.file.name,Record='YES',Duplicate = "YES",shapefile= "NO")
 
   new.missing.shapes = rbind(new.missing.shapes,this.row2)
   table.duplicate.codes = rbind(table.duplicate.codes,this.row2)
}
  if(nrow(this.file.data)== 1)
  {
    sp.name = this.file.data %>% 
      .$Scientific 
    
    newcode.name = species_data %>% 
      tbl_df %>% filter(Scientific==sp.name) %>% .$NewCode
    
    newcode.name = newcode.name[1] 
    new.shape.file.name = paste((unlist(strsplit(shape.file.name,"-"))[1]),(unlist(strsplit(shape.file.name,"-"))[2]),newcode.name,sep="-")
    
    this.row2 = this.file.data %>%
      tbl_df %>% 
      dplyr::mutate(New_Code = new.shape.file.name,Record='YES',Duplicate = "NO",shapefile= "NO")
    
    new.missing.shapes = rbind(new.missing.shapes,this.row2)
  }
}
table.bitacoras2 = table.bitacoras %>% 
  mutate(shapefile="YES")

new.table.bitacoras= rbind(table.bitacoras2,new.missing.shapes)

write.csv(new.table.bitacoras, file="new_bitacoras_data.csv")

write.csv(table.duplicate.codes, file="table_duplicate_codes.csv")

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
