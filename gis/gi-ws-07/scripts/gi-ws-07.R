# --- gi-ws-07 example control script 
# --- MOC - Advanced GIS 
# --- setup working environment 
# --- setup basic GI API links
# --- provides some LiDAR functionality 
# --- calculate some basic biodiversity indices
#   Things to do  
#   - Basics
#      - please check and realize the source code
#      - if everything is comparable why we have so different results 
#      - find the reason and eleminate it 
#   - Advanced
#      - implement one more diversity index and calculate the 
#        already implemented vertical distribution ratio (vdr)
#
#  NOTE  you will find some weird settings in these scripts most
#        are dealing with platform incompatibility. It may occure
#        that even generic windows tools like fusion or lastools 
#        will NOT run under Windows I just tested the emulation 
#        on Linux platforms
#        Whenever you run in trouble open an issue
#        https://github.com/logmoc/msc-phygeo-class-of-2017-creuden/issues
#        describe the problem with source code and add your sessionInfo() output
#
# --- see also: https://github.com/logmoc/msc-phygeo-class-of-2017-creuden
# ---
# --- Basic idea is to set up a static working environment starting a an 
# --- arbitray point in your folder structure called "projDir". Additionally
# --- you need to provide a so called "rootDir" which should be an subfolder 
# --- of the "projDir" folder. 
# --- Within the classes you have to provide the "courseCode" and the 
# --- "activeSessionFolder"  wich is the number of the current class session
# --- Finally if you use GRASS you should provide a valid georefrenced File - 
# --- preferably a geotif of the area you want to work at
# --- That's it
### -------------------------- setup the environment --------------------------

#--> library requirements
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE)
require(gdalUtils)
require(rgrass7)
require(raster)
require(mapview)


#--> NOTE point to whereever you want but avoid strange letters as dots etc
#--> the ~ is a substitute for the system variable HOME
#--> projDir is general project folder  basic folder eg. C:/Dokumente/1_semester_MSCGEO/GIS/
projDir<-"~/lehre/msc/active/msc-2017/"
#-->  rootFolder of the github repository 
rootDir<-"msc-phygeo-class-of-2017-creuden"

#--> current class
courseCode<-"gi"
#--> current class session folder
activeSessionFolder<-7
#-->  input file for GRASS initiaalisation
inputFile<- "geonode-lidar_dem_01m.tif"

### ------------------------- end basic pathes and settings -------------------
### ------------------
### ---------

#--> optionally convert laz to las
laz<-FALSE
#--> create full rootDir
rootDir<-paste0(projDir,rootDir)
#--> make a list of all functions in the corresponding function folder and source these functions
res<- sapply(list.files(pattern="[.]R$",path=paste0(rootDir,"/fun"),full.names=TRUE),FUN=source)

# create project structure and export global pathes
#--> create/add new folder(s) 
createMocFolders(rootDir, 
                 ccourse = "gi", 
                 csess = activeSessionFolder
                 )
#--> create *global* path variables for the current session (they won't work in doParallel or foreach)
getSessionPathes(filepath_git = rootDir, 
                 filepath_data = paste0(projDir,"data"),
                 sessNo = activeSessionFolder,
                 courseCode = courseCode,
                 dataWorkingFolder=c("run/","input/","output/","GRASS7/","GRASS7"))

# create GRASS7 link
#--> for a straightforward initialisation of GRASS you need to provide an referenced input file
grass<- link2GI::linkGRASS7(x = paste0(gi_input,inputFile),
                            gisdbase = paste0(projDir,"data/gis/GRASS7"), 
                            location = "OFM")

# check GDAL binaries and start gdalUtils
gdal<- link2GI::linkgdalUtils()


#--> set working directory (just if you missed using golbal path variables as a backup)
setwd(gi_run)
if (laz){
  lazfiles<-list.files(gi_input, pattern=".laz$", full.names=TRUE,recursive = TRUE) 
  lasTool(  tool="las2las",dirname(lazfiles)[1])
}

### ------------------------- end environment setup --------------------------
### ------------------
### ------------------
### ---------------------------- Thematic  Settings ----------------------------
# Before starting it makes sense to focus the goal:
#   - we want to calculate some diversity indices based on LiDAR data
#   - we need to identify the indices lets say FHD and VDR
#       * what kind of information as derived by the data do we need?
# + we need to strip the terrain altitudes i.e. normalize/reduce the point cloud data 
#         + FHD all returns and a defined number of horizontally sliced returns
#         + VDR max and median returns
#       * what is technically required?
#         + we need to deal with a bunch of data files in a row  
#         + we might deal with more indices so it would fine to split preprocessing from calulation
#
#--> Create a list containing the las files in the input folder
lasfiles<-list.files(gi_input, pattern=".las$", full.names=FALSE) 

#--> use the GRASS way
useGRASS <- TRUE

#--> use the FUSION way
useFusion <- FALSE

#--> list of height pairs as used for slicing the las data
heightClassList <- list(c(0,5), c(5,10), c(10,15), c(15,20), c(20,50), c(0,50))

#--> list of statistic analysis 
statList <- list("max", "median")

# target grid size
gridsize <- 10

### ---------------------------- here we go ----------------------------




#--> do it the Fusion way
# NOTE the fusion algorithm is completly coded in the function fu_sliceRas()
#      it returns a raster stack with the necessary data for FHD index calculation  
#      The Fusion stuff is only tested under Linux. Ironically it may NOT run under Windows
#      you may change manually the paramList, only the target gridsize is automatically implemented

if (useFusion) {
  #--> dirty workaround for platform indepentend  use of fusion
  source(paste0(fun,"controlFusion.txt"))
  
  # call fusion based slicing funtion
  fuStack <- fu_sliceRas(lasFiles = lasfiles,
                         heights = heightClassList, 
                         paramList = c("10 M M 1 32 0 0 "),
                         res = gridsize)
  plot(fuStack)
  #--> FHD using the fun_fhd function  provided in diversityindeces.R
  fhd_fu<- fun_fhd(fuStack)
  
  spplot(fhd_fu, scales = list(draw = TRUE), 
         xlab = "easting", ylab = "northing", 
         col.regions = heat.colors(99), 
         names.attr=c('original', 'times two'))
  
  }


#--> do it the GRASS way
# NOTE the used GRASS modules are wrapped using the rgrass7. they can be found as single functions 
#      in the file grassLiDAR.R file. The control structure is written below as a mixture of function 
#      calls and generic R code
#      
if (useGRASS){
  
  #--> basic calculation of DEM from las data set 
  # NOTE: standard aproach would be to use the class2 values unfortunately there will be A LOT
  #       of NA values. second best simple methodis to use the "min" function
  for (j in 1:(length(lasfiles))){

    dem_grass_lidar(path = gi_input, 
                    inFN = lasfiles[j], 
                    outFN = paste0(j, "_dem"),
                    grass_lidar_method = "min",
                    res = gridsize)
    
    #--> slice the data horizontally according to the provide gi_input = NULL, gi_input = NULL,d heightClassList
    #      Reduction to a base raster is applied
    for (i in heightClassList){
      heightClasses(input = paste0(gi_input,lasfiles[j]), 
                    outFN = paste0("class",i[1],i[2]),
                    grass_lidar_method = "n",
                    dem_base = paste0(j, "_dem"),
                    height_class = i,
                    res = gridsize)
    }
    
    for (k in statList ){
    medmax(input = paste0(gi_input,lasfiles[j]), 
           outFN = paste0(k,"_veg"),
           grass_lidar_method = k,
           dem_base =  paste0(j, "_dem"),
           res = gridsize)
    }
    
    ###----> import data to R
    
    #-->  read and stack the data according to the GRASS file names as generated by the heightClassList 
    # using an lapply approach which iterates over the list and returns a list of raster wich is stacked 
    sliceStack <- stack(lapply(heightClassList ,
                                  function(x){raster::raster(rgrass7::readRAST(paste0("class",x[1],x[2])))
                                  }
    )
    )

    statStack <- stack(lapply(statList ,
                                  function(x){raster::raster(rgrass7::readRAST(paste0(x,"_veg")))
                                  }
    )
    )
    plot(sliceStack)
    plot(statStack)
    
    #ras<-raster(rgrass7::readRAST(paste0(j,"maxveg")))
    ###----> Calculate indices
    
    #--> FHD using the fun_fhd function  provided in diversityindeces.R
    fhd_grass<- fun_fhd(sliceStack)
    
    #--> FHD using the fun_fhd function  provided in diversityindeces.R
    vdr_grass<- fun_vdr(statStack[[1]],statStack[[2]])
    
    spplot(fhd_grass, scales = list(draw = TRUE), 
           xlab = "easting", ylab = "northing", 
           col.regions = heat.colors(99), 
           names.attr=c('original', 'times two'))
    
        
    spplot(vdr_grass, scales = list(draw = TRUE), 
           xlab = "easting", ylab = "northing", 
           col.regions = heat.colors(99), 
           names.attr=c('original', 'times two'))
           
  }  
}

