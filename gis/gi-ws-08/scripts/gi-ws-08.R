# --- gi-ws-07-1 example control script GRASS/lastool
# --- MOC - Advanced GIS 
# --- setup working environment 
# --- setup basic GI API links
# --- provides some LiDAR functionality 
# --- calculate some basic biodiversity indices
#
#  NOTE  It may occure that generic windows tools like fusion or lastools 
#        will NOT run under Windows most reasonable because I just tested 
#        the wine emulation on Linux platforms...
#
#        Whenever you run in trouble open an issue
#        https://github.com/logmoc/msc-phygeo-class-of-2017-creuden/issues
#        describe the problem with source code and add your sessionInfo() output
#
#  Codebase: https://github.com/logmoc/msc-phygeo-class-of-2017-creuden
# 
#
### -------------------------- setup the environment --------------------------

# --- Basic idea is to set up a a arbitrary folder ("rootDir") as a root point for 
# --- fixed subfolder structure. The project subfolder of the rootDir path is assigned
# --- using the variable "projDir".
# --- Within the courses you have to provide the "courseCode" ("gi","rs","da") and the 
# --- "activeSessionFolder" (1..15) wich should be the number of the class session you 
# --- like to work in.
# --- All functions like the setup procedure, formulas, wrappers etc. will be stored
# --- in the folder file.path(rootDir,projdir,"fun") and will be automatically sourced.

# --- The current script acts as main control script 

# --- That's it

#--> library requirements
rm(list =ls())
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE)
library(link2GI)
require(gdalUtils)
require(rgrass7)
require(raster)
require(mapview)

#--> NOTE point to whereever you want but avoid strange letters as dots etc
#--> the ~ is a substitute for the system variable HOME

#--> rootDir is general project folder  basic folder eg. C:/Dokumente/1_semester_MSCGEO/GIS/
if (Sys.info()["sysname"] == "Windows"){
  rootDir<-"e:/R_proj/teaching/"
} else {
  rootDir<-"~/lehre/msc/active/msc-2017/"
}

#-->  rootFolder of the github repository 
projDir<-"msc-phygeo-class-of-2017-creuden"
#--> current class
courseCode<-"gi"
#--> current class session folder
activeSessionFolder<-8

# start preprocessing to correct las files
correctLas = TRUE
#--> create plots
plotIt <- TRUE

#--> projection string 
proj4="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#--> create complete projDir string
projfolder<-file.path(rootDir,projDir,.Platform$file.sep,fsep = .Platform$file.sep)

#--> all setup tasks are executed by the AAAsetupProj.R script
#--> if necessary you may adapt it to your needs
#--> make a list of all functions in the corresponding function folder and source these functions
res<- sapply(list.files(pattern="[.]R$",path=paste0(projfolder,"/fun"),full.names=TRUE),FUN=source)




### ---------------------------- Thematic  Settings ----------------------------

#    0) basic correction of las files TODO put it in a preprocessing step
#    1) reducing the oversampling of counts using lasoverage
#    2) rescaling of las files to a 1 cm resolution
#    3) TODO set projection with https://www.liblas.org/utilities/las2las.html 
#       (needs some adaption due to naming conflicts)
#       las2las -i in.las -o out.laz -utm 32N -vertical_wgs84
# NOTE no correction of broken extents is performed this will be done during runtime
#

if (correctLas){
  cat("\n: correcting las files...\n")
  lasfiles<-list.files(paste0(gi_input),pattern=".las$", full.names=FALSE) 
  for (j in 1:(length(lasfiles))) {
    cat(":: check extent patterns...\n")
    ext<-lasTool(lasDir = paste0(gi_input, lasfiles[j]))
    link2GI::linkGRASS7(search_path = searchPathGrass,spatial_params = c(ext[2],ext[1],ext[4],ext[3],"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    m<-try(execGRASS("r.in.lidar",
                     input = paste0(gi_input, lasfiles[j]),
                     output = "tst",
                     flags = c("e","n","overwrite","o","v"),
                     resolution = 10,
                     method = "min",
                     class_filter = 2,
                     echoCmd=FALSE,
                     intern = TRUE,
                     ignore.stderr = FALSE))
    if (!class(m)=="try-error") {
    cat(":: reducing overlap patterns...\n")
    lasTool("lasoverage",paste0(gi_input, lasfiles[j]))
    cat(":: rescaling las files...\n")
    lasTool("rescale",paste0(gi_input, lasfiles[j],"_lopcor.las"))
    } else {
      file.rename(paste0(gi_input, lasfiles[j]),paste0(gi_input, lasfiles[j],"_lopcor.las_fixed.las"))
    }
    # getting the new las file list
    
  }
  correctLas = TRUE
  lasfiles<-list.files(paste0(gi_input),pattern="_lopcor.las_fixed.las$", full.names=FALSE) 
} else {

  lasfiles<-list.files(paste0(gi_input),pattern="_lopcor.las_fixed.las$", full.names=FALSE) 
}

# for corrected las files classFilter has to be 13 
# if running uncorrected lasfiles set it to 2
classFilter<-13

#--> list of height pairs as used for slicing the las data
zrList <- list(c(0,5,10,15,20,50))
zrange<- makenames(zrList)[[2]]
zrnames<- makenames(zrList)[[1]]


#--> list of statistic calculation for more info see r.in.lidar help
statList <- list("max", "median","range")

#--> target grid sizec(0,5,10,15,20,50)
gridsize <- 1
# horizontal aggregation level
focalSize <- 3
#--> projection string 
proj4="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

### ---------------------------- here we go ----------------------------

# NOTE the used GRASS modules are wrapped using the rgrass7. they can be found as single functions 
#      in the file grassLiDAR.R file. The main control structure is written below as a mixture of function 
#      calls and generic R code
#      
cat(": starting calculations...\n")
# define some list variables
m_vdr <- m_fhd <-vdr <- fhd <- zrLayer <- statLayer <- hdl1<- hdl2<- gapl<- list() 

# for each existing las file do
for (j in 1:(length(lasfiles))) {

  # create *temporyry* GRASS location
  ext<-lasTool(lasDir = paste0(gi_input, lasfiles[j]))
  link2GI::linkGRASS7(search_path = searchPathGrass,spatial_params = c(ext[2],ext[1],ext[4],ext[3],proj4),resolution = gridsize)

  # create straightforward dem 
  r_in_lidar(input = paste0(gi_input,lasfiles[j]), 
             output = paste0("dem_",j),
             method = "min",
             resolution = gridsize,
             class_filter = 2,
             flags = c("e","n","overwrite","o","v"))

  # fill data gaps 
  fillGaps(gi_input,paste0("dem_",j))

  # for each height break do
  for ( i in 1:(length(zrnames))) {

    # slice las file according to height breaks and reduce the altitude by subtracting the base raster
    # NOTE the v flag allows only for valid points
    r_in_lidar(input = paste0(gi_input,lasfiles[j]), 
               output = zrnames[i],
               method = "n",
               base_raster = paste0("dem_",j),
               zrange = c(zrange[[i]][1],zrange[[i]][2]),
               class_filter = classFilter,
               resolution=gridsize,
               flags = c("d","overwrite","o","v")
    )
    # fill data gaps 
    fillGaps(gi_input,zrnames[i])
  }
  
  # calculate the statistics as given  by the statList
  for (meth in statList ){
    r_in_lidar(input = paste0(gi_input,lasfiles[j]), 
               output = paste0(meth,"_veg"),
               method = meth,
               base_raster = paste0("dem_",j),
               resolution = gridsize,
               flags = c("d","overwrite","o","v")
    )
    fillGaps(gi_input,paste0(meth,"_veg"))
    }

  
  # import and stack the data from GRASS back to R
  # all height layers
  zrLayer[[j]] <- stack(lapply(zrnames ,
                              function(x){raster::raster(rgrass7::readRAST(x))})
                        )
  
  test <- sum(stack(lapply(zrnames[2:5] ,
                           function(x){raster::raster(rgrass7::readRAST(x))})
  ))
  
  # all stat layers
  statLayer[[j]] <- stack(lapply(statList ,
                            function(x){raster::raster(rgrass7::readRAST(paste0(x,"_veg"),NODATA=-9999))})
                          )
  cat(": starting calculation of indices...\n")
  # calculate indices
  # FHD foliage height density  -> diversityindeces.R
  fhd[[j]]<- fun_fhd(zrLayer[[j]])
  
  # VDR vertical density ratio -> diversityindeces.R
  vdr[[j]]<- fun_vdr(statLayer[[j]][[3]],statLayer[[j]][[2]])

  # cat(": starting calculation of horizontal filtering ... \n")
  # horizontal diversity layer 1
  #hdl1[[j]]<-(focal(fhd[[j]],w=matrix(1/focalSize*focalSize,nrow = focalSize,ncol = focalSize),na.rm=T,fun=var))
  
  
  # horizontal diversity layer 2
  #hdl2[[j]]<-(stack(lapply(unstack(statLayer[[j]]),
  #                             function(x){focal(x,w=matrix(1/focalSize*focalSize,nrow = focalSize,ncol = focalSize),na.rm=T,fun=var)})))
  # Mode<-function(x) {
  #   ux <- unique(x)
  #   ux[which.max(tabulate(match(x, ux)))]
  # }
  # # gap layer
  # gapl[[j]]<-focal(statLayer[[j]][[1]],w=matrix(1/focalSize*focalSize,nrow = focalSize,ncol = focalSize),Mode)
  # 
  # # reclass
  # gapl[[j]][gapl[[j]]> 3]<-0
  
    
  # if plot is true plot them
  if (plotIt) {
    cat(": starting plotting ... \n")
    plot(zrLayer[[j]])
    plot(statLayer[[j]])
    plot(fhd[[j]],  col=rev(heat.colors(10)),main="FHD Index")
    plot(vdr[[j]],  col=rev(heat.colors(10)),main="VDR Index")
    
    #create mapview objects
    m_fhd[[j]]<- mapview::mapview(fhd[[j]],
                     legend = TRUE,
                     alpha.regions = 0.6,
                     layer.name="FHD Index")
     
    m_vdr[[j]]<-mapview::mapview(vdr[[j]],
                     legend = TRUE,
                     alpha.regions = 0.6,
                     layer.name="VDR Index")
  }
}  
  
  # create names according to inputdata and methods/ranges
  
  mn<-paste( unlist(statList), collapse='_')
  zrn<-paste( unlist(zrList), collapse='_')
  lsf<-paste( unlist(tools::file_path_sans_ext(lasfiles)), collapse='_')
  # save the results
  save(zrLayer,file = paste0(gi_output,zrn,"horizon.RData"))
  save(statLayer,file = paste0(gi_output,zrn,"stat.RData"))
  # save the results
  save(fhd,file = paste0(gi_output,zrn,"_fhd",".RData"))
  save(vdr,file = paste0(gi_output,zrn,"_vdr",".RData"))
  
  cat("save results to: " ,paste0(gi_output,zrn,".RData\n"))
  cat("save results to: " ,paste0(gi_output,zrn,".RData\n"))
  cat("save results to: " ,paste0(gi_output,zrn,"_fhd.RData\n"))
  cat("save results to: " ,paste0(gi_output,zrn,"_vdr.RData\n"))
  cat("\nfinished")

  
