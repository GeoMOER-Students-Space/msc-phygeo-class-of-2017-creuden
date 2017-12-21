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
# --- Basic idea is to set up a static working environment starting a an 
# --- arbitray point in your folder structure called "projDir". Additionally
# --- you need to provide a so called "rootDir" which should be an subfolder 
# --- of the "projDir" folder. 
# --- Within the classes you have to provide the "courseCode" (gi,rs,da) and the 
# --- "activeSessionFolder"  wich is the number of the current class session
# --- All reusuable functionallity like setup procedure formulas wrappers etc.
# --- is shared by sourced functions. The current script acts as main control script

# --- That's it

#--> library requirements
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE)
library(link2GI)
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

# start preprocessing to correct las files
correctLas = TRUE
#--> create plots
plotIt <- TRUE

#--> create complete rootDir string
rootDir<-paste0(projDir,rootDir)

#--> all setup tasks are executed by the AAAsetupProj.R script
#--> if necessary you may adapt it to your needs
#--> make a list of all functions in the corresponding function folder and source these functions
res<- sapply(list.files(pattern="[.]R$",path=paste0(rootDir,"/fun"),full.names=TRUE),FUN=source)



### ---------------------------- Thematic  Settings ----------------------------

#--> Create a list containing the las files in the input folder
if (correctLas){
  cat("\n: correcting las files...\n")
  lasfiles<-list.files(paste0(gi_input),pattern=".las$", full.names=FALSE) 
  cat(":: rescaling las files...\n")
  lasTool("rescale",paste0(gi_input, lasfiles[j]))
  cat(":: reducing overlap patterns...\n")
  lasTool("lasoverage",paste0(gi_input, lasfiles[j],"_fixed.laz"))
  lasfiles<-list.files(paste0(gi_input),pattern="_lapcor.las$", full.names=FALSE) 
} else {
  #lasfiles<-list.files(paste0(gi_input),pattern=".las$", full.names=FALSE) 
  lasfiles<-list.files(paste0(gi_input),pattern="_lapcor.las$", full.names=FALSE) 
}


#--> list of height pairs as used for slicing the las data
zrList <- list(c(0,5,10,15,20,50))
zrange<- makenames(zrList)[[2]]
zrnames<- makenames(zrList)[[1]]


#--> list of statistic calculation for more info see r.in.lidar help
statList <- list("max", "median","range")

#--> target grid sizec(0,5,10,15,20,50)
gridsize <- 10

#--> projection string 
proj4="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

### ---------------------------- here we go ----------------------------

# NOTE the used GRASS modules are wrapped using the rgrass7. they can be found as single functions 
#      in the file grassLiDAR.R file. The main control structure is written below as a mixture of function 
#      calls and generic R code
#      
cat(": starting calculations...\n")
# define some list variables
m_vdr <- m_fhd <-vdr <- fhd <- zrLayer <- statLayer <- list() 

# for each existing las file do
for (j in 1:(length(lasfiles))) {

  # create *temporyry* GRASS location
  ext<-lasTool(lasDir = paste0(gi_input, lasfiles[j]))
  result<-link2GI::linkGRASS7(spatial_params = c(ext[2],ext[1],ext[4],ext[3],proj4),resolution = gridsize)

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
               class_filter = 13,
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
  # all stat layers
  statLayer[[j]] <- stack(lapply(statList ,
                            function(x){raster::raster(rgrass7::readRAST(paste0(x,"_veg"),NODATA=-9999))})
                          )
  
  # calculate indices
  # FHD foliage height density  -> diversityindeces.R
  fhd[[j]]<- fun_fhd(zrLayer[[j]])
  
  # VDR vertical density ratio -> diversityindeces.R
  vdr[[j]]<- fun_vdr(statLayer[[j]][[3]],statLayer[[j]][[2]])
  
  # if plot is true plot them
  if (plotIt) {
    plot(zrLayer[[j]])
    plot(statLayer[[j]])
    plot(fhd[[j]],  col=rev(heat.colors(10)),main="FHD Index")
    plot(vdr[[j]],  col=rev(heat.colors(10)),main="VDR Index")
    
    # create mapview objects
    # m_fhd[[j]]<- mapview::mapview(fhd[[j]],
    #                  legend = TRUE,
    #                  alpha.regions = 0.3,
    #                  layer.name="FHD Index")
    # 
    # m_vdr[[j]]<-mapview::mapview(vdr[[j]],
    #                  legend = TRUE,
    #                  alpha.regions = 0.3,
    #                  layer.name="VDR Index")
  }
}  
  
  # create names according to inputdata and methods/ranges
  
  mn<-paste( unlist(statList), collapse='_')
  zrn<-paste( unlist(zrList), collapse='_')
  lsf<-paste( unlist(tools::file_path_sans_ext(lasfiles)), collapse='_')
  # save the results
  save(zrLayer,file = paste0(gi_output,zrn,lsf,".RData"))
  save(statLayer,file = paste0(gi_output,zrn,mn,".RData"))
  # save the results
  save(fhd,file = paste0(gi_output,zrn,lsf,"_fhd",".RData"))
  save(vdr,file = paste0(gi_output,zrn,mn,"_vdr",".RData"))
  
  cat("save results to: " ,paste0(gi_output,zrn,lsf,".RData\n"))
  cat("save results to: " ,paste0(gi_output,zrn,mn,".RData\n"))
  cat("save results to: " ,paste0(gi_output,zrn,lsf,"_fhd.RData\n"))
  cat("save results to: " ,paste0(gi_output,zrn,mn,"_vdr.RData\n"))
  cat("\nfinished")

  