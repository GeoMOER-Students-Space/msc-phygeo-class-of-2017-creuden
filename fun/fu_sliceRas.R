#'@name fu_sliceRas
#'@title Create a raster* object from a LiDAR generated point cloud 
#'
#'@description
#' Create a raster* object from a LiDAR generated point cloud based on regular las format files
#'
#'@author Chris Reudenbach,Jannis Gottwald
#'
#'@param lasFiles  default is \code{NULL} list of the las file(s)
#'@param heights default is \code{list(c(0,3), c(3,15))} list vector pairs describing the vertical slice 
#'@param paramList default is \code{c("1 M M 1 32 0 0 ")}list of parameters for info see clipdata.exe manual 
#'@param res resolution for raster operations 
#'@param proj4  any valid proj4 string that is assumingly the correct one default is \code{"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"}
#'
#'@examples
#'\dontrun{
#' # create a DSM based on a uav point cloud 
#' r<-lasSlice(lasFiles = lasfiles)
#'}
#'


fu_sliceRas<- function(lasFiles = NULL,
                                    heights =  list(c(0,3), c(3,15)), 
                                    paramList=c("1 M M 1 32 0 0 "),
                                    res= 10,
                                    proj4 = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                                    ) {
  
  paramList <- c(paste(as.character(gridsize),"M M 1 32 0 0 "))
  density<-stack()
  for (i in 1:length(lasFiles)) {
    ### --> retrieve infomation from las file
    
    # calculate orfeo HaralickTextureExtraction
    
    #--> Fusion catalog 
    command<-Fusion
    command<-paste0(command, "catalog.exe")
    command<-paste0(command," ", gi_run, basename(lasFiles[i]) )
    command<-paste0(command," ", gi_output, i,".html"   )
    system(command)  
    #--> extract extent info 
    info <- strsplit(readLines(paste0(gi_output,i,".csv"),encoding = "utf-8"),split = ",")
    #TODO  fix error in las files if (as.numeric(info[[2]][3])) fixLas()
    #--> define extent for further calculation
    extent<-paste(as.numeric(info[[2]][3]),as.numeric(info[[2]][4]),as.numeric(info[[2]][6]),as.numeric(info[[2]][7]))
    ext<-c(as.numeric(info[[2]][3]),as.numeric(info[[2]][4]),as.numeric(info[[2]][6]),as.numeric(info[[2]][7]))
    
    #--> Create a .las with groundpoints only 
    command<-Fusion
    command<-paste0(command, "clipdata.exe")
    command<-paste0(command," ","/class:2 ")
    command<-paste0(command," ", gi_run, basename(lasFiles[i])   )
    command<-paste0(command," ", gi_run,"ground_",basename(lasFiles[i])   )
    command<-paste0(command," ", extent)
    system(command)  
    
    
    #--> Create the required PLANS DTM format 
    command<-Fusion
    command<-paste0(command, "gridsurfacecreate.exe")
    command<-paste0(command," ", gi_run,"surf_",basename(lasFiles[i]),".dtm")
    command<-paste0(command," ", paramList  )
    command<-paste0(command," ", gi_run,"ground_",basename(lasFiles[i])   )
    system(command)  
    
    for (j in 1:length(heights)){
      #--> Create a a horizontally sliced las file which is reduced by the DEM
      command<-Fusion
      command<-paste0(command,"clipdata.exe")
      command<-paste0(command," ", "/zmin:",heights[[j]][1])
      command<-paste0(command," ", "/zmax:",heights[[j]][2])
      command<-paste0(command," ", "/height")
      command<-paste0(command," ", "/dtm:",gi_run,"surf_",basename(lasFiles[i]),".dtm")
      command<-paste0(command," ", "/ground")
      command<-paste0(command," ", gi_run, basename(lasFiles[i]))
      command<-paste0(command," ", gi_run,"normalised_",heights[[j]][1],"_",heights[[j]][2],"_",basename(lasFiles[i]))
      command<-paste0(command," ", extent)
      system(command)  
      
      
      ### NOTE Linux you need to install install winetricks mfc42
      #--> create the return density (that means counts per area)
      command<-Fusion
      command<-paste0(command, "returndensity.exe")
      command<-paste0(command," ", "/ascii")
      command<-paste0(command," ", gi_run,heights[[j]][1],"_",heights[[j]][2],"_density.asc "  )
      command<-paste0(command," ", res   )
      command<-paste0(command," ", gi_run,"normalised_",heights[[j]][1],"_",heights[[j]][2],"_",basename(lasFiles[i])   )
      system(command)  
      
      ### NOTE ###
      #--> Workaround of misreferencing the raster files belonging to one  stack
      #    -> rasterize file if more than one resample it to the first one 
      r <- raster(paste0(gi_run,heights[[j]][1],"_",heights[[j]][2],"_density.asc"),quick=T)
      if (j>1) r <- raster::resample(r, rold, method = 'bilinear')
      rold<-r
      
      #--> stack the file
      density<-raster::stack(density,r,quick=T)
    }
  }
  #--> set correct extent amd projection
  raster::setExtent(density,ext)
  crs(density)<- proj4
  return(density)
}


