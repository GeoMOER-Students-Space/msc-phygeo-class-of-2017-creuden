
## creates a dem according to the class 2  and the scan modus a raster file from the lidar data
dem_grass_lidar <- function(path, 
                            inFN, 
                            outFN,
                            grass_lidar_method,
                            res,
                            class=2
                            ) {
  
  
  
  ground_raster <- execGRASS("r.in.lidar",
                             input = paste0(path, inFN),
                             output = outFN,
                             flags = c("e", "n", "v", "overwrite","o"),
                             resolution = res,
                             method = grass_lidar_method,
                             class_filter = class,
                             intern = FALSE,
                             ignore.stderr = TRUE)
  
}


medmax <- function(input, 
                          flags = c("e", "n", "v", "overwrite","o"),
                          outFN,
                          grass_lidar_method,
                          dem_base,
                          res) {

  
  medmax <- execGRASS("r.in.lidar",
                             input = input,
                             output = outFN,
                             flags = flags,
                             resolution = res,
                             method = grass_lidar_method,
                             base_raster = dem_base,
        
                             intern = FALSE,
                             ignore.stderr = TRUE)
  
}
  
heightClasses <- function(input, 
                          flags = c("e", "n", "v", "overwrite","o"),
                          outFN,
                          grass_lidar_method,
                          dem_base,
                          height_class=c(0,100),
                          res){
  
  heightClasses <- execGRASS("r.in.lidar",
                             input = input,
                             output = outFN,
                             flags = flags,
                             resolution = res,
                             method = grass_lidar_method,
                             base_raster = dem_base,
                             zrange = height_class,
                             intern = FALSE,
                             ignore.stderr = TRUE)
  return(heightClasses)
}