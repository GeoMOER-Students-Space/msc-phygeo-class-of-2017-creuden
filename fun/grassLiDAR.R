
## dem function
dem_grass_lidar <- function(path, 
                            inFN, 
                            outFN,
                            grass_lidar_method,
                            res){
  
ground_raster <- execGRASS("r.in.lidar",
                             input = paste0(path, inFN),
                             output = outFN,
                             flags = c("e", "n", "v", "overwrite","o"),
                             resolution = gridsize,
                             method = grass_lidar_method,
                             class_filter = 2,
                             intern = TRUE,
                             ignore.stderr = TRUE)
  return(ground_raster)
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
  return(medmax)
}
  
heightClasses <- function(input, 
                          flags = c("e", "n", "v", "overwrite","o"),
                          outFN,
                          grass_lidar_method,
                          dem_base,
                          height_class=c(0,100),
                          res) {
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