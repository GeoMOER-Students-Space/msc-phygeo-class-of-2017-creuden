# some useful hints
setwd("/home/creu/lehre/msc/active/msc-2017/data/gis/input/ref/")
load("0_5_10_15_20_50stat.RData")
chm<-statLayer[[2]][[3]]

# OPT 1: read it with sf and convert it to sp 
# (NOTE a lot of packages are still lacking to support sf extremly powerful - the future!)
plot2 <- as(sf::st_read("plot2.shp"),"Spatial")
# OPT 2: read it with rgdal classic most common and stable way
plot2 <- rgdal::readOGR(".","plot2")
# OPT 3: read reference shapefile with raster package (most simple way)
plot2<-raster::shapefile("plot2.shp")

# read example polygons from working directory (look in email)
segmentation_example<-readRDS("segementation.rds")

### projection basics
#--> projection string 
projString <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# project plot2
plot_utm <- sp::spTransform(plot2,sp::CRS(projString))

# write classical shape with rgdal
rgdal::writeOGR(obj = plot_utm,
                layer = "plot_utm", 
                driver = "ESRI Shapefile", 
                dsn = ".", 
                overwrite_layer = TRUE)
# write geojson via sp to sf  it keeps long attribute names utf8 stuff etc. 
# NOTE much much much better than shape nevetheless more complex ...
# NOTE it is forces to 4236 projection
sf::st_write(sf::st_as_sf(plot_utm), "plot_utm.geojson",delete_dsn=TRUE,driver="GeoJSON")
# read it back and reagsing proj4 because geojson had forced  projstr but not the coordinates to +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
sf_plot_utm <- as(sf::st_read("plot_utm.geojson"),"Spatial")
# reassign correct projection
sf_plot_utm@proj4string <-sp::CRS(projString)

#view it
mapview::mapview(sf_plot_utm)

# now clip area with segmentation result using rgeos package
cuttedArea <- rgeos::gIntersection(segmentation_example,
                                   sf_plot_utm,
                                   byid = TRUE)
# uups differnt proj string so reassign it 
segmentation_example@proj4string<-sp::CRS(projString)

cuttedArea <- rgeos::gIntersection(segmentation_example,
                                   sf_plot_utm,
                                   byid = TRUE)

#view it 
mapview::mapview(cuttedArea,col.regions="blue",col.alpha=0.3)  +
mapview::mapview(sf_plot_utm,col.regions="green")


