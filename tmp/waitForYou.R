#' I do not do anything except you tell me
#' @description is exactly for describing what I am doing. You may use formatting tags like \cr 
#' for carriage return, \code{"CODE"} for formatting code, \href<-{"http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip"}{Download this DGM} for refernces
#' @note is used for special remarks
#' @author Your Name
#' 
#' @param param1 an object of raster for doing this and that default is blablabla Note the following
#' @param param2 logical switch for blabla
#' @param param2 list character for blabla

#' @export waitforYou
#' @examples 
#' \dontrun{
#'  waiting <- waitForYou(param1 = NULL, 
#'                        param2 = TRUE,
#'                        param3 = c("A", "B", "C")
#' }

waitForYou <- function(param1 = NULL, 
                       param2 = TRUE,
                       param3 = c("waypoints", "tracks", "routes", "track_points", "route_points")) {
                         # note it is obligatory to call all function via namespace that means 
                         raster::raster(param1)
                         # not 
                         raster(param1)
                         # check if cat is happy
                         if (!all(param3 %in% c("A", "B", "C"))) stop("ABC Katze sitzt im Schnee")
                             SPDF<- treecrowns
                             return(SPDF)
                       }
                       