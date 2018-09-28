#' @title complexTrace
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Outlines each unique element in a \emph{RasterLayer} and returns a \emph{SpatialPolygons} object.
#' @param x Object of class \emph{RasterLayer}.
#' @return A \emph{SpatialPolygons} object.
#' @importFrom raster crs cellStats
#' @importFrom sp Polygon Polygons SpatialPolygons
#' @importFrom grDevices chull
#' @details {builds a \emph{SpatialPolygons} object defined as the convex hull of a set of coordinate pairs provided through \emph{x}.}
#' @examples {
#' 
#' # read reference data
#' data(fieldData)
#' 
#' # extract centroids for each polygon
#' cp <- spCentroid(fieldData)
#' 
#' # build single polygon from centroid points
#' p <- simpleTrace(cp)
#' 
#' # compare objects
#' plot(p)
#' points(cp, col="red")
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

complexTrace <- function(x) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (class(x)[1] != "RasterLayer") {stop('"x" is not of a valid class')}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. sort tracking
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  w <- (w*2)+1 # update window
  
  x0 <- extend(x, w) # pad image
  x0 <- disaggregate(aggregate(x0, fact=2, max, na.rm=TRUE), fact=w) > 0 # aggregate image (fills gaps)
  p <- rasterToPolygons(crop(x0,x), fun=function(i) {i == 1}, na.rm=TRUE, dissolve=TRUE) # derive polzgon
  
  return(p)
  
}
