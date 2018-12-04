#' @title smoothTrace
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Outlines each unique element in a \emph{RasterLayer} and returns a \emph{SpatialPolygons} object.
#' @param x Object of class \emph{RasterLayer}.
#' @param f Object of class \emph{numeric}.
#' @return A \emph{SpatialPolygons} object.
#' @importFrom raster crs cellStats
#' @importFrom sp Polygon Polygons SpatialPolygons
#' @details {Aggregates \emph{x} to a lower spatial resolution, with a factor defined by \emph{f}, an disaggregates 
#' the output back to the native resolution. Finally, the function crops the output using the extent of the non-NA 
#' valuesin \emph{x} and derives a \emph{SpatialPolygons} object.}
#' @examples {
#' 
#' # read reference data
#' data(fieldData)
#' 
#' # extract centroids for each polygon
#' cp <- spCentroid(fieldData)
#' 
#' # build single polygon from centroid points
#' p1 <- simpleTrace(cp)
#' 
#' p2 <- smoothTrace(rasterize(p, raster(extent(p), res=30, crs=crs(p))), 1)
#' 
#' # compare objects
#' plot(p1)
#' points(cp, col="red")
#' plot(p2, add=TRUE, border="blue")
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

smoothTrace <- function(x, f) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (class(x)[1] != "RasterLayer") {stop('"x" is not of a valid class')}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. sort tracking
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  w <- (w*2)+1 # update window
  
  x0 <- extend(x, w) # pad image
  x0 <- disaggregate(aggregate(x0, fact=f, max, na.rm=TRUE), fact=w) > 0 # aggregate image (fills gaps)
  p <- rasterToPolygons(crop(x0,x), fun=function(i) {i == 1}, na.rm=TRUE, dissolve=TRUE) # derive polzgon
  
  return(p)
  
}
