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
#' values in \emph{x} and derives a \emph{SpatialPolygons} object.}
#' @examples {
#' 
#' require(raster) 
#'
#' # read reference data
#' data(fieldData)
#' 
#' # build polygon from raster (smooth)
#' ref <- raster(extent(fieldData[1,]), res=30, crs=crs(fieldData))
#' p <- smoothTrace(rasterize(fieldData[1,], ref), 1)
#' 
#' # compare objects
#' plot(fieldData[1,])
#' plot(p, add=TRUE, border="blue")
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
  
  f <- (f*2)+1 # update window
  
  x0 <- extend(x, f) # pad image
  x0 <- disaggregate(aggregate(x0, fact=f, max, na.rm=TRUE), fact=f) > 0 # aggregate image (fills gaps)
  p <- rasterToPolygons(crop(x0,x), fun=function(i) {i == 1}, na.rm=TRUE, dissolve=TRUE) # derive polzgon
  
  return(p)
  
}
