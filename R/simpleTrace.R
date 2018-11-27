#' @title simpleTrace
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Outlines a set of spatial points and returns a \emph{SpatialPolygons} object.
#' @param x Object of class \emph{data.frame}, \emph{matrix}, \emph{SpatialPoints} or \emph{SpatialPointsDataFrame}.
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

simpleTrace <- function(x) {
  
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (!class(x)[1] %in% c("data.frame", "matrix", "SpatialPoints", "SpatialPointsDataFrame")) {stop('"x" is not of a valid class')}
  
  # data type check (1)
  if (class(x)[1] %in% c("data.frame", "matrix")) {
    if (ncol(x) != 2) {stop('since "x" is a matrix/data.frame it should have 2 columns')}
    rp <- NA # reference projection is not known
  }
  
  # data type check (2)
  if (class(x)[1] %in% c("SpatialPoints", "SpatialPointsDataFrame")) {
    rp <- crs(x)@projargs # extract projection
    x <- x@coords # extract coordinates
  }
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. build polygon
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  ic <- chull(x) # draw outline
  
  # derive polygon
  if (length(ic) > 2) {
    if (!is.na(rp)) {return(SpatialPolygons(list(Polygons(list(Polygon(x[c(ic,ic[1]),])), ID=1)), proj4string=crs(rp)))}
    if (is.na(rp)) {return(SpatialPolygons(list(Polygons(list(Polygon(x[c(ic,ic[1]),])), ID=1))))}
  } else {
    return(NULL)
  }
  
}
