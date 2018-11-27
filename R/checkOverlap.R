#' @title checkOverlap
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Reports on how much two spatial objects overlap.
#' @param x A spatial object.
#' @param y A spatial object.
#' @return A two element numeric \emph{vector}.
#' @importFrom raster extent intersect ncell raster
#' @details {Uses \link[raster]{intersect} to report on the percentage of the area
#' of \emph{x} and \emph{y} that coincides with their common spatial overlap.}
#' @examples {
#' 
#'  require(raster)
#'  
#'  # build polygons
#'  df1 <- data.frame(x=c(1, 5, 10, 2, 1), y=c(10, 9, 8, 7, 10))
#'  df2 <- data.frame(x=c(2, 6, 5, 4, 2), y=c(10, 9, 7, 4, 10))
#'  p <- list(Polygons(list(Polygon(df1)), ID=1), 
#'  Polygons(list(Polygon(df2)), ID=2))
#'  p <- SpatialPolygons(p)
#'
#'  # check overlap %
#'  checkOverlap(p[1,], p[2,])
#'
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

checkOverlap <- function(x, y) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  r1 <- tryCatch(extent(x), error=function(e) return(NULL)) # is x a spatial object?
  r2 <- tryCatch(extent(y), error=function(e) return(NULL)) # is y a spatial object?
  if (is.null(x)) {return(warning('"x" is not a spatial object'))}
  if (is.null(y)) {return(warning('"y" is not a spatial object'))}

  oc <- tryCatch(intersect(r1,r2), error=function(e) return(NULL)) # do they overlap?
  if (is.null(oc)) {return(c(0,0))}

  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 2. Compare input variables
  #-----------------------------------------------------------------------------------------------------------------------------------------------#

  ac <- ncell(raster(extent(oc), res=1)) # how much is the overlap area?
  a1 <- round((ac / ncell(raster(r1, res=1))) * 100) # what proportion of x is in the overlap?
  a2 <- round((ac / ncell(raster(r2, res=1))) * 100) # what proportion of y is in the overlap?

  return(c(a1,a2))

}
