#' @title raster2sample
#'
#' @description {Converts a raster grid to points.}
#' @param x Object of class \emph{SpatialPolygons} or \emph{SpatialPolygonDataFrame}.
#' @param y A raster object or a numeric element.
#' @param min.cover Minimum percent a pixel should be covered by a polygon for sampling (1-100). Default is 1.
#' @importFrom raster raster extent crop rasterToPoints rasterize xyFromCell cellFromXY crs
#' @importFrom sp SpatialPointsDataFrame
#' @seealso \code{\link{dataQuery}} \code{\link{imgInt}}
#' @return A \emph{SpatialPointsDataFrame} with sampled pixels reporting on pixel compactness.
#' @details {\emph{poly2Sample} extends on the \code{\link[raster]{RasterToPoint}} function from the raster package. For 
#' each non-NA pixel in \emph{x}, the function will use 3x3 moving window and report on the frequency of non-NA pixels. 
#' This can be useful to identify "pure" samples within a clump of pixels (i.e. high frequency) as well as mixed pixels 
#' along their borders (i.e. low frequency).}
#' @examples {
#'
#'  require(raster)
#'
#'  # load example probability image
#'  file <- system.file('extdata', 'probabilities.tif', package="rsMove")
#'  img <- raster(file) > 0.5
#'  img[img != 1] <- NA
#'  
#'  # extract samples
#'  samples <- raster2sample(img)
#'
#' }
#' @export

#-------------------------------------------------------------------------------------------------------------------------#

raster2sample <- function(x) {
  
#-------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-------------------------------------------------------------------------------------------------------------------------#
  
  # check/derive reference raster
  if (class(x)[1] != "RasterLayer") {stop('"x" is not a RasterLayer')}
  
#-------------------------------------------------------------------------------------------------------------------------#
# 2. evaluate pixel compactness
#-------------------------------------------------------------------------------------------------------------------------#
  
  px.freq <- focal(x, matrix(1,3,3), function(i) {ifelse(is.na(i), NA, sum(!is.na(i)) / length(i))})
  
#-------------------------------------------------------------------------------------------------------------------------#
# 3. derive samples
#-------------------------------------------------------------------------------------------------------------------------#
  
  odf <- as.data.frame(rasterToPoints(px.freq))
  colnames(odf) <- c('x', 'x', 'cover')
  
#-------------------------------------------------------------------------------------------------------------------------#
# 4. build shapefile
#-------------------------------------------------------------------------------------------------------------------------#
  
  return(SpatialPointsDataFrame(odf[,1:2], odf, proj4string=crs(x)))
  
}
