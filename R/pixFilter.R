#' @title pixFilter
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Erosion and dilation filter of a raster image.
#' @param x Object of class \emph{RasterLayer}.
#' @param y A \emph{numeric} element.
#' @param method A \emph{character} element. One of "erode" or "dilate".
#' @return A \emph{RasterLayer}.
#' @importFrom raster focal
#' @details {Uses \link[raster]{focal} to filter \emph{x} using either an erosion or a dilation filter, specified by \emph{method}. If 
#' "erosion" is chosen, the function will identify and filter out border pixels around each cluster of pixels in \emph{x}. Small or isolated 
#' groups of pixels will also be removed. If "dilation" is set, the function will increase the size of each cluster of pixels and simultaneously 
#' remove all gaps within them. The size of the buffer used in this function is defined by \emph{y} and is expressed in number of pixels.}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- raster(system.file("extdata", "ndvi.tif", package="fieldRS")) > 2000
#' r[r == 0] <- NA
#' 
#' # filter image (3x3 erosion)
#' or <- pixFilter(r, 1, "erode")
#' plot(r)
#' plot(or)
#' 
#' #' # filter image (3x3 dilation)
#' or <- pixFilter(r, 1, "dilate")
#' plot(r)
#' plot(or)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

pixFilter <- function(x, y, method) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (!method %in% c("erode", "dilate")) {stop('"method" is not a recognized keyword')}
  if (!is.numeric(y)) {stop('"y" is not numeric')}
  if (length(y) != 1) {stop('"y" has more than 1 element')}
  y <- (y*2) + 1 # adjust window size
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. apply filter
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (method == "erode") {
    f <- focal(x, w=matrix(1, y, y), relative.freq, na.rm=TRUE) * !is.na(x)
    f[f != 1] <- NA
    f <- focal(f, w=matrix(1, y, y), relative.freq, na.rm=TRUE) > 0
    f[f == 0] <- NA
  }
  
  if (method == "dilate") {
    f <- focal(x, w=matrix(1, y, y), relative.freq, na.rm=TRUE) > 0
    f[f == 0] <- NA
    f <- focal(f, w=matrix(1, y, y), relative.freq, na.rm=TRUE) == 1
    f[f == 0] <- NA
  }
  
  
  return(f)
  
  
}

