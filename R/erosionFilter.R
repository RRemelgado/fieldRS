#' @title erosionFilter
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Applies an erosion and dilation filter over a raster object.
#' @param x Object of class \emph{RasterLayer}.
#' @return A \emph{RasterLayer}.
#' @importFrom raster raster res crs
#' @details {For each unique, non-NA value in \emph{x} the function derives a mask, 
#' erodes it and dilates it simplifying the shape of the corresponding pixels.}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # spatial change labeling
#' or <- ccLabel(r[[1]], method="spatial_change", change.threshold=10)$regions
#' 
#' # convert to polygons and plot
#' er <- erosionFilter(or)
#' 
#' # plot and compare
#' plot(or)
#' plot(er)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

erosionFilter <- function(x) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (class(x)[1]!='RasterLayer') {stop('"x" is not of a valid class')}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. extract raster information and build base raster
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  r <- raster(extent(x), res=res(x), crs=crs(x), vals=NA) # region raster
  ur <- unique(x) # unique values in x
  ur <- ur[!is.na(ur)] # remove NA's from vector of unique values
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. apply filter
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  for (i in 1:length(ur)) {
    
    tmp <- or$regions
    tmp[tmp != ur[i]] <- NA
    f <- focal(tmp, matrix(1, 3, 3), relative.freq)
    f[f != 1] <- NA
    f <- focal(f, matrix(1, 3, 3), relative.freq)
    r[f > 0] <- ur[i]
    
  }
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. re-evaluate pixel connectivity
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  r <- ccLabel(r)$regions
  
  return(r)
  
}
