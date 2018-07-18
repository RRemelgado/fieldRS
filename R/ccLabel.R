#' @title ccLabel
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Labels groups of pixels in a raster object that share similar attributes.
#' @param x Object of class \emph{RasterLayer}, \emph{RasterStack} or \emph{RasterBrick}.
#' @param method Labeling method. Choose between 'simple' and 'change'. Default is 'simple'.
#' @param change.threshold Numeric element.
#' @return A list.
#' @importFrom raster which.max raster extent crs res cellStats clump focal calc freq
#' @details {Uses a 8-neighbor connected component labeling algorithm (determined by \emph{method}) to identify groups of pixels of the same
#' value. Each group receives a distinct numeric label. The function provides two connected component labeling algorithms:
#' \itemize{
#'  \item{\emph{simple} - Connects neighboring pixels with the same value. Suitable for categorical data.}
#'  \item{\emph{spatial} - Estimates the MAPE using a 3x3 moving window distinguishes neighboring pixels 
#'  when the spatial change is lower than \emph{change.threshold}.}
#'  \item{\emph{temporal} - Estimates the MAPE among all bands in a raster object and distinguishes spatially 
#'  neighboring pixels when the temporal change is higher than \emph{change.threshold}.}}
#' The final output of the function is a list consisting of:
#' \itemize{
#'  \item{\emph{regions} - \emph{RasterLayer} object with region labels.}
#'  \item{\emph{frequency} - \emph{data.frame} object with the pixel count for each unique value in \emph{regions}.}}}
#' @seealso \code{\link{classModel}} \code{\link{rankPlots}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # spatial change labeling
#' or <- ccLabel(r[[1]], method="spatial", change.threshold=10)
#' plot(or$regions)
#' 
#' # temporal change labeling
#' or <- ccLabel(r, method="temporal", change.threshold=80)
#' plot(or$regions)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

ccLabel <- function(x, method='simple', change.threshold=NULL) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (!class(x)[1] %in% c('RasterLayer', 'RasterStack', 'RasterBrick')) {stop('"x" is not a valid raster object')}
  if (!method %in%  c('simple', 'spatial', 'temporal')) {stop('"method" is not a valid keyword')}
  
  if (method == 'temporal' & class(x)[1]=='RasterLayer') {stop('"method" set to "temporal" ("x" should be a multi-band raster)')}
  
  if (method %in% c('spatial', 'temporal')) {
    if (is.null(change.threshold)) {stop(paste0('"method" set as ", ', method, '" (please specify "change.threshold")'))}
    if (!is.numeric(change.threshold)) {stop('"change.threshold" is not numeric')}
    if (length(change.threshold) > 1) {stop('"change.threshold" has more than one element')}
    
    if (method == "spatial" & class(x)[1] != "RasterLayer") {
      stop('"method" is set to "spatial", provide "x" as a RasterLayer')}
    if (method == "temporal" & !class(x)[1] %in% c("RasterStack", "RasterBrick")) {
      stop('"method" is set to "temporal", provide "x" as a RasterBrick or RasterStack')}
    
  }

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. find connected pixel regions
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (method == 'simple') {

    uc <- unique(x)
    uc <- uc[!is.na(uc)]
    c <- 0

    regions <- raster(extent(x), crs=crs(x), res=res(x), vals=NA) #base image

    for (u in 1:length(uc)) {
      r <- x == uc[u]
      cp <- which.max(r)
      regions[cp] <- clump(r)[cp] + c
      c <- cellStats(regions, max, na.rm=TRUE)
    }

  }

  if (method == 'spatial') {
    regions <- focal(x, w=matrix(1, 3, 3), mape, na.rm=TRUE)
    regions[regions > change.threshold] <- NA
    regions <- clump(regions)}
  
  if (method == 'temporal') {
    regions <- calc(x, mape)
    regions[regions < change.threshold] <- NA
    regions <- clump(regions)}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. return region image and region pixel frequency
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  return(list(regions=regions, frequency=freq(regions)))

}
