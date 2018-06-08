#' @title ccLabel
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Labels groups of pixels in a raster object that share similar atributes.
#' @param x Object of class \emph{RasterLayer}, \emph{RasterStack} or \emph{RasterBrick}.
#' @param method Labeling method. Choose between 'simple' and 'change'. Default is 'simple'.
#' @param change.threshold Numeric element.
#' @return A list.
#' @importFrom raster which.max raster extent crs res cellStats clump focal calc freq
#' @details {Uses a 8-neighbor connected compoent labelling algorithm (determined by \emph{method}) to identify groups of pixels of the same
#' value. Each group receives a distinct numeric label. The function provides two connected component labeling alogorithms:
#' \itemize{
#'  \item{\emph{simple} - Connects neighboring pixels with the same value. Suitable for categorical data.}
#'  \item{\emph{space_change} - Estimates the MADE using a 3x3 moving window and connects pixels where the 
#'  spatial change \emph{change.threshold}.}
#'  #'  \item{\emph{time_change} - Estimates the MADE using all layers in a multi-band raster and connects 
#'  pixels where the temporal change is below \emph{change.threshold}.}}
#' The final output of the function is a list consisting of:
#' \itemize{
#'  \item{\emph{regions} - \emph{RasterLayer} object with region labels.}
#'  \item{\emph{frequency} - \emph{data.frame} object with the pixel count for each unique value in \emph{regions}.}}}
#' @seealso \code{\link{classModel}} \code{\link{classFilter}}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

ccLabel <- function(x, method='simple', change.threshold=NULL) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (!class(x)[1] %in% c('RasterLayer', 'RasterStack', 'RasterBrick')) {stop('"x" is not a valid raster object')}
  if (!method %in%  c('simple', 'spatial_change', 'temporal_change')) {stop('"method" is not a valid keyword')}
  
  if (method == 'temporal_change' & class(x)[1]=='RasterLayer') {stop('"method" set to "temporal_change" ("x" should be a multi-band raster)')}
  
  if (method %in% c('spatial_change', 'temporal_change')) {
    if (is.null(change.threshold)) {stop(paste0('"method" set as ", ', method, '" (please specify "change.threshold")'))}
    if (!is.numeric(change.threshold)) {stop('"change.threshold" is not numeric')}
    if (length(change.threshold) > 1) {stop('"change.threshold" has more than one element')}}

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

  if (method == 'spatial_change') {regions <- clump(focal(r, w=matrix(1, 3, 3), mape, na.rm=TRUE) < change.threshold)}

  if (method == 'temporal_change') {regions <- clump(calc(x, mape, na.rm=TRUE) < change.threshold)}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. return region image and region pixel frequency
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  return(list(regions=regions, frequency=freq(regions)))

}
