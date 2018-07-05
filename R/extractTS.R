#' @title extractTS
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Summarizes multi-band \emph{raster} data within each element of a \emph{SpatialPolygons} object.
#' @param x Object of class \emph{SpatialPolygons} or \emph{SpatialPolygonsDataFrame}.
#' @param y A \emph{raster} object or a numeric element.
#' @return A \emph{list}.
#' @importFrom raster crs weighted.mean
#' @importFrom rsMove poly2sample
#' @importFrom stats sd
#' @details {For each polygon in \emph{x}, the function identifies the overlapping pixels in \emph{y} and, for each pixel, estimates the
#' percentage area covered by the polygon. Using this data as weights, the function calculates the weighted mean for each band in \emph{y}. 
#' If \emph{y} is a numeric element, the function will build a raster with resolution equal to \emph{y} over which the pixel cover will be 
#' estimated. The function returns a list of three \emph{data.frame} objects where each row represents a different polygon in \emph{x}:
#' \itemize{
#' \item{\emph{pixel.info} - Extracted weighted-mean time-series.}
#' \item{\emph{polygon.info} - Mean, min, max and standard deviation of the pixel cover; centroid coordinates.}
#' \item{\emph{weighted.mean} - Weighted mean raster values (if \emph{y} is a raster object).}}}
#' @seealso \code{\link{analyzeTS}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # read field data
#' p <- shapefile(system.file("extdata", "fields.shp", package="fieldRS"))
#' 
#' # derive time series
#' ev <- extractTS(p[1,], r)
#' 
#' # see information on selected pixels
#' head(ev$pixel.info)
#' 
#' # plot profile
#' plot(ev$weighted.mean[1,], type="l")
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

extractTS <- function(x, y) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (!class(x)[1] %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {stop('"x" is not of a valid class')}
  
  if (!class(y) %in% c("numeric", "RasterStack", "RasterLayer", "RasterBrick")) {stop('"y" is not of a valid class')} else {
    
    if (class(y)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
      if (crs(x)@projargs!=crs(y)@projargs) {stop('"x" and "y" have different projections')}
      ev <- TRUE}
    
    if (is.numeric(y)) {
      if (length(y) > 1) {stop('"y" has more than 1 element')}
      y <- raster(extent(x), res=y, crs=crs(x))
      ev <- FALSE}
    
  }
    
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. derive weighted mean
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # extract samples per polygon ID
  out.df <- do.call(rbind, lapply(1:length(x), function(i) {
    s <- poly2sample(x[i,], y[[1]], min.cover=1)
    if (is.null(s)) {return(NULL)} else {return(data.frame(id=i, x=s@coords[,1], y=s@coords[,2], cov=s@data$cover))}}))
  
  # extract raster values (if y is provided)
  out.val <- lapply(1:length(x), function(i) {
    ind <- which(out.df$id == i)
    if (ev) {v <- apply(extract(y, out.df[ind,2:3]), 2, function(j) {weighted.mean(j, out.df$cov[ind], na.rm=TRUE)})} else {v <- NULL}
    odf <- data.frame(id=i, x=mean(out.df$x[ind]), y=mean(out.df$y[ind]), min.cover=min(out.df$cov[ind]), 
                      max.cover=max(out.df$cov[ind]), mean.cover=mean(out.df$cov[ind]))
    return(list(val=v, info=odf))})
  
  # return list
  return(list(pixel.info=out.df, polygon.info=do.call(rbind, lapply(out.val, function(i) {i$info})), 
              weighted.mean=do.call(rbind, lapply(out.val, function(i) {i$val}))))

}
