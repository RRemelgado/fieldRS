#' @title extractTS
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Summarizes multi-band \emph{raster} data within each element of a \emph{SpatialPolygons} object.
#' @param x Object of class \emph{SpatialPolygons} or \emph{SpatialPolygonsDataFrame}.
#' @param y A \emph{raster} object.
#' @return A \emph{data.frame}.
#' @importFrom raster crs weighted.mean
#' @importFrom rsMove poly2sample
#' @importFrom stats sd
#' @details {For each element in \emph{x}, the function will identify the overlapping pixels in \emph{y} and, for each pixel, estimates the
#' percentage area covered by the polygon. Using this data as weights, the function estimates a weighted mean for each band in \emph{y}. The
#' function returns a list containing three \emph{data.frame} objects where each row represents a different polygon in \emph{x}:
#' \itemize {
#' \item{\emph{values} - Extracted weighted-mean time-series.}
#' \item{\emph{coords} - Polygon center coordinates.}
#' \item{\emph{cover} - Mean, min, max and standard deviation of the pixel cover.}}}
#' @seealso \code{\link{analyzeTS}}
#' @examples {}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

extractTS <- function(x, y) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (!class(x)[1] %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {stop('"x" is not of a valid class')}
  if (!class(y)[1]!="RasterStack") {stop('"reference.grid" is not of a valid class')}
  if (crs(x)@projargs!=crs(y)@projargs) {stop('"x" and "y" have different projections')}

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. derive weighted mean
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # extract samples per polygon ID
  out <- lapply(1:length(x), function(i) {
    s <- poly2sample(pol.shp=field.data[i,], ref.ext=y, min.cover=0)
    v <- as.numeric(apply(extract(y, x), 2, weighted,mean(x, s@data$cover, na.rm=TRUE)))
    return(list(val=v, xy=s@coords, cov=data.frame(mean=mean(s@data$cover), min=min(s@data$cover),
                                                   max=max(s@data$cover), sd=sd(s@data$cover))))})

  # return list
  return(list(values=do.call(rbind, lapply(out, function(x) {x$val})),
         coords=do.call(rbind, lapply(out, function(x) {x$xy})),
         cover=do.call(rbind, lapply(out, function(x) {x$cov}))))

}
