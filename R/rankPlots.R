#' @title rankPlots
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description helps fix spelling mistakes in the labels of a set of samples.
#' @param x Object of class \emph{RasterLayer}, \emph{RasterStack} or \emph{RasterBrick}.
#' @param y Object of class \emph{SpatialPolygons} or \emph{SpatialPolygonsDataFrame}.
#' @param z Object of class \emph{SpatialLines} or \emph{SpatialLinesDataFrame}.
#' @param min.size Numeric element.
#' @param priority Character vector.
#' @return A list.
#' @importFrom raster crop which.max crs
#' @importFrom sp SpatialPoints
#' @importFrom rgeos gDistance
#' @details {For each polygon in \emph{y}, the function will determine the distance between its centroid and
#' the nearest road provided through \emph{z}, count the number of classes in \emph{x} and the number of patches
#' of connected pixels and report on the proportion of non NA values. The patch count can be restricted to those
#' with a size greater \emph{min.size} which specifies the minimum number of pixels per patch. Then, the function
#' will use this data to rank the elements of \emph{y} according to the order of the keywords in \emph{priority}.
#' The user can choose one or more of the following keywords:
#'  \itemize{
#'  \item{\emph{class_count} - Priority given to the highest class count.}
#'  \item{\emph{pixel_frequency} - Priority given to the highest non-NA pixel count.}
#'  \item{\emph{patch_count} - Priority given to the highest patch count.}
#'  \item{\emph{road_distance} - Priority given to shortest distance.}}
#' The final output is a \emph{data.frame} reporting on:
#'  \itemize{
#'  \item{\emph{x} - Polygon centroid x coordinate.}
#'  \item{\emph{y} - Polygon centroid y coordinate.}
#'  \item{\emph{mape} - Mean Absolute Percent Error.}
#'  \item{\emph{count} - Number of pixel regions.}
#'  \item{\emph{frequency} - Number of non-NA pixels.}
#'  \item{\emph{distance} - Linear distance to the closest road.}
#'  \item{\emph{ranking} - Priority ranking}}}
#' @seealso \code{\link{derivePlots}} \code{\link{ccLabel}}
#' @examples {
#' 
#' require(raster)
#' require(RStoolbox)
#' require(ggplot2)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # read road information
#' data(roads)
#' 
#' # unsupervised classification with kmeans
#' uc <- unsuperClass(r, nSamples=5000, nClasses=5)$map
#' 
#' # derive potential sampling plots
#' pp <- derivePlots(uc, 1000)
#' 
#' # plot ranking
#' pp@data <- rankPlots(uc, pp, roads)
#' 
#' # plot output
#' gp <- fortify(pp, region="ranking")
#' ggplot(gp, aes(x=long, y=lat, group=group, fill=as.numeric(gp$id))) + 
#' geom_polygon() + scale_fill_continuous(name="Ranking")
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

rankPlots <- function(x, y, z, min.size=1, priority=c('class_count', 'patch_count', 'pixel_frequency', 'road_distance')) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  var.ls <- list() # variable list (used for priority ranking)

  # base image
  if (class(x)[1] != 'RasterLayer') {stop('"x" is not of a valid class')}
  var.ls[[1]] <- 'class_count'
  var.ls[[2]] <- 'patch_count'
  var.ls[[3]] <- 'pixel_frequency'

  # pixel region size filter
  if (!is.numeric(min.size)) {stop('"min.size" is not numeric')}
  if (length(min.size) > 1) {stop('"min.size" has more than 1 element')}

  # training plots
  if (!class(y)[1] %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {stop('"y" is not of a valid class')}
  if (checkOverlap(x, y)[2]!=100) {stop('"y" is not contained by "x"')}

  # roads
  if (exists("z")) {
    if (!class(z)[1] %in% c('SpatialLines', 'SpatialLinesDataFrame')) {stop('"roads" is not of a valid class')}
    if (checkOverlap(z, y)[1] == 0) {warning('"z" is not contained by "x"')}
    reportDistance <- TRUE
    var.ls[[(length(var.ls)+1)]] <- 'road_distance'
  } else {reportDistance <- FALSE}

  var.ls <- unlist(var.ls) # convert variable list to a character vector

  # ranking
  if (!is.character(priority)) {stop('"priority" is not a character vector')}
  if (sum(priority %in% c('class_count', 'pixel_frequency', 'patch_count', 'road_distance'))!=length(priority)) {
    stop('"priority" has one or more invalid keywords')}
  priority <- priority[priority %in% var.ls]

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. analyze plots
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  regions <- ccLabel(x)$regions # label regions

  # statistics
  df.original <- do.call(rbind, lapply(1:length(y), function(j) {
    r <- crop(x, y[j,])
    i <- which.max(!is.na(r))
    cc <- length(unique(r[i]))
    pf <- relative.freq(r[i])
    r <- crop(regions, y[j,])
    pc <- sum(freq(r)[,2] >= min.size)
    if (reportDistance) {
      d <- spCentroid(y[j,])@coords
      dr <- SpatialPoints(cbind(d[1], d[2]), proj4string=crs(z))
      dr <- min(gDistance(dr, z, byid=TRUE))
    } else {
      xc <- NA
      yc <- NA
      dr <- NA}
    return(data.frame(x=d[1], y=d[2], c1=cc, pc=pc, frequency=pf, distance=dr))}))

  colnames(df.original) <- c('x', 'y', 'class_count', 'patch_count', 'pixel_frequency', 'road_distance')

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. rank plots
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  df.relative <- df.original
  df.relative[,3] <- 1 - (df.relative[,3] / max(df.relative[,3], na.rm=TRUE))
  df.relative[,4] <- 1 - (df.relative[,4] / max(df.relative[,4], na.rm=TRUE))
  df.relative[,5] <- 1 - (df.relative[,5] / max(df.relative[,5], na.rm=TRUE))
  if (reportDistance) {df.relative[,6] <- df.relative[,6] / max(df.relative[,6], na.rm=TRUE)}

  # priority sorting
  df.original$ranking <- do.call(order, df.original[priority]) # rank
  colnames(df.original) <- c('x', 'y', 'class count', 'patch count', 'frequency', 'distance', 'ranking')

  # return list with results
  return(df.original)

}
