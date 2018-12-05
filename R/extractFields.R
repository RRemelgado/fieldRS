#' @title extractFields
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Extracts and vectorizes clumps of pixels with equal value within a raster object.
#' @param x Object of class \emph{RasterLayer}.
#' @param method One of "chull" or "smooth".
#' @param ... Further arguments to pass to \link{ccLabel}.
#' @return A \emph{SpatialPolygonsDataFrame}.
#' @importFrom raster rasterToPoints res crs cellStats area crop
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom spatialEco polyPerimeter
#' @details {Assuming \emph{x} is a classified or segmented image, this function segments it using 
#' \link{ccLabel} and, draws polygons for each group of connected pixels. The way polygons are drawn 
#' depends on the \emph{method} keyword. The function will accept one of the following options:
#' \itemize{
#'   \item{\emph{chull} - Extracts the center pixel coordinates and builds a polygon based on their minimum convex hull.}
#'   \item{\emph{smooth} - Aggregates pixels to 2x the resolution to fill data gaps, disaggregates them to the original 
#'   resolution, applies an erosion filter and converts the output to a polygon.}}
#' Once this process is completed, the function derives a \emph{SpatialPolygonsDataFrame} reporting on:
#' \itemize{
#'  \item{\emph{region.id} - Unique polygon identifier corresponding to the original pixel region.}
#'  \item{\emph{area} - Polygon Area (in square meters).}
#'  \item{\emph{perimeter} - Polygon perimeter (in meters).}
#'  \item{\emph{percent.cover} - Ration between the polygon area and the area of the corresponding pixels.}}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # spatial change labeling
#' or <- ccLabel(r[[1]], method="spatial", change.threshold=10)$regions
#' 
#' # convert to polygons and plot
#' ef <- extractFields(or)
#' plot(ef)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

extractFields <- function(x, method="chull", ...) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (class(x)[1]!='RasterLayer') {stop('"x" is not of a valid class')}
  if (!method %in% c("smooth", "chull")) {stop('"method" is not a valid keyword')}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. identify pixel clusters
#-----------------------------------------------------------------------------------------------------------------------------------------------#
    
  x <- ccLabel(x, ...)$regions
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. extract raster information
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  rp <- rasterToPoints(x, fun=function(i) {!is.na(i)}) # convert segmented raster to points
  uv <- unique(x) # identify unique regions

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. build polygons
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (method == "smooth") {
    
    # build polygons from convex hull
    pc <- lapply(uv, function(u) {
      i <- which(rp[,3]==u)
      if (length(i) > 2) {p <- smoothTrace(crop(x, extent(rp[i,1:2])), 1)} else {p <- NULL}
      if (!is.null(p)) {p$id <- u}
      return(p)})
    
  }
  
  if (method == "chull") {
    
    # build polygons from convex hull
    pc <- lapply(uv, function(u) {
      i <- which(rp[,3]==u)
      if (length(i) > 2) {p <- simpleTrace(rp[i,1:2])} else {p <- NULL}
      if (!is.null(p)) {p$id <- u}
      return(p)})
    
    # remove NULL observations
    i <- sapply(pc, function(x) {!is.null(x)})
    uv <- uv[i]
    pc <- pc[i]
    
  }

  # remove unused entries and build SpatialPolygons
  shp <- pc[[1]]
  for (p in 2:length(pc)) {shp <- rbind(shp, pc[[p]], makeUniqueIDs = TRUE)}

  rm(pc)
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. derive shape information and filter polygons
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # needed information to extimate polygon complexity
  pixel.area <- res(x) # pixel resolution
  pixel.area <- pixel.area[1] * pixel.area[2] # pixel area
  
  # check if regions are contained by their polygons and evaluate shape
  shp@data <- do.call(rbind,lapply(1:length(shp), function(i) {
    pp <- polyPerimeter(shp[i,])
    pa <- area(shp[i,])
    pc <- pa / (cellStats(x == shp$id[i], sum, na.rm=TRUE) * pixel.area)
    return(data.frame(pp=pp, pa=pa))}))

  return(shp)

}
