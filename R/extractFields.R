#' @title extractFields
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Extracts and vectorizes patches of equal value within a raster object.
#' @param x Object of class \emph{RasterLayer}.
#' @return A \emph{SpatialPolygonsDataFrame}.
#' @importFrom raster rasterToPoints res crs cellStats area crop
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom grDevices chull
#' @importFrom spatialEco polyPerimeter
#' @details {Given a segmented image as \emph{x}, the function extracts patches of pixels with equal value. For each 
#' pixel region, the function extracts the center pixel coordinates and derives their minimum convex polygon. Then, 
#' for each polygon, the derives a ratio between the area of the polygon and the area of the pixel region. Ratios 
#' below zero suggest that the region has a clearly defined shape (e.g. rectangular). Clumps is less than 3 points 
#' are ignored. The output of the function is a \emph{SpatialPolygonsDataFrame} reporting on:
#' \itemize{
#'  \item{\emph{Region ID} - Unique polygon identifier.}
#'  \item{\emph{Area} - Polygon Area (in square meters).}
#'  \item{\emph{Perimeter} - Polygon perimeter (in square meters).}
#'  \item{\emph{Pixel \%} - Percentage of non-NA pixels.}}}
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
#' ef <- extractFields(or)
#' plot(ef)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

extractFields <- function(x) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (class(x)[1]!='RasterLayer') {stop('"x" is not of a valid class')}

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. extract raster information
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  rp <- rasterToPoints(x) # convert segmented raster to points
  uv <- unique(rp[,3]) # identify unique regions
  uv <- uv[!is.na(uv)] # remove NA values

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. build polygons
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # build polygons from convex hull
  pc <- lapply(uv, function(u) {
    i <- which(rp[,3]==u)
    xy <- rp[i, 1:2]
    ic <- chull(xy)
    if (length(ic) > 2) {return(Polygons(list(Polygon(xy[c(ic,ic[1]),])), ID=u))} else {return(NULL)}})

  # remove unused entries and build SpatialPolygons
  i <- sapply(pc, function(x) {!is.null(x)})
  uv <- uv[i]
  shp <- SpatialPolygons(pc[i], proj4string=crs(x))

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. derive shape information and filter polygons
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # check if regions are contained by their polygons and evaluate shape
  shp.info <- lapply(1:length(shp), function(i) {
    pp <- polyPerimeter(shp[i,])
    pa <- area(shp[i,])
    return(list(pp=pp, pa=pa))})

  # build SpatialPolygonsDataFrame
  odf <- data.frame(ID=uv, sapply(shp.info, function(i) {i$pa}), pa=sapply(shp.info, function(i) {i$pp}))
  colnames(odf) <- c("region ID", "Area", "Perimeter")
  row.names(odf) <- uv
  shp <- SpatialPolygonsDataFrame(shp, odf)

  return(shp)

}
