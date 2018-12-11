#' @title extractFields
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Extracts and vectorizes clumps of pixels with equal value within a raster object.
#' @param x Object of class \emph{RasterLayer}.
#' @param method One of "simple" or "complex".
#' @param smooth.x A logical argument.
#' @return A \emph{SpatialPolygonsDataFrame}.
#' @importFrom raster rasterToPoints res crs cellStats area crop freq
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom spatialEco polyPerimeter
#' @importFrom concaveman concaveman
#' @importFrom grDevices chull
#' @details {Assuming \emph{x} is a classified or segmented image, this function segments it using 
#' \link{ccLabel} and, draws polygons for each group of connected pixels. The way polygons are drawn 
#' depends on the \emph{method} keyword. The function will accept one of the following options:
#' \itemize{
#'   \item{\emph{simple} - Extracts the center pixel coordinates and builds a polygon based on their minimum convex hull.}
#'   \item{\emph{complex} - Extracts the center pixel coordinates and builds a polygon based on their minimum concave hull.}}
#'   The "simple" approach is a faster but it can lead to poor results when dealing with very complex shapes. For example, crop 
#'   fields can be rectangular in which case the "simple" method is sufficient. On the other hand, forest belts can have irregular 
#'   shapes, in which case the "complex" method is more appropriate. By default, the function will use all pixels associated to a 
#'   region. However, if \emph{smooth} is set to TRUE, for each region, the function will remove samples with less than 5 neighbors 
#'   efectively removing isolated pixels along the edge of a region. The final output is a \emph{SpatialPolygonsDataFrame} reporting on:
#' \itemize{
#'  \item{\emph{region.id} - Unique polygon identifier corresponding to the original pixel region.}
#'  \item{\emph{area} - Polygon Area (in square meters).}
#'  \item{\emph{perimeter} - Polygon perimeter (in meters).}
#'  \item{\emph{cover.ratio} - Ration between the polygon area and the area of the corresponding pixels.}}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # spatial change labeling
#' or <- ccLabel(r, method="temporal", change.threshold=50)$regions
#' 
#' # convert to polygons and plot (simple)
#' ef <- extractFields(or[1:50,1:50, drop=FALSE])
#' plot(ef)
#' 
#' # convert to polygons and plot (complex)
#' ef <- extractFields(or[1:50,1:50, drop=FALSE], method="complex")
#' plot(ef, border="red", add=TRUE)
#' 
#' # convert to polygons and plot (complex and smoothed)
#' ef <- extractFields(or[1:50,1:50, drop=FALSE], method="complex", smooth=TRUE)
#' plot(ef, border="blue", add=TRUE)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

extractFields <- function(x, method="simple", smooth.x=FALSE) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (class(x)[1]!='RasterLayer') {stop('"x" is not of a valid class')}
  if (!method %in% c("simple", "complex")) {stop('"method" is not a valid keyword')}
  if (!is.logical(smooth.x)) {stop('"smooth.x" is not a logical argument')}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. identify pixel clusters
#-----------------------------------------------------------------------------------------------------------------------------------------------#
    
  x <- ccLabel(x)$regions
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. extract raster information
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  rp <- rasterToPoints(x, fun=function(i) {!is.na(i)}, spatial=TRUE) # convert segmented raster to points
  
  # apply smoothing when prompted
  if (smooth.x) {
    x1 <- x > 0
    x1[x1==0] <- NA
    x1 <- focal(x1, matrix(1,3,3), relative.freq)
    ev <- extract(x1, rp)
    rp <- rp[which(ev > 0.5),]
    rm(x1, ev)
  }
  
  uv <- unique(rp@data[,1]) # identify unique regions

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. build polygons
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (method == "simple") {
    
    # build polygons from convex hull
    pc <- lapply(uv, function(u) {
      i <- which(rp@data[,1]==u)
      ic <- chull(rp@coords[i,1:2])
      ic <- c(ic, ic[1])
      if (length(ic) >= 4) {
        return(Polygons(list(Polygon(rp@coords[i[ic],1:2])), ID=u))
      } else {return(NULL)}})
    
  }
  
  if (method == "complex") {
    
    # build polygons from convex hull
    pc <- lapply(uv, function(u) {
      i <- which(rp@data[,1]==u)
      p <- concaveman(rp[i,])
      t1 <- p@bbox[1,1]-p@bbox[1,2]
      t2 <- p@bbox[2,1]-p@bbox[2,2]
      if (t1 == 0 | t2 == 0) {return(NULL)} else {
        p$region.id <- u
        return(p)
      }})
    
  }
  
  # return clumped image if the are no polygons
  i <- sapply(pc, function(i) {!is.null(i)})
  if (sum(i) == 0) {
    warning('unable to draw polygons, returning clumped image instead (have a look, there are no regions with more than 2 pixels)')
    return(x)}
  
  # remove NULL observations
  uv <- uv[i]
  pc <- pc[i]
  
  # build polygons
  if (method=="simple") {shp <- SpatialPolygonsDataFrame(SpatialPolygons(pc, proj4string=crs(x)), data.frame(region.id=uv), match.ID=FALSE)}
  if (method=="complex") {shp <- do.call(rbind, pc)}
  
  # remove temporary data
  rm(i, pc, uv, rp)
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. derive shape information and filter polygons
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # needed information to extimate polygon complexity
  pixel.area <- res(x) # pixel resolution
  pixel.area <- pixel.area[1] * pixel.area[2] # pixel area
  
  # evaluate each polygon
  shp@data$area <- area(shp) # in m2
  shp@data$arperimeter <- polyPerimeter(shp) # in m
  shp@data$cover.ratio <- shp@data$area / (freq(x)[shp@data$region.id,2]*pixel.area) # polygon/region ratio

  return(shp)

}
