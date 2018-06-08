#' @title splitSamples
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Aggregates a spatial object into regions.
#' @param x A \emph{SpatialPoints} or a \emph{SpatialPolygons} object.
#' @param y A \emph{RasterLayer}.
#' @param z A vector.
#' @param agg.radius Numeric element.
#' @return A list.
#' @importFrom raster crs rasterize which.max rowFromCell colFromCell extract
#' @details {For each class in \emph{z}, the function converts the elements in \emph{x} into a raster layer using \emph{y} as a basis. Then, 
#' it aggregates all pixels that are within a given distance of each other - defined by \emph{agg.radius} using \code{\link{ccLabel}}. The 
#' output is a list consisting of:
#' \itemize{
#'  \item{\emph{region.index} - Class dependent region label for each element in \emph{x}.}
#'  \item{\emph{region.frequency} - Pixel count for each unique value in \emph{region.index}.}}}
#' @seealso \code{\link{assignClass}} \code{\link{classModel}}
#' @examples {}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

splitSamples <- function(x, y, z, agg.radius=agg.radius) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables 
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (!class(x)[1] %in% c('SpatialPoints', 'SpatialPointsDataFrame', 'SpatialPolygons', 'SpatialPolygonsDataFrame')) {
    stop('"x" is not a valid spatial object')}
  if (!class(y) %in% c('RasterLayer', 'RasterStack', 'RasterBrick')) {stop('"y" is not a valid raster object')}
  
  if (crs(x)@projargs!=crs(y)@projargs) {stop('"x" and "y" have different projections')}
  if (checkOverlap(x,y)[1]) {stop('"x" is not contained by "y"')}
  rdims <- dim(y) # raster dimensions
  
  if (exists(z)) {if (!is.vector(z)) {stop('"z" should be a vector')}} else {z <- replicate(length(x), 1)}
  unique.z <- unique(z) # target classes
  
  if (!is.numeric(agg.radius)) {stop('"agg.radius" is not a numeric element')}
  if (length(agg.radius) > 0) {stop('"agg.radius" has more than 1 element')}
  agg.radius <- round(res(y) / agg.radius) # number of pixels
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. derive region indices
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  region.index <- as.character(z) # region id for each sample
  region.freq <- vector('list', length(z)) # region pixel count
  
  for (c in 1:length(unique.z)) {
    
    ri <- which(z == unique.z[c]) # target samples for class
    regions <- rasterize(x[ri,], y, field=1, background=0) # sample mask
    ci <- which.max(regions) # target cells
    regions[] <- 0 # remove values
    
    # dilate image samples
    for (p in 1:length(ci)) {
      rp <- rowFromCell(r, ci[p])
      cp <- colFromCell(r, ci[p])
      if (cp > agg.radius) {sc<-cp-agg.radius} else {sc<-cp}
      if (cp < (rdims[2]-agg.radius)) {ec<-cp+agg.radius} else {ec<-cp}
      if (rp > agg.radius) {sr<-rp-agg.radius} else {sr<-rp}
      if (rp < (rdims[1]-agg.radius)) {er<-rp+agg.radius} else {er<-rp}
      regions[sr:er,sc:ec]<- 1}
    
    # label regions
    regions <- ccLabel(regions)$regions
    
    # update region id's
    for (r in 1:length(ri)) {region.index[ri[r]] <- paste0(region.index[ri[r]], '_', as.character(unique(extract(regions, x[ri[r],]))))}
    
    # count pixels per region
    urv <- unique(region.index[ri[r]])
    region.freq[[c]] <- data.frame(id=urv, count=sapply(urv, function(r) {sum(region.index[ri]==r)}))
    
  }
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. return output
#-----------------------------------------------------------------------------------------------------------------------------------------------#
 
  return(region.index=region.index, region.frequency=region.freq)

}