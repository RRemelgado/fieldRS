#' @title spCentroid
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Aggregates a spatial object into regions.
#' @param x An object of class \emph{SpatialPoints} or \emph{SpatialPolygons}.
#' @return A \emph{spatialPointsDataFrame} object.
#' @importFrom sp SpatialPointsDataFrame
#' @details {Returns the centroid of each element in \emph{x}.}
#' @seealso \code{\link{ecDistance}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # read field data
#' data(fieldData)
#' 
#' # derive centroids
#' c <- spCentroid(fieldData)
#' 
#' # plot polygons and compare with centroids
#' plot(fieldData)
#' points(c, col="red")
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

spCentroid <- function(x) {
  
  if (class(x)[1] %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {
    
    centroids <- do.call(rbind, lapply(1:length(x), function(j) {
      mc <- apply(x[j,]@polygons[[1]]@Polygons[[1]]@coords, 2, mean)
      return(data.frame(x=mc[1], y=mc[2], id=j))}))
    
    centroids <- SpatialPointsDataFrame(centroids[,1:2], centroids, proj4string=crs(x))
    
    return(centroids)
    
  } else {
    
    stop('"x" is not of a valid class')
    
  }
  
}
