#' @title ecDistance
#' @description Calculates the ecludian distance among all elements of a SpatialPoints object.
#' @param x A \emph{matrix}, \emph{data.frame} or a \emph{SpatialPoints} object.
#' @return A \emph{matrix}.
#' @details {compares all elements of \emph{x} and returns the minimum ecludian distance between them.}
#' @importFrom rgeos gDistance
#' @seealso \code{\link{checkOverlap}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read field data
#' p <- shapefile(system.file("extdata", "fields.shp", package="fieldRS"))
#' 
#' # show distance matrix
#' head(ecDistance(p))
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

ecDistance <- function(x) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (class(x)[1] %in% c('SpatialPoints', 'SpatialPointsDataFrame')) {
    od <- function(n) {
     return(do.call(rbind, lapply(n, function(i) {
       p1 <- x[i,]@coords
       md <- sapply(n, function(p) {
         p2 <- x[p,]@coords
         d <- sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)
         return(d)})})))}}
  
  if (class(x)[1] %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {
    od <- function(n) {
      return(do.call(rbind, lapply(n, function(i) {
      p1 <- x[i,]
      md <- sapply(n, function(p) {
        p2 <- x[p,]
        d <- gDistance(p1, p2)
        return(d)})})))}}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. build distance matrix
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (exists("od")) {
    
    od <- od(1:length(x)) # estimate distance
    od[which(od==0)] <- NA # set redundant values to NA
    return(od)
    
  } else {
    
    stop('"x" is not of a valid class')
    
  }
  
}
