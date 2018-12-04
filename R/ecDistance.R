#' @title ecDistance
#' @description Calculates the Euclidean distance among all elements of a SpatialPoints object.
#' @param x A \emph{matrix}, \emph{data.frame} or a \emph{SpatialPoints} object.
#' @return A \emph{matrix}.
#' @details {compares all elements of \emph{x} and returns the minimum Euclidean distance between them.}
#' @importFrom rgeos gDistance
#' @seealso \code{\link{spCentroid}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read field data
#' data(fieldData)
#' 
#' # show distance matrix
#' head(ecDistance(fieldData))
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
    od <- function(y) {
      n <- length(y)
     return(do.call(cbind, lapply(1:n, function(i) {
       p1 <- x[i,]@coords
       md <- sapply(1:n, function(p) {
         p2 <- x[p,]@coords
         d <- sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)
         return(d)})})))}}
  
  if (class(x)[1] %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {
    od <- function(y) {
      n <- length(y)
      return(do.call(cbind, lapply(1:n, function(i) {
      p1 <- y[i,]
      md <- sapply(1:n, function(p) {
        p2 <- y[p,]
        d <- gDistance(p1, p2)
        return(d)})})))}}
  
  if (class(x)[1] %in% c('matrix', 'data.frame')) {
    od <- function(y) {
      n <- nrow(y)
      return(do.call(cbind, lapply(1:n, function(i) {
        p1 <- y[i,]
        md <- sapply(1:n, function(j) {
          p2 <- y[j,]
          d <- sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)
          return(d)})})))}}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. build distance matrix
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (exists("od")) {
    
    od <- od(x) # estimate distance
    od[which(od==0)] <- NA # set redundant values to NA
    return(od)
    
  } else {
    
    stop('"x" is not of a valid class')
    
  }
  
}
