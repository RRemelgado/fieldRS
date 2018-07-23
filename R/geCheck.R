#' @title geCheck
#' @description Finds overlaps between polygons in the same shapefile.
#' @param x An object of class \emph{SpatialPolygons} or \emph{SpatialPolygonsDataFrame}.
#' @return A \emph{list}.
#' @details {compares all elements of \emph{x} and returns a a list containing:
#' \itemize{
#'  \item{\emph{overlap.df} - \emph{data.frame} where each row shows the indices of which two polygons overlap.}
#'  \item{\emph{overlap.shp} - \emph{SpatialPointsDataFrame} with the actual overlap for each row in \emph{overlap.df}.}}}
#' @importFrom rgeos intersect
#' @importFrom raster crop which.max
#' @seealso \code{\link{spCentroid}} \code{\link{ecDistance}}
#' @examples {
#' 
#' require(raster)
#' 
#' # build polygons
#' df1 <- data.frame(x=c(1, 5, 10, 2, 1), y=c(10, 9, 8, 7, 10))
#' df2 <- data.frame(x=c(2, 6, 5, 4, 2), y=c(10, 9, 7, 4, 10))
#' p <- list(Polygons(list(Polygon(df1)), ID=1), 
#' Polygons(list(Polygon(df2)), ID=2))
#' p <- SpatialPolygons(p)
#' 
#' # check overlap
#' co <- geCheck(p)
#' 
#' # show distance matrix
#' plot(p)
#' plot(co$overlap.shp, col="red", add=TRUE)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

geCheck <- function(x) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (!class(x)[1] %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {stop('"x" is not of a valid class')}
  if (length(x) == 1) {
    warning('"x" only has one entry (aborting)')
    return(NULL)}
  nr <- length(x)
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. Find overlaps
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  om <- matrix(0, nr, nr)
  for (i in 1:nr) {for (j in 1:nr) {om[i,j] <- ifelse(!is.null(crop(x[i,], x[j,])), 1, 0)}}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. Determine which polygons are overlapped
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  i <- which(om == 1) # overlapping polygons
  rp = ((i-1) %% nrow(om)) + 1 # row position
  cp = floor((i-1) / nrow(om)) + 1 # column position
  o1 <- data.frame(polygon_1=rp, polygon_2=cp)
  
  # remove duplicated occurrences
  i <- which(!duplicated(apply(cbind(rp, cp), 1, function(j) {paste0(sort(j), collapse="")})) & rp!=cp)
  o1 <- o1[i,]
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. Build shapefile with overlapping areas
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (nrow(o1) > 0) {o2 <- do.call(rbind, lapply(1:nrow(o1), function(i) {return(intersect(x[o1[i,1],], x[o1[i,2],]))}))} else {o2 <- NULL}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 5. Build / Derive output
#-----------------------------------------------------------------------------------------------------------------------------------------------#
                
  return(list(overlap.df=o1, overlap.shp=o2))
  
}

