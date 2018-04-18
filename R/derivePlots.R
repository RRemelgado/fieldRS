#' @title derivePlots
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Creates a fishnet from a spatial extent.
#' @param x A spatial object.
#' @param y A numeric element.
#' @return An object of class \emph{SpatialPolygons}.
#' @importFrom raster crop rasterToPolygons raster extent crs
#' @details {Creates a rectangular fishnet in a \emph{SpatialPolygon} format based on
#' the extent of \emph{x} and the value of \emph{y} which defines the spatial resolution.}
#' @seealso \code{\link{rankPlots}}
#' @examples {}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

derivePlots <- function(x, y) {

  # determine rescaling factor
  if (length(y) > 1) {stop('"y" has more than 1 element')}

  p <- crop(rasterToPolygons(raster(extent(x), res=y, crs=crs(x))), x)

  return(p)

}

