#' @title classFilter
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Smoothing of a categorical raster object.
#' @param x An object of class \emph{RasterLayer}.
#' @param y A single or a two element vector of class \emph{numeric}.
#' @return A \emph{RasterLayer}.
#' @importFrom raster modal mask focal
#' @details {For all non-NA pixels in \emph{x}, the function uses a moving window with a number of pixels defined by \emph{y} to 
#' look at its surroundings. If at least half of the values are non-NA, the function will return the dominant class.}
#' @seealso \code{\link{classModel}} \code{\link{ccLabel}}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

classFilter <- function(x, y) {

  m <- matrix(1, y, y)
  f <- function(z) {ifelse(!is.na(z) & (sum(!is.na(z)) / length(z)) > 0.5, modal(z, na.rm=TRUE), NA)}
  return(mask(x, focal(x, m, f)))

}
