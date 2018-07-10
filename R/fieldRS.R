#' fieldRS.
#'
#' @name rsMove
#' @docType package
#' @import raster sp
NULL

#' Polygon shapefile.
#'
#' Ground truth data on crop types collected in Uzbekistan.
#'
#' \itemize{
#'   \item{crop}{Crop type.}
#'   \item{date}{Sampling date.}
#'   \item{area}{Area in m^{2}.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name fieldData
#' @usage data(fieldData)
#' @format A SpatialPointsDataFrame
NULL

#' Reference profiles.
#'
#' Reference NDVI profiles of selected classes.
#'
#' @docType data
#' @keywords datasets
#' @name referenceProfiles
#' @usage data(referenceProfiles)
#' @format A data.frame
NULL

#' Road shapefile.
#'
#' Road shapefile information extract from Open Street Map.
#'
#' @docType data
#' @keywords datasets
#' @name roads
#' @usage data(roads)
#' @format A SpatialLinesDataFrame
NULL
