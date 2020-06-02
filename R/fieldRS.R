#' fieldRS.
#'
#' @name fieldRS
#' @description Example datasets of the fieldRS package
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
#' @format A SpatialPolygonsDataFrame
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

#' Pixel sample shapefile.
#'
#' Samples derived from the fieldData dataset with poly2sample().
#'
#' @docType data
#' @keywords datasets
#' @name samples1
#' @usage data(samples1)
#' @format A SpatialLinesDataFrame
NULL

#' Predictive classification model accuracy (class-wise)
#'
#' Sample-wise validation returned by classModel().
#'
#' @docType data
#' @keywords datasets
#' @name predictive.model1
#' @usage data(predictive.model1)
#' @format A logical vector.
NULL

#' Predictive classification model accuracy (overall)
#'
#' Class-wise F1-score returned by classModel().
#'
#' @docType data
#' @keywords datasets
#' @name predictive.model2
#' @usage data(predictive.model2)
#' @format A data.frame
NULL