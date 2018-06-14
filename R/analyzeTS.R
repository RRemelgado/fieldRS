#' @title analyzeTS
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Extracts time series data from a \emph{RasterStack} for a \emph{SpatialPoints} object.
#' @param x Object of class \emph{matrix} or \emph{data.frame}.
#' @param y Vector of class \emph{character} or \emph{numeric} with a length equal to the number of rows in \emph{x}.
#' @return A \emph{SpatialPointsDataDrame} with the coordinate pairs for each of the sampeled pixels.
#' @importFrom ggplot2 ggplot aes_string theme_bw ggtitle geom_ribbon geom_line xlab ylab ylim
#' @importFrom stats median mad
#' @details {For each unique value in \emph{y}, the function will select the rows in \emph{x} that correspond to it and estimate the
#' median, Median Absolte Deviation (MAD), minimum, maximum, mean and standard deviation for each column. Then, the function will build
#' a plot showing the median and draw a buffer that expresses the minimum and maximum. Furthermore, the function will cross-correlate each
#' row in \emph{x} against the median values for each unique class. The final output is a list consisting of:
#' \itemize{
#'  \item{\emph{y.statistics} - Median, minimum and maximum values for each column in \emph{x} over each unique class in \emph{y}.}
#'  \item{\emph{y.r2} - \eqn{R^{2}} between the each row in \emph{x} and the median values for each unique class found in \emph{y.statistics}.}
#'  \item{\emph{plots} - List of line plots for each unique element in \emph{y}.}}}
#' @seealso \code{\link{extractTS}} \code{\link{assignClass}} \code{\link{classModel}}
#' @examples {
#' 
#' require(raster)
#' 
#' # read raster data
#' r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))
#' 
#' # read field data
#' p <- shapefile(system.file("extdata", "fields.shp", package="fieldRS"))
#' 
#' # derive time series
#' ev <- extractTS(p, r)
#' 
#' # read reference profiles
#' reference <- read.csv(system.file("extdata", "classes.csv", package="fieldRS"))
#' 
#' analyzeTS(ev, reference)
#' 
#' }
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

analyzeTS <- function(x, y) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (!class(x)[1]%in%c('matrix', 'data.frame')) {stop('"x" is not of a valid class')}
  if (!class(y)[1]%in%c('numeric', 'character')) {stop('"y" is not of a valid class')}
  if (ncol(x) == 1) {warning('"x" has only 1 variable')}
  if (nrow(x) != length(y)) {stop('"x" and "y" have different lengths')}

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. derive reference time series
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # base variables
  unique.y <- unique(y) # unique classes
  min.value <- min(apply(x, 1, min, na.rm=TRUE), na.rm=TRUE) # define data range (minimum)
  max.value <- max(apply(x, 1, max, na.rm=TRUE), na.rm=TRUE) # define data range (maximum)
  unique.id <- 1:ncol(x) # plot x value

  # derive statistics and plots
  tmp <- lapply(unique.y, function(u) {
    i <- which(y == u)
    d <- data.frame(v1=as.numeric(apply(x[i,], 2, median, na.rm=TRUE)), v2=as.numeric(apply(x[i,], 2, mad, na.rm=TRUE)),
                    v3=as.numeric(apply(x[i,], 2, min, na.rm=TRUE)), v4=as.numeric(apply(x[i,], 2, max, na.rm=TRUE)),
                    v5=as.numeric(apply(x[i,], 2, mean, na.rm=TRUE)), v6=as.numeric(apply(x[i,], 2, sd, na.rm=TRUE)), id=unique.id)
    colnames(d) <- c("median", "mad", "min", "max", "mean", "sd", "id")
    d0 <- d[,c("median", "mad", "id")]
    d0$um <- d$median+d$mad
    d0$lm <- d$median-d$mad
    p <- ggplot(d0, aes_string(x="id")) + theme_bw() + ggtitle(u) + geom_ribbon(aes_string(x='id', ymin='lm', ymax='um'), fill="grey70") +
      geom_line(aes_string(y='median')) + theme_bw() + xlab("\nVariable ID") + ylab("Value\n") + ylim(min.value, max.value)
    return(list(stats=d, plot=p))})

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. correlate reference and id-wise time-series
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  d <- do.call(rbind, lapply(tmp, function(r) {r$stats$median}))
  d <- as.data.frame(do.call(rbind, lapply(1:nrow(x), function(r) {
    v1 <- as.numeric(x[r,])
    r2 <- sapply(1:nrow(d), function(c) {
      v2 <- d[c,]
      i <- which(!is.na(v1) & !is.na(v2))
      return(cor(v1[i], v2[i])^2)})
    return(r2)})))
  colnames(d) <- unique.y

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 5. return output as a list
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  return(list(labels=unique.y, y.statistics=lapply(tmp, function(i) {i$stats}), r2=d, plots=lapply(tmp, function(i) {i$plot})))

}
