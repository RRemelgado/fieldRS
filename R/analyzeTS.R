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
#' @examples {}
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

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. derive reference time series
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # base variables
  unique.y <- unique(y) # unique classes
  min.value <- min(x, na.rm=TRUE) # define data range (minimum)
  max.value <- max(x, na.rm=TRUE) # define data range (maximum)
  unique.id <- colnames(x) # plot x value

  # derive statistics and plots
  tmp <- lapply(unique.y, function(u) {
    i <- which(y == u)
    d <- data.frame(v1=as.numeric(apply(x[i,], 2, median, na.rm=TRUE)), v2=as.numeric(apply(x[i,], 2, mad, na.rm=TRUE)),
                    v3=as.numeric(apply(x[i,], 2, min, na.rm=TRUE)), v4=as.numeric(apply(x[i,], 2, max, na.rm=TRUE)),
                    v5=as.numeric(apply(x[i,], 2, mean, na.rm=TRUE)), v6=as.numeric(apply(x[i,], 2, sd, na.rm=TRUE)), id=unique.id)
    colnames(d) <- c("median", "mad", "min", "max", "mean", "sd")
    p <- ggplot(d, aes_string(x="doy")) + theme_bw() + ggtitle(u) + geom_ribbon(aes_string(x='id', ymin='min', ymax='max'), fill="grey70") +
      geom_line(aes_string(y='median')) + theme_bw() + xlab("\nVariable ID") + ylab("Value\n") + ylim(min.value, max.value)
    return(list(stats=d, plot=p))})

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. correlate reference and id-wise time-series
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  d <- do.call(rbind, lapply(tmp, function(r) {r$stats$median}))
  d <- as.data.frame(do.call(rbind, lapply(1:nrow(x), function(r) {
    v1 <- x[r,]
    r2 <- sapply(1:nrow(d), function(c) {
      v2 <- d[c,]
      i <- which(!is.na(v1) & !is.na(v2))
      return(cor(v1[i], v2[i])^2)})
    return(r2)})))
  colnames(d) <- unique.y
  row.names(d) <- y

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 5. return output as a list
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  return(list(y.statistics=do.call(rbind, lapply(tmp, function(i) {i$stats})), r2=d, plots=lapply(tmp, function(i) {i$plot})))

}
