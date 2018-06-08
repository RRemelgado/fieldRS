#' @title assignClass
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Classifies temporal profiles based on reference data.
#' @param x A numeric vector.
#' @param y A \emph{matrix} or a \emph{data.frame}.
#' @return A list containing containing multi-variate information on the classification of \emph{x}.
#' @importFrom stats cor
#' @details {The function correlates \emph{x} against each row in \emph{y} and classifies
#' \emph{x} based on the highest r2 value. The final output is provided as a list containing:
#' \itemize{
#'  \item{\emph{class} - Class ID of \emph{x} corresponding to the rrow in \emph{y} with the highest r2 when compared with \emph{x}.}
#'  \item{\emph{r2} - R2 value for the best fit.}
#'  \item{\emph{r2diff} - Difference between the 1st and 2nd highest correlation values.}
#'  \item{\emph{rmse} - Root Mean Square Error (RMSE) betwen \emph{x} and the best fitting profile in \emph{y}.}}}
#' @seealso \code{\link{extractTS}} \code{\link{analyzeTS}}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

assignClass <- function(x, y, z) {
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 1. Check variables
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (!is.numeric(x)) {stop('please provide "x" as a numeric vector')}
  if (!class(y)[1] %in% c("matrix", "data.frame")) {stop('"y" is not of a valid class')}
  if (length(x) != ncol(y)) {stop('"x" has a different lenght from the number of columns in "y"')}
  if (exists("z")) {
    if (!class(z)[1] %in% c("matrix", "data.frame")) {stop('please provide "z" as a numeric vector')}
    if (length(x) != ncol(z)) {stop('"x" has a different lenght from the number of columns in "y"')}
    if (ncol(z) != ncol(y)) {stop('"y" and "z" have a different number of columns')}
    constrain <- TRUE
  } else {constrain <- FALSE}
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 2. Check variables
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # assure x si a numeric variable (failure in cor() otherwise)
  x <- as.numeric(x)
  
  # correlate x with reference profiles in y
  c <- lapply(1:nrow(y), function(r) {
    i <- which(!is.na(y[r,]) & !is.na(x))
    r <- try(cor(as.numeric(y[r,i]), x[i]), silent=TRUE)
    return(ifelse(class(c)[1] == "try-error", NA, c^2))})
  
  # check if further anaylsis are possible
  if (sum(!is.na(c)) == 0) {return(list(class=NA, r2=NA, r2diff=NA, rmse=NA))} else {
    
    # find best fit
    m <- max(c, na.rn=TRUE)
    i1 <- which(c == m)
    i2 <- which(!is.na(y[i1,]) & !is.na(x))
    
    # derive bi-products
    r <- c[i1]
    e <- sqrt(sum((y[i1,i2] - x[i2])^2))
    c <- rev(order(c))
    d <- c[1]-c[2]
    
    # check if profile is above the class minimum
    if (constrain) {
      cc <- x[i2] - z[i1,i2]
      if (min(cc) < 0) {ov <- list(class=NA, r2=NA, r2diff=NA, rmse=NA)}
    } else {ov <- list(class=i, r2=r, r2diff=d, rmse=e, count=length(i2))}
    if (!constain) {ov <- list(class=i, r2=r, r2diff=d, rmse=e, count=length(i2))}
    
    # final output
    return(ov)
    
  }
  
}
