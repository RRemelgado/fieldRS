#' @title assignClass
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Classifies samples based on reference data.
#' @param x Object of class \emph{data.frame}.
#' @param y Object of class \emph{data.frame}.
#' @param z A character vector.
#' @return A t\emph{data.frame}.
#' @importFrom stats complete.cases cor 
#' @details {For each row in \emph{x}, the function derives the coefficient of determination (\eqn{2^{x}}) between it and each 
#' row in \emph{y}. Then, the function will identify the value in \emph{z} that is associated with the highest \eqn{2^{x}}. The 
#' output if a \emph{data.frame} reporting on the \eqn{2^{x}}) and on the selected \emph{z} value.}
#' @seealso \code{\link{extractTS}} \code{\link{analyzeTS}} \code{\link{classModel}}
#' @examples {}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

assignClass <- function(x, y, z) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. Check input variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  if (!class(x)[1] %in% c('data.frame', 'matrix')) {stop('"x" is not of a valid class')}
  x <- x[complete.cases(x),]
  if (length(x) == 0) {stop('NA values found in one or more rows in "x"')}
  
  if (!class(y)[1] %in% c('data.frame', 'matrix')) {stop('"y" is not of a valid class')}
  if (sum(!complete.cases(y)) > 0) {stop('NA values found in one or more rows in "y"')}
  if (ncol(x) != ncol(y)) {stop('"x" and "y" have a different number of columns')}
  
  if (!is.character(z)) {stop('"z" is not of a character vector')}
  if (nrow(y) != length(z)) {stop('"y" and "z" have a different number of entries')}
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. select classes
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # evaluate class combinations
  out <- do.call(rbind, lapply(1:nrow(x), function(s) {
    r0 <- x[s,]
    r2 <- sapply(1:nrow(y), function(r) {
      r1 <- y[r,]
      return(cor(r0, r1)^2)})
    return(data.frame(r2=max(r2), class=z[which(r2==max(r2))], stringsAsFactors=FALSE))}))
  
  # derive output
  return(out)
  
}
