#' @title mape
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description Mean Absolute Percent Error (MAPE).
#' @param x A vector of class \emph{numeric}.
#' @param na.rm Logical. Should the NA values be excluded. Default is TRUE.
#' @return A \emph{numeric} element.
#' @details {Estimates the Mean Absolute Percent Error (MAPE) for a given vector. The MAPE compares the individual values against their mean and
#' translates the mean of the differences into a percent deviation from the mean of the vector. The MAPE is estimated as:
#' \deqn{100 / length(x) * sum(abs((x-mean(x))/x))}}
#' @seealso \code{\link{}}
#' @examples {relative.freq}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

mape <- function(x, na.rm=TRUE) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  if (!is.numeric(x)) {stop('"x" is not a numeric vector')}

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. estimate MAPE
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # remove missing values
  if (na.rm) {x <- x[!is.na(x)]}

  # requires at least one non-NA value
  if (length(x) > 0) {

    # return MAPE
    return(100 / length(x) * sum(abs((x-mean(x))/x)))

  } else {

    # return NA is no values exist
    return(NA)

  }

}
