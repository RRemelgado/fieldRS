#' @title labelCheck
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description helps fix spelling mistakes in the labels of a set of samples.
#' @param x Vector of class \emph{character}.
#' @param y Vector of class \emph{character}.
#' @param z Vector of class \emph{character}.
#' @param auto Logical argument. Default is FALSE.
#' @importFrom ggplot2 aes_string geom_bar theme_bw theme xlab ylab element_text
#' @importFrom stringdist stringdist
#' @return A \emph{character} vector.
#' @details {If \emph{y} and \emph{z} are missing, the function will return the unique values among
#' all the elements of \emph{y}. Otherwise, the function will provide a corrected copy of \emph{y}.
#' Additionally, the function will count the number of records for each of the unique labels from 
#' which a plot will be built. The final output consists of:
#' \itemize{
#'  \item{\emph{unique.labels} - Unique labels in the output.}
#'  \item{\emph{corrected.labels} - Corrected labels in \emph{x}.}
#'  \item{\emph{label.count} - Count of occurrences in \emph{unique.labels} per each element in \emph{x}.}
#'   \item{\emph{label.count.plot} - Plot of \emph{label.count}.}}
#' If \emph{auto} is set to TRUE, the user can ignore \emph{z} to correct the existing labels. Instead, the 
#' user can provide all the potential cases through \emph{y}. Then, for each element in \emph{x}, the function 
#' will return the most similar element in \emph{y} using the \code{\link[stringdist]{stringdist}} function.}
#' @example {
#' 
#' require(fieldRS)
#' 
#' # ground truth data
#' data(fieldData)
#' 
#' # label count check (original)
#' unique.crop <- labelCheck(fieldData$crop)
#' unique.crop$label.count.plot
#' 
#' # new classes
#' nc <- c("wheat", "not-wheat", "not-wheat"))
#' 
#' # label correction
#' corrected.labels <- labelCheck(fieldData$crop, unique.crop$labels, nc)
#' corrected.labels$label.count.plot
#' 
#' }
#' @seealso \code{\link{extractFields}}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

labelCheck <- function(x, y, z, auto=FALSE) {
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  correct <- FALSE # default setting
  
  # check original labels
  if (!is.character(x)) {stop('"x" is not a character vector')}
  
  # check if y and z are provided (manual correction)
  if (!missing(y) & !missing(z) & !auto) {
    if (sum(duplicated(y)) > 0) {stop('duplicated records in "y"')}
    if (length(y) != length(z)) {stop('"y" and "z" have different lengths')}
    correct <- TRUE
  }
  
  # check if y and auto are set (automatic correction)
  if (!missing(y) & auto==TRUE) {
    z <- unique(y) # retrieve unique labels in y (to coorect with)
    y <- unique(x) # retireve unique labels in x (to correct)
    z <- sapply(y, function(l) {z[which.min(stringdist(l, z))[1]]})
    correct <- TRUE
  } 
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. identify / correct unique labels and count unique values
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # no correction needed (return unique values)
  if (!correct) {
    s.labels <- unique(x)
    if (sum(is.na(s.labels)) > 0) {stop('NA values found in shapefile (please fix before proceeding)')}
    count <- data.frame(count=sapply(s.labels, function(l) {sum(x==l, na.rm=TRUE)}), label=unique(s.labels))
  }
  
  # correction needed (update original labels with corrected values)
  if (correct) {
    s.labels <- x
    for (l in 1:length(y)) {
      i <- which(x == y[l])
      if (length(i) > 0) {s.labels[i] <- z[l]}}
    count <- data.frame(count=sapply(unique(z), function(l) {sum(s.labels==l, na.rm=TRUE)}), label=unique(s.labels))
  }
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. derive plot with unique labels per
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  p <- ggplot(count, aes_string(x="label", y="count")) + geom_bar(stat="identity") + theme_bw() + xlab("\nLabel") + ylab("Frequency (Nr.)\n") +
    theme(axis.text.x=element_text(angle=45, hjust=1), axis.title=element_text(size=12, face="bold"),
          axis.text=element_text(size=10), legend.title=element_text(size=12, face="bold"), legend.position="bottom")
  
#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. return output as a list
#-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # derive output
  return(list(labels=s.labels, label.count=count, label.count.plot=p))
  
}
