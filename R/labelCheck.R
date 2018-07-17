#' @title labelCheck
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description helps fix spelling mistakes in the labels of a set of samples.
#' @param x Vector of class \emph{character}.
#' @param y Vector of class \emph{character}.
#' @param z Vector of class \emph{character}.
#' @importFrom ggplot2 aes_string geom_bar theme_bw theme xlab ylab element_text
#' @details {If \emph{y} and \emph{z} are missing, the function will return the unique values among
#' all the elements of \emph{y}. Otherwise, the function will provide a corrected copy of \emph{y}.
#' Aditionally, the function will count the number of records for each of the unique labels from which a plot will be built. The final output
#' consists of:
#' \itemize{
#'  \item{\emph{unique.labels} - Unique labels in the output.}
#'  \item{\emph{corrected.labels} - Corrected labels in \emph{x}.}
#'  \item{\emph{label.count} - Count of occurrences in \emph{unique.labels} per each element in \emph{x}.}
#'   \item{\emph{label.count.plot} - Plot of \emph{label.count}.}}
#' }
#' @return A \emph{character} vector.
#' @seealso \code{\link{assignClass}}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

labelCheck <- function(x, y, z) {
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 1. check variables
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # check original labels
  if (!is.character(x)) {stop('"x" is not a character vector')}
  
  if (!missing(y) & !missing(z)) {
    if (sum(duplicated(y)) > 0) {stop('duplicated records in "y"')}
    if (length(y) != length(z)) {stop('"y" and "z" have different lengths')}
    correct <- TRUE
  } else {correct <- FALSE}
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 2. identify / correct unique labels
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # no correction needed (return unique values)
  if (!correct) {
    s.labels <- unique(x)
    if (sum(is.na(s.labels)) > 0) {stop('NA values found in shapefile (please fix before proceeding)')}}
  
  # correction needed (update original labels with corrected values)
  if (correct) {
    for (l in 1:length(y)) {
      i <- which(x == y[l])
      if (length(i) > 0) {x[i] <- z[l]}}
    s.labels <- x}
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 3. count unique values
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  count <- data.frame(count=sapply(s.labels, function(l) {sum(x==l, na.rm=TRUE)}), label=s.labels)
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 4. derive plot with unique labels per
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  p <- ggplot(count, aes_string(x="label", y="count")) + geom_bar(stat="identity") + theme_bw() + xlab("\nLabel") + ylab("Frequency (NÂ°)\n") +
    theme(axis.text.x=element_text(angle=45, hjust=1), axis.title=element_text(size=12, face="bold"),
          axis.text=element_text(size=10), legend.title=element_text(size=12, face="bold"), legend.position="bottom")
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  # 5. return output as a list
  #-----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # derive output
  return(list(labels=s.labels, label.count=count, label.count.plot=p))
  
}
