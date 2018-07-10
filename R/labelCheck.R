#' @title labelCheck
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#' @description helps fix spelling mistakes in the labels of a set of samples.
#' @param original.labels Vector of class \emph{character}.
#' @param unique.original.labels Vector of class \emph{character}.
#' @param unique.corrected.labels Vector of class \emph{character}.
#' @importFrom ggplot2 aes_string geom_bar theme_bw theme xlab ylab element_text
#' @details {If \emph{unique.original.labels} and \emph{unique.corrected.labels} are missing, the function will return the unique values among
#' all the elements of \emph{unique.original.labels}. Otherwise, the function will provide a corrected copy of \emph{unique.original.labels}.
#' Aditionally, the function will count the number of records for each of the unique labels from which a plot will be built. The final output
#' consists of:
#' \itemize{
#'  \item{\emph{unique.labels} - Unique labels in the output.}
#'  \item{\emph{corrected.labels} - Corrected labels in \emph{original.labels}.}
#'  \item{\emph{label.count} - Count of occurrences in \emph{unique.labels} per each element in \emph{original.labels}.}
#'   \item{\emph{label.count.plot} - Plot of \emph{label.count}.}}
#' }
#' @return A \emph{character} vector.
#' @seealso \code{\link{assignClass}}
#' @export

#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#

labelCheck <- function(original.labels, unique.original.labels, unique.corrected.labels) {

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 1. check variables
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # check original labels
  if (!is.character(original.labels)) {stop('"original.labels" is not a character vector')}

  # compare original and corrected labels
  if (exists("unique.original.labels") & !exists("unique.corrected.labels")) {
    stop('"unique.original.labels" provided but missing "unique.corrected.labels" (Remove or assign both)')}
  if (exists("unique.original.labels") & !exists("unique.corrected.labels")) {
    if (sum(duplicated(unique.original.labels)) > 0) {stop('duplicated records in "unique.original.labels"')}
    if (sum(duplicated(unique.original.labels)) > 0) {stop('duplicated records in "unique.original.labels"')}
    if (length(unique.original.labels) != length(unique.corrected.labels)) {
      stop('"unique.original.labels" and "unique.corrected.labels" have different lengths')}
    correct = TRUE}
  if (!exists("unique.original.labels") & !exists("unique.corrected.labels")) {correct <- FALSE}

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 2. identify / correct unique labels
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # no correction needed (return unique values)
  if (!correct) {
    unique.labels <- unique(original.labels)
    if (sum(is.na(unique.labels)) > 0) {stop('NA values found in shapefile (please fix before proceeding)')}}

  # correction needed (update original labels with corrected values)
  if (correct) {
    for (l in 1:length(unique.original.labels)) {
      i <- which(original.labels == original.labels[l])
      if (length(i) > 0) {original.labels[i] <- unique.corrected.labels[l]}}
    unique.labels <- unique.corrected.labels}

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 3. count unique values
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  count <- data.frame(count=sapply(unique.labels, function(l) {sum(original.labels==l, na.rm=TRUE)}), label=unique.labels)

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 4. derive plot with unique labels per
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  p <- ggplot(count, aes_string(x="label", y="count")) + geom_bar(stat="identity") + theme_bw() + xlab("Label") + ylab("Frequency (Nr)") +
    theme(axis.text.x=element_text(angle=45, hjust=1), axis.title=element_text(size=12, face="bold"),
          axis.text=element_text(size=10), legend.title=element_text(size=12, face="bold"), legend.position="bottom")

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# 5. return output as a list
#-----------------------------------------------------------------------------------------------------------------------------------------------#

  # derive output
  return(list(unique.labels=unique.labels, label.count=count, label.count.plot=p))

}
