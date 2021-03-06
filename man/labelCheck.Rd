% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labelCheck.R
\name{labelCheck}
\alias{labelCheck}
\title{labelCheck}
\usage{
labelCheck(x, y, z, auto = FALSE)
}
\arguments{
\item{x}{Vector of class \emph{character}.}

\item{y}{Vector of class \emph{character}.}

\item{z}{Vector of class \emph{character}.}

\item{auto}{Logical argument. Default is FALSE.}
}
\value{
A \emph{character} vector.
}
\description{
helps fix spelling mistakes in the labels of a set of samples.
}
\details{
{If \emph{y} and \emph{z} are missing, the function will return the unique values among
all the elements of \emph{y}. Otherwise, the function will provide a corrected copy of \emph{y}.
Additionally, the function will count the number of records for each of the unique labels from 
which a plot will be built. The final output consists of:
\itemize{
 \item{\emph{unique.labels} - Unique labels in the output.}
 \item{\emph{corrected.labels} - Corrected labels in \emph{x}.}
 \item{\emph{label.count} - Count of occurrences in \emph{unique.labels} per each element in \emph{x}.}
  \item{\emph{label.count.plot} - Plot of \emph{label.count}.}}
If \emph{auto} is set to TRUE, the user can ignore \emph{z} to correct the existing labels. Instead, the 
user can provide all the potential cases through \emph{y}. Then, for each element in \emph{x}, the function 
will return the most similar element in \emph{y} using the \code{\link[stringdist]{stringdist}} function.}
}
\seealso{
\code{\link{extractFields}}
}
