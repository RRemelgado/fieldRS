% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rankPlots.R
\name{rankPlots}
\alias{rankPlots}
\title{rankPlots}
\usage{
rankPlots(
  x,
  y,
  z,
  min.size = 1,
  priority = c("diversity", "richness", "patch_count", "pixel_frequency",
    "road_distance")
)
}
\arguments{
\item{x}{Object of class \emph{RasterLayer}, \emph{RasterStack} or \emph{RasterBrick}.}

\item{y}{Object of class \emph{SpatialPolygons} or \emph{SpatialPolygonsDataFrame}.}

\item{z}{Object of class \emph{SpatialLines} or \emph{SpatialLinesDataFrame}.}

\item{min.size}{Numeric element.}

\item{priority}{Character vector.}
}
\value{
A list.
}
\description{
helps fix spelling mistakes in the labels of a set of samples.
}
\details{
{For each polygon in \emph{y}, the function will determine the distance between its centroid and
the nearest road provided through \emph{z}, count the number of classes in \emph{x} and the number of patches
of connected pixels and report on the proportion of non NA values. The patch count can be restricted to those
with a size greater \emph{min.size} which specifies the minimum number of pixels per patch. Then, the function
will use this data to rank the elements of \emph{y} according to the order of the keywords in \emph{priority}.
The user can choose one or more of the following keywords:
 \itemize{
 \item{\emph{diversity} - Priority given to the highest Shannon, class diversity.}
 \item{\emph{richness} - Priority given to the highest class richness (number of classes in plot / total number of classes).}
 \item{\emph{pixel_frequency} - Priority given to the highest non-NA pixel count.}
 \item{\emph{patch_count} - Priority given to the highest patch count.}
 \item{\emph{road_distance} - Priority given to shortest distance.}}
The final output is a \emph{data.frame} reporting on:
 \itemize{
 \item{\emph{x} - Polygon centroid x coordinate.}
 \item{\emph{y} - Polygon centroid y coordinate.}
 \item{\emph{mape} - Mean Absolute Percent Error.}
 \item{\emph{diversity} - Class diversity.}
 \item{\emph{richness} - Class richeness.}
 \item{\emph{pixel.frequency} - Number of non-NA pixels.}
 \item{\emph{road.distance} - Linear distance to the closest road.}
 \item{\emph{ranking} - Priority ranking}}}
}
\examples{
{

require(raster)
require(RStoolbox)
require(ggplot2)

# read raster data
r <- brick(system.file("extdata", "ndvi.tif", package="fieldRS"))

# read road information
data(roads)

# unsupervised classification with kmeans
uc <- unsuperClass(r, nSamples=5000, nClasses=5)$map

# derive potential sampling plots
pp <- derivePlots(uc, 1000)

# plot ranking
pp@data <- rankPlots(uc, pp, roads)

# plot output
gp <- fortify(pp, region="ranking")
ggplot(gp, aes(x=long, y=lat, group=group, fill=as.numeric(id))) + 
geom_polygon() + scale_fill_continuous(name="Ranking")

}
}
\seealso{
\code{\link{derivePlots}} \code{\link{ccLabel}}
}
