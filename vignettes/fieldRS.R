## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
# load packages
library(fieldRS)
library(raster)
library(ggplot2)
library(knitr)
library(kableExtra)
library(RStoolbox)

## ----message=FALSE-------------------------------------------------------
data(fieldData) # ground truth data
data(roads) # road shapefile
data(referenceProfiles) # target crop types NDVI profiles
ndvi.ts <- brick(system.file("extdata", "ndvi.tif", package="fieldRS")) # NDVI raster time series

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
plot.grid <- derivePlots(fieldData, 1000)
plot(ndvi.ts[[1]]) # plot 1st NDVI image
plot(plot.grid, add=TRUE) # overlap sampling plot grid

## ----message=FALSE-------------------------------------------------------
k.img <- unsuperClass(ndvi.ts, nSamples=5000, nClasses=5)$map

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
plot(k.img)

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
# example 1
plot.grid_1 <- plot.grid
plot.grid_1@data <- rankPlots(k.img, plot.grid, roads, priority=c('diversity', 'richness', 'patch_count', 'road_distance'))
kable_styling(kable(head(plot.grid_1@data, 5), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive")

# example 2
plot.grid_2 <- plot.grid
plot.grid_2@data <- rankPlots(k.img, plot.grid, roads, priority=c('road_distance', 'patch_count'))
kable_styling(kable(head(plot.grid_2@data, 5), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive")

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
gp <- fortify(plot.grid_1, region="ranking")
ggplot(gp, aes(x=long, y=lat, group=group, fill=as.numeric(gp$id))) + geom_polygon() + scale_fill_continuous(name="Ranking")

gp <- fortify(plot.grid_1, region="ranking")
ggplot(gp, aes(x=long, y=lat, group=group, fill=as.numeric(gp$id))) + geom_polygon() + scale_fill_continuous(name="Ranking")

## ----message=FALSE-------------------------------------------------------
ndvi.max <- calc(ndvi.ts, max, na.rm=TRUE) # derive maximum NDVI composite)
seg.img <- ccLabel(ndvi.max, method="spatial", change.threshold=5)$regions # segment NDVI image

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
plot(seg.img)

## ----message=FALSE, eval=FALSE-------------------------------------------
#  seg.img <- pixFilter(seg.img, 1, "erosion")

## ----message=FALSE, echo=FALSE-------------------------------------------
seg.img <- raster(system.file("extdata", "segFilter.tif", package="fieldRS"))

## ----message=FALSE-------------------------------------------------------
fields <- extractFields(seg.img)

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
plot(seg.img)
plot(fields, border="red", add=TRUE)

## ----message=FALSE-------------------------------------------------------
fields <- extractFields(seg.img, method="complex")

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
plot(seg.img)
plot(fields, border="red", add=TRUE)

## ------------------------------------------------------------------------
unique.crop <- labelCheck(fieldData$crop)
unique.crop$labels # show unique labels

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
kable_styling(kable(head(unique.crop$label.count, 3), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive") # label frequency
plot(unique.crop$label.count.plot) # show label frequency plot

## ------------------------------------------------------------------------
corrected.labels <- labelCheck(fieldData$crop, unique.crop$labels, c("wheat", "not-wheat", "not-wheat"))
fieldData$crop_2 <- corrected.labels$labels

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
kable_styling(kable(head(corrected.labels$label.count, 3), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive") # label frequency
plot(corrected.labels$label.count.plot) # show label frequency plot

## ----message=FALSE, eval=FALSE-------------------------------------------
#  samples1 <- poly2sample(fieldData, seg.img, min.cover=50)

## ----echo=FALSE----------------------------------------------------------
data(samples1)

## ----echo=FALSE, message=FALSE-------------------------------------------
r <- rasterize(fieldData, seg.img)
samples1$id <- extract(r, samples1)
samples1 <- samples1[!is.na(samples1$id),]
rm(r)

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
ggplot(samples1@data, aes(x=x, y=y, color=cover)) + geom_point()

## ----message=FALSE-------------------------------------------------------
samples2 <- raster2sample(seg.img)

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
ggplot(samples2@data, aes(x=x, y=y, color=cover)) + geom_point()

## ----eval=FALSE----------------------------------------------------------
#  predictor.df <- as.data.frame(extract(ndvi.ts, samples1)) # extracted values
#  ids <- unique(samples1$id) # polygon id's
#  predictor.df <- do.call(rbind, lapply(ids, function(u) {
#    i <- which(samples1$id == u)
#    return(as.vector(apply(predictor.df[i,], 2, mean, na.rm=TRUE)))})) # summarize on field level
#  crop.types <- fieldData$crop_2[ids] # crop type vector
#  predictive.model <- classModel(as.data.frame(predictor.df), crop.types, ids)

## ---- echo=FALSE---------------------------------------------------------
predictor.df <- as.data.frame(extract(ndvi.ts, samples1)) # extracted values
ids <- unique(samples1$id) # polygon id's
predictor.df <- do.call(rbind, lapply(ids, function(u) {
  i <- which(samples1$id == u)
  return(as.vector(apply(predictor.df[i,], 2, mean, na.rm=TRUE)))})) # summarize on field level
crop.types <- fieldData$crop_2[ids] # crop type vector
data(predictive.model1)
data(predictive.model2)


## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
kable_styling(kable(head(predictive.model2, 3), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive")
fieldData <- fieldData[ids,]
plot(fieldData)
plot(fieldData[!predictive.model1,], col="red", add=TRUE)

