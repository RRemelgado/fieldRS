## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, message=FALSE-------------------------------------------
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

## ----message=FALSE-------------------------------------------------------
plot.grid <- derivePlots(fieldData, 1000)
plot(ndvi.ts[[1]]) # plot 1st NDVI image
plot(plot.grid, add=TRUE) # overlap sampling plot grid

## ----message=FALSE-------------------------------------------------------
k.img <- unsuperClass(ndvi.ts, nSamples=5000, nClasses=5)$map
plot(k.img)

## ----message=FALSE-------------------------------------------------------
# example 1
plot.grid_1 <- plot.grid
plot.grid_1@data <- rankPlots(k.img, plot.grid, roads, priority=c('class_count', 'patch_count', 'road_distance'))
kable_styling(kable(head(plot.grid_1@data, 5), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive")

# example
plot.grid_2 <- plot.grid
plot.grid_2@data <- rankPlots(k.img, plot.grid, roads, priority=c('road_distance', 'class_count', 'patch_count'))
kable_styling(kable(head(plot.grid_2@data, 5), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive")

## ---- out.width="98%", fig.height=5, fig.width=10, dpi=600, fig.align="center", fig.show='hold', echo=FALSE----
gp <- fortify(plot.grid_1, region="ranking")
ggplot(gp, aes(x=long, y=lat, group=group, fill=as.numeric(gp$id))) + geom_polygon() + scale_fill_continuous(name="Ranking")

gp <- fortify(plot.grid_1, region="ranking")
ggplot(gp, aes(x=long, y=lat, group=group, fill=as.numeric(gp$id))) + geom_polygon() + scale_fill_continuous(name="Ranking")

## ----message=FALSE-------------------------------------------------------
ndvi.max <- calc(ndvi.ts, max, na.rm=TRUE) # derive maximum NDVI composite)
seg.img <- ccLabel(ndvi.max, method="spatial", change.threshold=5)$regions # segment NDVI image
plot(seg.img)

## ----message=FALSE-------------------------------------------------------
seg.img <- erosionFilter(seg.img)

## ----message=FALSE-------------------------------------------------------
fields <- extractFields(seg.img)

## ----echo=FALSE----------------------------------------------------------
plot(seg.img)
plot(fields, border="red", add=TRUE)

## ------------------------------------------------------------------------
unique.crop <- labelCheck(fieldData$crop)
kable_styling(kable(head(unique.crop$label.count, 3), format="html", align="c", full_width=TRUE), "stripped", bootstrap_options="responsive") # label frequency
plot(unique.crop$label.count.plot) # show label frequency plot
unique.crop$labels # show unique labels

## ----message=FALSE-------------------------------------------------------
corrected.labels <- labelCheck(fieldData$crop, unique.crop$labels, c("wheat", "not-wheat", "not-wheat"))
fieldData$crop_2 <- corrected.labels$labels

