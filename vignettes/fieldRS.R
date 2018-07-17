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
library(rsMove)

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

