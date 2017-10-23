# Rnightlights [![Build Status](https://travis-ci.org/chrisvwn/Rnightlights.svg?branch=master)](https://travis-ci.org/chrisvwn/Rnightlights)

## Nightlights for R

The Rnightlights package extracts raster and zonal statistics from satellite nightlight rasters downloaded from the United States National Oceanic and Atmospheric Administration (<http://www.noaa.gov>) free data repositories. 

Both the DMSP-OLS annual and SNPP-VIIRS monthly nightlight raster data are supported. Satellite nightlight raster tiles are downloaded and cropped to the country boundaries using shapefiles from the GADM database of Global Administrative Areas (<http://gadm.org>). Zonal statistics are then calculated at the lowest administrative boundary for the selected country and cached locally for future retrieval. Finally, a simple data explorer/browser is included that allows one to visualize the cached data e.g. graphing, mapping and clustering regional data.

## Installation

R package Rnightlights is available on CRAN and can be installed in R as:

```
install.packages('Rnightlights')
```

### Example

An example to process VIIRS monthly nightlights for Kenya for the year 2014

```
install.packages(“Rnightlights”)

library(Rnightlights)

#(Optional performance enhancement if you have aria2c and gdal installed)
pkgOptions(downloadMethod = "aria", cropMaskMethod = "gdal", extractMethod = "gdal", deleteTiles = TRUE) 

kenyaWards <- getCtryNlData(ctryCode = "KEN", nlPeriods = nlRange("201401", "201412"), nlType = "VIIRS", stats = "sum")

kenyaWardsMelted <-  reshape2::melt(kenyaWards, value.name="sum")

kenyaCounties <- aggregate(kenyaWardsMelted$sum, by=list(kenyaWardsMelted$county), FUN=sum, na.rm=T))
```