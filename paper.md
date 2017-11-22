---
title: 'Nightlights for R'
tags:
  - satellite imagery
  - nightlights
  - data extraction
authors:
 - name: Christopher Njuguna
   orcid: 0000-0001-9691-8008
date: 22 November 2017
---

# Summary

The Rnightlights package extracts raster and zonal statistics from satellite nightlight rasters downloaded from the United States National Oceanic and Atmospheric Administration (<http://www.noaa.gov>) free data repositories. 

Both the DMSP-OLS annual and SNPP-VIIRS monthly nightlight raster data are supported. Satellite nightlight raster tiles are downloaded and cropped to the country boundaries using shapefiles from the GADM database of Global Administrative Areas (<http://gadm.org>). Zonal statistics are then calculated at the lowest administrative boundary for the selected country and cached locally for future retrieval. Finally, a simple data explorer/browser is included that allows one to visualize the cached data e.g. graphing, mapping and clustering regional data.

Researchers often spend an inordinate amount of time gathering and pre-processing the data they require for their projects. This is especially true while dealing with geospatial imagery which requires special knowledge of the domain, data types and tools. For illustration, if one needed a monthly time series of nightlight radiance sums for, say, Kenya aggregated at the county level for the year 2014 one would have to follow something akin to these steps:

1. Identify and download administrative level country boundaries for Kenya. The polygon administrative boundaries can be obtained from http://www.gadm.org

2. Identify the tiles required for Kenya. This can be obtained by getting the geospatial extents of Kenya and finding which VIIRS tiles intersect them.

3. Locate and download the required tiles. Research would show they can be downloaded from https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html

4. For each month in 2014:

    * load the tiles and Kenya county polygons
    
    * Calculate the sum of nightlights at the county level. In QGIS you can use the “zonal statistics” function
    
    * Save the output as a CSV

5. Load the CSVs in R and perform the analysis


This can be reduced to a few lines of code in R using the Rnightlights package. For this example one can run:


```
library(Rnightlights)

#(Optional)
pkgOptions(downloadMethod = "aria", cropMaskMethod = "gdal", extractMethod = "gdal", deleteTiles = TRUE)

#get data at the lowest admin level i.e. ward
kenyaWards <- getCtryNlData(ctryCode = "KEN", nlPeriods = nlRange("201401", "201412"), nlType = "VIIRS", stats = "sum")

#sum the ward radiances to the county level
kenyaWardsMelted <-  reshape2::melt(kenyaWards, value.name="sum")

kenyaCounties <- aggregate(kenyaWardsMelted$sum, by=list(kenyaWardsMelted$county), FUN=sum, na.rm=T))
```

# References
[Rnightlights Github repository](http://github.com/chrisvwn/Rnightlights)