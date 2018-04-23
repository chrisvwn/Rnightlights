---
title: 'Nightlights Imagery Data Extraction in R'
tags:
  - satellite imagery
  - nightlights
  - data extraction
authors:
 - name: Christopher Njuguna
   orcid: 0000-0001-9691-8008
date: 23 April 2017
---

# Summary

The Rnightlights package extracts raster and zonal statistics from satellite nightlight rasters downloaded from the United States National Oceanic and Atmospheric Administration (<http://www.noaa.gov>) free data repositories. 

Both the DMSP-OLS annual and SNPP-VIIRS monthly nightlight raster data are supported. Satellite nightlight raster tiles are downloaded and cropped to the country boundaries using shapefiles from the GADM database of Global Administrative Areas (<http://gadm.org>). Zonal statistics are then calculated at the lowest administrative boundary for the selected country and cached locally for future retrieval. Finally, a simple data explorer/browser is included that allows one to visualize the cached data e.g. graphing, mapping and clustering regional data.

Researchers often spend an inordinate amount of time gathering and pre-processing the data they require for their projects. This is especially true while dealing with geospatial imagery which requires special knowledge of the domain, data types and tools. For illustration, if one needed a monthly time series of nightlight radiance sums for, say, Kenya aggregated at the county level for the year 2014 one would have to follow something akin to these steps:

1. Identify and download administrative level country boundaries for Kenya. The polygon administrative boundaries can be obtained from http://gadm.org

2. Identify the tiles required for Kenya. This can be obtained by getting the geospatial extents of Kenya and finding which VIIRS tiles intersect them.

3. Locate and download the required tiles. Research would show they can be downloaded from https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html

4. For each month in 2014:

    * load the tiles and Kenya county polygons
    
    * Calculate the sum of nightlights at the county level. In QGIS you can use the “zonal statistics” function
    
    * Save the output as a CSV

5. Load the CSVs in R and perform the analysis

This can be reduced to a few lines of code in R using the Rnightlights package. For this example one can run:

a) In Rnightlights version 0.2.0 and later calculate directly at the county level with one line:

```
library(Rnightlights)

kenyaCounties <- getCtryNlData(ctryCode = "KEN", admLevel = "county", nlType = "VIIRS.M", nlPeriods = nlRange("201401", "201412"), stats = "sum")
```

b) In Rnightlights versions pre-0.2.0, calculate at the lowest admin level and aggregate to the county level:

```
library(Rnightlights)
library(reshape2)
library(lubridate)

#get satellite data at the lowest admin level in the country i.e. ward
kenyaWards <- getCtryNlData(ctryCode = "KEN", nlType = "VIIRS.M", nlPeriods = nlRange("201401", "201412"), stats = "sum")

#melt the ward radiances in preparation for aggregation
kenyaWardsMelted <-  melt(kenyaWards, 
                          id.vars = grep("NL_", names(kenyaWards), 
                                         invert=TRUE), 
                          variable.name = "nlPeriod", 
                          value.name = "radiancesum")
                                  
#convert the period string into a date string
kenyaWardsMelted$nlPeriod <- substr(kenyaWardsMelted$nlPeriod, 10, 15)

#aggregate radiance sums to the county level
kenyaCounties <- setNames(aggregate(kenyaWardsMelted$radiancesum, 
                            by=list(kenyaWardsMelted$county, kenyaWardsMelted$nlPeriod),
                            FUN=sum, na.rm=T),
                            c("county", "month", "sumradiances"))
```

# References
[Rnightlights Github repository](http://github.com/chrisvwn/Rnightlights)