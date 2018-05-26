# Rnightlights [![Build Status](https://travis-ci.org/chrisvwn/Rnightlights.svg?branch=master)](https://travis-ci.org/chrisvwn/Rnightlights) [![codecov](https://codecov.io/gh/chrisvwn/Rnightlights/branch/master/graph/badge.svg)](https://codecov.io/gh/chrisvwn/Rnightlights) [![Monthly Downloads](http://cranlogs.r-pkg.org/badges/Rnightlights)](http://cranlogs.r-pkg.org/badges/Rnightlights) [![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/Rnightlights)](http://cranlogs.r-pkg.org/badges/grand-total/Rnightlights)

## Nightlights Data Extraction in R

The Rnightlights package extracts country rasters and zonal statistics from satellite nightlight rasters downloaded from the United States National Oceanic and Atmospheric Administration (<http://www.noaa.gov>) free data repositories. 

The package supports DMSP-OLS annual and SNPP-VIIRS daily, monthly and annual (yearly) nightlight raster data. Satellite nightlight raster tiles are downloaded and cropped to the country boundaries using shapefiles from the GADM database of Global Administrative Areas (<http://gadm.org>). Specified zonal statistics are then calculated at the selected administrative level(s) for the selected country(ies) and cached locally for future retrieval. Finally, a simple data explorer/browser is included that allows one to visualize the cached data e.g. graphing, mapping and clustering regional data.

## Installation

R package Rnightlights is available on CRAN and can be installed in R as:

```
install.packages("Rnightlights")
```

To install the development version run:

````
devtools::install_github("chrisvwn/Rnightlights")
````

## Performance enhancements
Some performance enhancements are available to speed up processing speeds and they fall into 2 classes:

a. Parallel processing: The most straightforward way to speed up processing is by increasing the number of CPU cores used to process the raster data. By default only one CPU core is used. This option is available out of the box and is enabled by setting the `numCores` package option e.g. to process in parallel across 4 CPU cores run `pkgOptions(numCores=4)` before processing.

b. GDAL: The GDAL tools provide tools that are often much faster than the usual processing workflow. For example, zonal statistics can be calculated much faster using GDAL than looping over sub-polygons even with a number of CPUs in parallel. GDAL tools are provided by the `rgdal` and `gdalUtils` packages, however, they require the GDAL software to be installed in the operating system.

### Example

An example to process VIIRS monthly nightlights for Kenya for the year 2014 from the
[Ishara Data blog post](http://isharadata.blogspot.co.ke/2017/09/rnightlights-satellite-nightlight-data.html)

#### Notes
1. Change the "ctry" variable to run the example on another country
2. This will not work for countries without admin levels below the country level e.g. ATA (Antarctica)
3. This calculates total radiances per region and so may be biased by area. Normalize by area to see 
        regions with higher radiances per unit area e.g. to estimate areas with higher economic activity

**a) In Rnightlights version 0.2.0 and later calculate directly at the county level with one line:**

```{r}
#install.packages(“Rnightlights”)
#install.packages("lubridate")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("plotly")

library(Rnightlights)
library(lubridate)
library(reshape2)

#(Optional performance enhancement if you have aria2c and gdal installed)
#pkgOptions(downloadMethod = "aria", cropMaskMethod = "gdal", extractMethod = "gdal", deleteTiles = TRUE)

#Optional performance enhancement. If extractMethod="rast" you can specify the number of
#CPU cores to use in parallel
#pkgOptions(extractMethod = "rast", numCores=4)

# ctry <- "KEN" #replace to run for any other country

#download and process monthly VIIRS stats at the highest admin level
highestAdmLevelStats <- getCtryNlData(ctryCode = ctry, 
                                     admLevel = "highest",
                                     nlType = "VIIRS.M", 
                                     nlPeriods = nlRange("201401", "201412"), 
                                     nlStats = "sum",
                                     ignoreMissing=FALSE)

#Optionally plot the data
library(ggplot2)
library(plotly)

#melt the stats into key-value format for easy multi-line plotting with ggplot2
highestAdmLevelStats <- melt(highestAdmLevelStats,
                              id.vars = grep("NL_", names(highestAdmLevelStats), 
                                             invert=TRUE), 
                              variable.name = "nlPeriod", 
                              value.name = "radiancesum")

#extract date from the NL col names
highestAdmLevelStats$nlPeriod <- substr(highestAdmLevelStats$nlPeriod, 12, 17)

#format period as date
highestAdmLevelStats$nlPeriod <- ymd(paste0(substr(highestAdmLevelStats$nlPeriod, 1,4), 
                                               "-",substr(highestAdmLevelStats$nlPeriod, 5,6), "-01"))

#plot 2nd admin level sums for the year
g <- ggplot(data = highestAdmLevelStats, 
            aes(x=nlPeriod, y=radiancesum, 
                color=highestAdmLevelStats[[2]])) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
  geom_line()+geom_point() + labs(color = names(highestAdmLevelStats)[2]) + 
  xlab("Month") + 
  ylab("Sum of Radiances") +
  ggtitle(paste0(unique(names(highestAdmLevelStats)[2]), " sum of radiances for ", ctry))

print(g)

#quick conversion to interactive map with plotly
ggplotly(g)

```

**b) In Rnightlights versions pre-0.2.0:**

```{r}
#install.packages(“Rnightlights”)
#install.packages("reshape2")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("plotly")

library(Rnightlights)
library(reshape2)
library(lubridate)

ctry <- "KEN" #replace to do for any other country

#(Optional performance enhancement if you have aria2c and gdal installed)
#pkgOptions(downloadMethod = "aria", cropMaskMethod = "gdal", extractMethod = "gdal", deleteTiles = TRUE)

#Optional performance enhancement. If extractMethod="rast" you can specify the number of
#CPU cores to use in parallel
#pkgOptions(extractMethod = "rast", numCores=4)

#download and process monthly VIIRS stats at the lowest admin level
lowestAdmLevelStats <- getCtryNlData(ctryCode = ctry, 
                                     nlType = "VIIRS", 
                                     nlPeriods = nlRange("201401", "201412"), 
                                     nlStats = "sum",
                                     ignoreMissing=FALSE)
                                     
#melt the stats into key-value format
lowestAdmLevelStatsMelted <- melt(lowestAdmLevelStats, 
                                  id.vars = grep("NL_", names(lowestAdmLevelStats), 
                                                 invert=TRUE), 
                                  variable.name = "nlPeriod", 
                                  value.name = "radiancesum")

#reformat the period titles into semitime periods
lowestAdmLevelStatsMelted$nlPeriod <- substr(lowestAdmLevelStatsMelted$nlPeriod, 10, 15)

#aggregate the data to 2nd country admin level
highestAdmLevelStatsAgg <- setNames(aggregate(lowestAdmLevelStatsMelted$radiancesum, 
                                              by=list(lowestAdmLevelStatsMelted[[2]], 
                                                      lowestAdmLevelStatsMelted$nlPeriod), 
                                              FUN=sum, na.rm=T), 
                                    c(names(lowestAdmLevelStatsMelted)[2], "nlperiod", "sumradiancesums"))

#format period as date
highestAdmLevelStatsAgg$nlperiod <- ymd(paste0(substr(highestAdmLevelStatsAgg$nlperiod, 1,4), 
                                               "-",substr(highestAdmLevelStatsAgg$nlperiod, 5,6), "-01"))

#Optionally plot the data

library(ggplot2)
library(plotly)

#plot 2nd admin level sums for the year
g <- ggplot(data = highestAdmLevelStatsAgg, 
            aes(x=nlperiod, y=sumradiancesums, 
                color=highestAdmLevelStatsAgg[[1]])) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
  geom_line()+geom_point() + labs(color = names(highestAdmLevelStatsAgg)[1]) + 
  xlab("Month") + 
  ylab("Sum of Radiances") +
  ggtitle(paste0("Sum of radiances for ", ctry))

print(g)

#quick conversion to interactive map with plotly
ggplotly(g)
```

**Browse the cached data with the internal Shiny app**

```
exploreData()

```