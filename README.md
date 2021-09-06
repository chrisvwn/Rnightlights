# Package outdated
## It seems there have been a number of changes that have happened upstream especially around tile downloading that render the package unusable. However, I have not had the time to keep the package up-to-date and do not see this changing in the near future. <br><br> Please find an alternative package/software for your use-case.

# Rnightlights [![Build Status](https://travis-ci.org/chrisvwn/Rnightlights.svg?branch=master)](https://travis-ci.org/chrisvwn/Rnightlights) [![codecov](https://codecov.io/gh/chrisvwn/Rnightlights/branch/master/graph/badge.svg)](https://codecov.io/gh/chrisvwn/Rnightlights) [![Monthly Downloads](http://cranlogs.r-pkg.org/badges/Rnightlights)](http://cranlogs.r-pkg.org/badges/Rnightlights) [![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/Rnightlights)](http://cranlogs.r-pkg.org/badges/grand-total/Rnightlights)

## Nightlights Data Extraction in R

<hr>

### NOTE:
1. I am not a GIS expert. This package arose from the difficulties I encountered figuring out satellite nighttime lights during my Masters. You can read the publication here: [https://www.researchgate.net/publication/306129326_Constructing_spatiotemporal_poverty_indices_from_big_data](https://www.researchgate.net/publication/306129326_Constructing_spatiotemporal_poverty_indices_from_big_data)
2. This package is still in beta. It is provided as-is for whatever use you may find for it. It may or may not function as you expect it to.
3. Background removal/gas flare removal has been added but is still under testing.
4. Apparently the monthly VIIRS tiles (VIIRS.M) are no longer provided for free. Daily VIIRS tiles (VIIRS.D) are only available for approximately the immediate previous 2 months. The annual tiles (VIIRS.Y) are only available for the years 2015 & 2016.

<hr>

The Rnightlights package extracts country rasters and zonal statistics from satellite nightlight rasters downloaded from the United States National Oceanic and Atmospheric Administration (<http://www.noaa.gov>) free data repositories. 

The package supports DMSP-OLS annual and SNPP-VIIRS daily, monthly and annual (yearly) nightlight raster data. Satellite nightlight raster tiles are downloaded and cropped to the country boundaries using shapefiles from the GADM database of Global Administrative Areas (<http://gadm.org>). Specified zonal statistics are then calculated at the selected administrative level(s) for the selected country(ies) and cached locally for future retrieval. Finally, an experimental gui is included that allows one to download, process, analyze and visualize the data.

**NOTE: Rnightlights is still in beta and may change drastically in ways that may break previous functionality and interfaces**

## Installation

R package Rnightlights is available on CRAN and can be installed in R as:

```
install.packages("Rnightlights")

```

To install the development version run:

```
devtools::install_github("chrisvwn/Rnightlights")

```

NOTE: On Linux the following software pre-requisites are required:

* libcurl
  * deb: libcurl4-openssl-dev (Debian, Ubuntu, etc)
  * rpm: libcurl-devel (Fedora, CentOS, RHEL)
  * csw: libcurl_dev (Solaris)
 
* openssl
  * deb: libssl-dev (Debian, Ubuntu, etc)
  * rpm: openssl-devel (Fedora, CentOS, RHEL)
  * csw: libssl_dev (Solaris)
  * brew: openssl@1.1 (Mac OSX)
 
* gdal >= 1.11.4
  * deb: gdal-dev (Debian, Ubuntu, etc) (deb: libgdal-dev)
  * rpm: gdal-devel (Fedora, CentOS, RHEL)
  * csw: gdal_dev (Solaris)

* libxml-2.0
  * deb: libxml2-dev (Debian, Ubuntu, etc)
  * rpm: libxml2-devel (Fedora, CentOS, RHEL)
  * csw: libxml2_dev (Solaris)

* aria2 (Optional) For faster tile downloads

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

library(Rnightlights)
library(lubridate)
library(reshape2)

#(Optional performance enhancement if you have aria2c and gdal installed)
#pkgOptions(downloadMethod = "aria", cropMaskMethod = "gdal", extractMethod = "gdal", deleteTiles = TRUE)

#Optional performance enhancement. If extractMethod="rast" you can specify the number of
#CPU cores to use in parallel
#pkgOptions(extractMethod = "rast", numCores=4)

ctry <- "KEN" #replace to run for any other country

#download and process monthly VIIRS stats at the highest admin level
highestAdmLevelStats <- getCtryNlData(ctryCode = ctry, 
                                     admLevel = "highest",
                                     nlType = "VIIRS.M", 
                                     nlPeriods = nlRange("201401", "201412","VIIRS.M"), 
                                     nlStats = list("sum",na.rm=TRUE),
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

#plot admin level sums for the year
g <- ggplot(data = highestAdmLevelStats, 
            aes(x=nlPeriod, y=radiancesum, 
                color=highestAdmLevelStats[[2]])) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
  geom_line()+geom_point() + labs(color = names(highestAdmLevelStats)[2]) + 
  xlab("Month") + 
  ylab("Sum of Radiances") +
  ggtitle(paste0(unique(names(highestAdmLevelStats)[2]), " sum of radiances for ", ctry))

print(g)

```
## Performance enhancements
Some performance enhancements are available to speed up processing speeds and they fall into 2 classes:

a. **Parallel processing**: The most straightforward way to speed up processing is by increasing the number of CPU cores used to calculate the zonal statistics. By default only one CPU core is used. This option is available out of the box and is enabled by setting the `numCores` package option e.g. to process in parallel across 4 CPU cores run `pkgOptions(numCores=4)` before processing. Note numCores is only used when: `pkgOptions(extractMethod="rast")` which is the default.

b. **GDAL**: The GDAL tools provide tools that are often much faster than the usual processing workflow. For example, zonal statistics can be calculated much faster using GDAL than looping over sub-polygons even with a number of CPUs in parallel. GDAL tools are provided by the `rgdal` and `gdalUtils` packages, however, they require the GDAL software to be installed in the operating system. GDAL can be enabled at 2 stages:
    i)  when cropping rasters by setting `pkgOptions(cropMaskMethod="gdal")` and
    ii) when calculating zonal stats by setting `pkgOptions(extractMethod="gdal")`
    
**Process multiple countries at once**

`processNlData` is the function to call to process multiple countries or admLevels which `getCtryNlData` cannot do. The other difference is that processNlData does not return any value - instead all processed data is cached and can then be retrieved using multiple calls to `getCtryNlData`. For example to process various admLevels in East Africa you could run:

```
processNlData(ctryCodes = list("KEN","TZA","UGA"), admLevels = list(list("county","constituency","ward"),list("adm0","adm1"), "adm1"), nlTypes = "OLS.Y", nlPeriods = "2013", nlStats = list("sum","na.rm=T"))
```

Note the matching ctryCodes and admLevels. Also note the `nlStats` parameter now can pass arguments to the functions and *DOES NOT* default to `na.rm=TRUE`.

**Browse the cached data with the internal Shiny app**

```
gui() #still under construction

```
