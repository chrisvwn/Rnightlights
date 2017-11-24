# Rnightlights [![Build Status](https://travis-ci.org/chrisvwn/Rnightlights.svg?branch=master)](https://travis-ci.org/chrisvwn/Rnightlights)

## Nightlights Data Extraction in R

The Rnightlights package extracts rasters and zonal statistics from satellite nightlight rasters downloaded from the United States National Oceanic and Atmospheric Administration (<http://www.noaa.gov>) free data repositories. 

Both the DMSP-OLS annual and SNPP-VIIRS monthly nightlight raster data are supported. Satellite nightlight raster tiles are downloaded and cropped to the country boundaries using shapefiles from the GADM database of Global Administrative Areas (<http://gadm.org>). Zonal statistics are then calculated at the lowest administrative boundary for the selected country and cached locally for future retrieval. Finally, a simple data explorer/browser is included that allows one to visualize the cached data e.g. graphing, mapping and clustering regional data.

## Installation

R package Rnightlights is available on CRAN and can be installed in R as:

```
install.packages('Rnightlights')
```

To install the development version run:

````
devtools::install_github("chrisvwn/Rnightlights")
````

### Example

An example to process VIIRS monthly nightlights for Kenya for the year 2014 from the
[Ishara Data blog post](http://isharadata.blogspot.co.ke/2017/09/rnightlights-satellite-nightlight-data.html)

####Notes
1. Change the "ctry" variable to run the example on another country
2. This will not work for countries without admin levels below the country level e.g. ATA (Antarctica)
3. This calculates total radiances per region and so may be biased by area. Normalize by area to see 
        regions with higher radiances per unit area e.g. to estimate areas with higher economic activity

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

#download and process stats
lowestAdmLevelStats <- getCtryNlData(ctryCode = ctry, 
                                     nlPeriods = nlRange("201401", "201412"), 
                                     nlType = "VIIRS", 
                                     stats = "sum",
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


#(optionally plot the data)
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


#browse the cached data with internal Shiny app
exploreData()

```