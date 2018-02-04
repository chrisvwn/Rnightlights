library(testthat)
library(Rnightlights)
library(rgdal)
library(raster)


ctryPoly <- readCtryPolyAdmLayer("STP", "STP_adm2", polyType = "rds")

ctryRaster <- raster::raster("STP_OLS_1992.tif")

temp <- NULL

admSum <- NULL

for (i in 1:length(ctryPoly@polygons))
{
  temp$name <- as.character(ctryPoly@data$NAME_2[i])
  temp$sum <- sum(masqOLS(ctryPoly, ctryRaster, i), na.rm=T)
  admSum <- rbind(admSum)
}
