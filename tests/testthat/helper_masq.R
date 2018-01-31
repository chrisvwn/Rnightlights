library(testthat)
library(Rnightlights)
library(rgdal)
library(raster)

file.copy("STP_adm_shp_test.zip", "STP_adm_shp.zip")

utils::unzip("STP_adm_shp.zip", exdir = ".", )

ctryPoly <- rgdal::readOGR("STP_adm_shp", "STP_adm2")

ctryRaster <- raster::raster("STP_OLS_1992.tif")

temp <- NULL

admSum <- NULL

for (i in 1:length(ctryPoly@polygons))
{
  temp$name <- as.character(ctryPoly@data$NAME_2[i])
  temp$sum <- sum(masqOLS(ctryPoly, ctryRaster, i), na.rm=T)
  admSum <- rbind(admSum)
}
