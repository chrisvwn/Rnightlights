library(testthat)
library(Rnightlights)
library(rgdal)
library(raster)

.runThisTest <- Sys.getenv("RunAllTests") == "yes"

if(.runThisTest)
{
  ctryPoly <- readRDS("STP_adm_shp.RDS")
  
  ctryPoly <- ctryPoly[[2]]
  
  ctryRaster <- raster::raster("NL_STP_OLS.Y_1992_STABLE_LIGHTS_GADM-2.8-SHPZIP.tif")
  
  temp <- NULL
  
  admSumMasqOLS <- NULL
  
  for (i in 1:length(ctryPoly@polygons))
  {
    temp$name <- as.character(ctryPoly@data$NAME_2[i])
    temp$sum <- sum(Rnightlights:::masqOLS(shp = ctryPoly, rast = ctryRaster, i = i, configName = pkgOptions(paste0("configName_","OLS.Y")))$vals, na.rm=T)
    admSumMasqOLS <- rbind(admSumMasqOLS, temp$sum)
  }
  
  admSumMasqOLS <- sum(admSumMasqOLS)
  
  #prepare masqVIIRS
  ctryRaster <- raster::raster("NL_STP_VIIRS.M_201401_VCMCFG_GADM-2.8-SHPZIP.tif")
  
  temp <- NULL
  
  admSumMasqVIIRS <- NULL
  
  for (i in 1:length(ctryPoly@polygons))
  {
    temp$name <- as.character(ctryPoly@data$NAME_2[i])
    temp$sum <- sum(Rnightlights:::masqVIIRS(ctryPoly, ctryRaster, i)$vals, na.rm=T)
    admSumMasqVIIRS <- rbind(admSumMasqVIIRS, temp$sum)
  }
  
  admSumMasqVIIRS <- sum(admSumMasqVIIRS)
}
