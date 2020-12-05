library(testthat)
library(Rnightlights)
library(rgdal)
library(raster)

.runThisTest <- Sys.getenv("RunAllTests") == "yes"

if (.runThisTest)
{
  ctryPoly <- readRDS("STP_adm_shp.RDS")
  
  ctryPoly <- ctryPoly[[2]]
  
  ctryRaster <-
    raster::raster("NL_STP_OLS.Y_1992_STABLE_LIGHTS_GADM-2.8-SHPZIP.tif")
  
  temp <- NULL
  
  admSumMasqOLS <- NULL
  
  for (i in seq_along(ctryPoly@polygons))
  {
    temp$name <- as.character(ctryPoly@data$NAME_2[i])
    temp$sum <-
      sum(
        Rnightlights:::masqOLS(
          ctryPoly = ctryPoly,
          ctryRast = ctryRaster,
          idx = i,
          configName = pkgOptions(paste0("configName_", "OLS.Y"))
        ),
        na.rm = T
      )
    admSumMasqOLS <- rbind(admSumMasqOLS, temp$sum)
  }
  
  admSumMasqOLS <- sum(admSumMasqOLS)
  
  #prepare masqVIIRS
  ctryRaster <-
    raster::raster("NL_STP_VIIRS.M_201401_VCMCFG_GADM-2.8-SHPZIP.tif")
  
  temp <- NULL
  
  admSumMasqVIIRS <- NULL
  
  for (i in seq_along(ctryPoly@polygons))
  {
    temp$name <- as.character(ctryPoly@data$NAME_2[i])
    temp$sum <-
      sum(Rnightlights:::masqVIIRS(ctryPoly, ctryRaster, i), na.rm = T)
    admSumMasqVIIRS <- rbind(admSumMasqVIIRS, temp$sum)
  }
  
  admSumMasqVIIRS <- sum(admSumMasqVIIRS)
}
