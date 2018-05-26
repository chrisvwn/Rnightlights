library(testthat)
library(Rnightlights)

.runThisTest <- Sys.getenv("RunAllTests") == "yes"

if(.runThisTest)
{
  nlTilesUrlOLS <- pkgOptions("ntLtsIndexUrlOLS.Y")
  
  nlTilesUrlVIIRS <- pkgOptions("ntLtsIndexUrlVIIRS.D")
  
  nlTilesOrig <- read.csv("nltiles.csv", header = T, stringsAsFactors = F)
  nlTilesOrigVIIRS <- nlTilesOrig[grep("VIIRS", nlTilesOrig$type),]
  nlTilesOrigOLS <- nlTilesOrig[grep("OLS", nlTilesOrig$type),]
  
  nlTilesVIIRS <- Rnightlights:::getNlTiles("VIIRS.M")
  nlTilesOLS <- Rnightlights:::getNlTiles("OLS.Y")
  
  noaaDownloadSiteIsAvailable <- function(dnldSite="www.ngdc.noaa.gov")
  {
    !as.logical(system(paste("ping -n -c 1 ", dnldSite)))
  }
  
  noaaIndexUrlIsAvailableOLS <- function(indexUrl=pkgOptions("ntLtsIndexUrlOLS"))
  {
    !as.logical(download.file(pkgOptions("ntLtsIndexUrlOLS"), tempfile()))
  }
  
  noaaIndexUrlIsAvailableVIIRS <- function(indexUrl=pkgOptions("ntLtsIndexUrlVIIRS"))
  {
    !as.logical(download.file(pkgOptions("ntLtsIndexUrlOLS"), tempfile()))
  }
}
