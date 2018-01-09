library(testthat)
library(Rnightlights)

nlTilesUrlOLS <- pkgOptions("ntLtsIndexUrlOLS")

nlTilesUrlVIIRS <- pkgOptions("ntLtsIndexUrlVIIRS")

nlTilesOrig <- read.csv("nltiles.csv", header = T, stringsAsFactors = F)

nlTiles <- Rnightlights:::getNlTiles()

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
