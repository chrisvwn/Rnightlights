library(testthat)
library(Rnightlights)
library(data.table)

getCtryNlDataSTP <- function()
{
  testCtryCode <- "STP"
  testAdmLevel <- "STP_adm0"
  testDataFile <- Rnightlights:::getCtryNlDataFname(ctryCode = testCtryCode, admLevel = testAdmLevel)
  
  print(testDataFile)
  
  print(file.exists(testDataFile))
  
  #stpOrig <- utils::read.csv(testDataFile, header = T, stringsAsFactors = F)
  stpOrig <<- as.data.frame(data.table::fread(testDataFile))
  
  pkgReset()
  
  pkgOptions(deleteTiles=TRUE)
  
  stpRast <<- getCtryNlData(ctryCode = testCtryCode, nlType = "OLS", nlPeriods = "1992", nlStats = c("mean", "sum"), ignoreMissing=FALSE)
  
  gdalUtils::gdal_chooseInstallation()
  
  gdalPath <- getOption("gdalUtils_gdalPath")
  
  if(gdalPath != "")
  {
    if(system("aria2c -h", ignore.stdout = T, ignore.stderr = T) == 0)
      pkgOptions(downloadMethod="aria")
    
    pkgOptions(cropMaskMethod="gdal", extractMethod="gdal")
    
    stpGdal <<- getCtryNlData(ctryCode = testCtryCode, nlType = "VIIRS", nlPeriods = "201401", nlStats = c("mean", "sum"), ignoreMissing=FALSE)
  }
}