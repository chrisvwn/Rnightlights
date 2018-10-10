library(testthat)
library(Rnightlights)
library(data.table)

getCtryNlDataSTP <- function()
{
  testCtryCode <- "STP"
  testAdmLevel <- "STP_adm0"
  testDataFile <- Rnightlights:::getCtryNlDataFname(ctryCode = testCtryCode, admLevel = testAdmLevel, gadmVersion = "2.8")
  
  print(testDataFile)
  
  print(file.exists(testDataFile))
  
  #stpOrig <- utils::read.csv(testDataFile, header = T, stringsAsFactors = F)
  stpOrig <<- as.data.frame(data.table::fread(testDataFile))
  
  pkgReset()
  
  pkgOptions(deleteTiles=TRUE)
  
  stpRast <<- getCtryNlData(ctryCode = testCtryCode, nlTypes = "OLS.Y", nlPeriods = "1992", nlStats = list(list("mean","na.rm=T"), list("sum","na.rm=T")), ignoreMissing=FALSE)
  
  gdalUtils::gdal_chooseInstallation()
  
  gdalPath <- getOption("gdalUtils_gdalPath")
  
  if(gdalPath != "")
  {
    if(system("aria2c -h", ignore.stdout = T, ignore.stderr = T) == 0)
      pkgOptions(downloadMethod="aria")
    
    pkgOptions(cropMaskMethod="gdal", extractMethod="gdal")
    
    stpGdal <<- getCtryNlData(ctryCode = testCtryCode, nlTypes = "VIIRS.M", nlPeriods = "201401", nlStats = list(list("mean",na.rm=T), list("sum",na.rm=T)), ignoreMissing=FALSE)
  }
}
