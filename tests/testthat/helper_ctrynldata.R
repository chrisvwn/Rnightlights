library(testthat)
library(Rnightlights)
library(data.table)

getCtryNlDataSTP <- function()
{
  testCtryCode <<- "STP"
  testAdmLevel <<- "STP_adm2"
  testGadmVer <<- "2.8"
  testDataFile <- Rnightlights:::getCtryNlDataFname(ctryCode = testCtryCode,
                                                    admLevel = testAdmLevel,
                                                    gadmVersion = testGadmVer)
  
  print(testDataFile)
  
  message(file.exists(testDataFile))
  
  #stpOrig <- utils::read.csv(testDataFile, header = T, stringsAsFactors = F)
  stpOrig <<- as.data.frame(data.table::fread(input = testDataFile, encoding = "UTF-8"))
  
  stpOrigOLS.Y <<- stpOrig[, -grep("VIIRS", names(stpOrig))]
  
  stpOrigVIIRS.M <<- stpOrig[, -grep("OLS", names(stpOrig))]
  
  pkgReset()
  
  pkgOptions(deleteTiles=TRUE)
  
  stpRastOLS.Y <<- Rnightlights::getCtryNlData(ctryCode = testCtryCode, admLevel = "lowest", nlTypes = "OLS.Y", nlPeriods = "1992", gadmVersion = testGadmVer, nlStats = c("sum", "mean"), ignoreMissing=FALSE)
  
  gdalUtils::gdal_chooseInstallation()
  
  gdalPath <- getOption("gdalUtils_gdalPath")
  
  if(gdalPath != "")
  {
    if(system("aria2c -h", ignore.stdout = T, ignore.stderr = T) == 0)
      Rnightlights::pkgOptions(downloadMethod="aria")
    
    Rnightlights::pkgOptions(cropMaskMethod="gdal", extractMethod="gdal")
    
    stpGdalVIIRS.M <<- Rnightlights::getCtryNlData(ctryCode = testCtryCode, admLevel = "lowest", nlTypes = "VIIRS.M", gadmVersion = testGadmVer, nlPeriods = "201401", nlStats = c("sum", "mean"), ignoreMissing=FALSE)
  }
}
