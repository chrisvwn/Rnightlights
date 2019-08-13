library(testthat)
library(Rnightlights)
library(data.table)

context("ctrynldata")

.runThisTest <- Sys.getenv("RunAllTests") == "yes"

if(.runThisTest)
{
  skip_if_not(internetAvailable(), "Internet not available")
  
  skip_on_cran()
  skip_on_travis()
  getCtryNlDataSTP()
  
  test_that("ctrynldata is extracting and calculating correctly", {
    skip_on_cran()
    skip_on_travis()
    expect_identical(file.size(getCtryRasterOutputFnamePath(ctryCode = testCtryCode, nlType =  "OLS.Y", nlPeriod = "1992", gadmVersion = testGadmVer)), file.size(paste0("NL_STP_OLS.Y_1992_STABLE_LIGHTS_GADM-", testGadmVer,"-SHPZIP.tif")))
    
    skip_on_cran()
    skip_on_travis()
    expect_identical(file.size(getCtryRasterOutputFnamePath(ctryCode = testCtryCode, nlType = "VIIRS.M", nlPeriod = "201401", gadmVersion = testGadmVer)), file.size(paste0("NL_STP_VIIRS.M_201401_VCMCFG_GADM-", testGadmVer,"-SHPZIP.tif")))
    
    skip_on_cran()
    skip_on_travis()
    expect_equal(stpRastOLS.Y, stpOrigOLS.Y)
    
    skip_on_cran()
    skip_on_travis()
    skip_if_not(exists("stpGdal"), "GDAL test not run. GDAL likely not installed. Skipping")
    expect_equal(stpGdalVIIRS.M, stpOrigVIIRS.M)
  })
  
  #drop the temp dataPath and revert to the main installation if it exists
  nlCleanup()
  
  detach("package:Rnightlights", unload = T)
  
  library(Rnightlights)
}