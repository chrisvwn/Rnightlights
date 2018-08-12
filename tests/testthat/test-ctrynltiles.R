library(testthat)
library(Rnightlights)

.runThisTest <- Sys.getenv("RunAllTests") == "yes"

if(.runThisTest)
{
  context("ctrynltiles")
  
  test_that("nlTiles are correct", {
    expect_equal(nlTilesVIIRS, nlTilesOrigVIIRS)
    expect_equal(nlTilesOLS, nlTilesOrigOLS)
  })
  
  test_that("nlTiles are available on NOAA", {
    
    skip_if_not(internetAvailable(), "Internet not available")
    skip_if_not(noaaIndexUrlIsAvailableOLS(), "NOAA OLS index url not found")
      
    allNlPeriodsOLS <- unlist(Rnightlights::getAllNlPeriods("OLS.Y"))
    
    for(nlPeriod in allNlPeriodsOLS)
      expect_match(Rnightlights:::getNlUrlOLS(nlPeriod), "^https\\:\\/\\/.*.tar$")
    
    skip_if_not(noaaIndexUrlIsAvailableVIIRS(), "NOAA VIIRS index url not found")
    
    allNlPeriodsVIIRS <- unlist(Rnightlights::getAllNlPeriods("VIIRS.M"))
    
    #Check only up to 201712 as 201801 missing for some reason. Investigating ...
    for(nlPeriod in grep("2018", allNlPeriodsVIIRS, invert = T, value = T))
      for(tileNum in 1:6)
      {
        message(Sys.time(), ":", nlPeriod, tileNum)
        testthat::expect_match(Rnightlights:::getNlUrlVIIRS(nlPeriod, tileNum, "VIIRS.M"), "^https\\:\\/\\/.*.tgz$")
      }
  })
}