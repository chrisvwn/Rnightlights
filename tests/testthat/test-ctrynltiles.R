library(testthat)
library(Rnightlights)

context("ctrynltiles")

test_that("nlTiles are correct", {
  expect_equal(nlTilesVIIRS, nlTilesOrigVIIRS)
  expect_equal(nlTilesOLS, nlTilesOrigOLS)
})

test_that("nlTiles are available on NOAA", {
  
  skip_if_not(internetAvailable(), "Internet not available")
  skip_if_not(noaaIndexUrlIsAvailableOLS(), "NOAA OLS index url not found")
    
  allNlPeriodsOLS <- unlist(getAllNlPeriods("OLS.Y"))
  
  for(nlPeriod in allNlPeriodsOLS)
    expect_match(Rnightlights:::getNlUrlOLS(nlPeriod), "^https\\:\\/\\/.*.tar$")
  
  skip_if_not(noaaIndexUrlIsAvailableVIIRS(), "NOAA OLS index url not found")
  
  allNlPeriodsVIIRS <- unlist(getAllNlPeriods("VIIRS.M"))
  
  for(nlPeriod in allNlPeriodsVIIRS[1:(length(allNlPeriodsVIIRS)-2)])
    for(tileNum in 1:6)
      expect_match(Rnightlights:::getNlUrlVIIRS(nlPeriod, tileNum, "VIIRS.M"), "^https\\:\\/\\/.*.tgz$")
})
