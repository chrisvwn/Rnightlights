library(testthat)
library(Rnightlights)

context("ctrynltiles")

test_that("nlTiles are correct", {
  expect_equal(nlTiles, nlTilesOrig)
})

test_that("nlTiles are available on NOAA", {
  
  skip_if_not(internetAvailable(), "Internet not available")
  skip_if_not(noaaIndexUrlIsAvailableOLS(), "NOAA OLS index url not found")
  
  allNlPeriodsOLS <- getAllNlPeriods("OLS")
  
  for(nlPeriod in allNlPeriodsOLS)
    expect_match(Rnightlights:::getNlUrlOLS(nlPeriod), "^https\\:\\/\\/.*.tar$")
  
  allNlPeriodsVIIRS <- getAllNlPeriods("VIIRS")
  
  for(nlPeriod in allNlPeriodsVIIRS[1:(length(allNlPeriodsVIIRS)-2)])
    for(tileNum in 1:6)
      expect_match(Rnightlights:::getNlUrlVIIRS(nlPeriod, tileNum), "^https\\:\\/\\/.*.tgz$")
})