library(testthat)
library(Rnightlights)

.runThisTest <- Sys.getenv("RunAllTests") == "yes"

if(.runThisTest)
{
  context("ctrynltiles")
  
  test_that("nlTiles and nlConfigNames names are correct", {
    nlTilesUrlOLS <- pkgOptions("ntLtsIndexUrlOLS.Y")
    
    nlTilesUrlVIIRS <- pkgOptions("ntLtsIndexUrlVIIRS.D")
    
    nlTilesOrig <- read.csv("nltiles.csv", header = T, stringsAsFactors = F)
    nlTilesOrigVIIRS <- nlTilesOrig[grep("VIIRS", nlTilesOrig$type),]
    nlTilesOrigOLS <- nlTilesOrig[grep("OLS", nlTilesOrig$type),]
    
    nlTilesVIIRS <- Rnightlights:::getNlTiles("VIIRS.M")
    nlTilesOLS <- Rnightlights:::getNlTiles("OLS.Y")

    expect_equal(nlTilesVIIRS, nlTilesOrigVIIRS)
    expect_equal(nlTilesOLS, nlTilesOrigOLS)
    
    nlConfigNames <- list("OLS.Y" = c("cf_cvg", "avg_vis", "stable_lights"),
                          "VIIRS.D" = c("vcmcfg", "vcmsl"),
                          "VIIRS.M" = c("vcmcfg", "vcmsl"),
                          "VIIRS.Y" = c("vcm-orm", "vcm-orm-ntl", "vcm-ntl"))
    
    expect_equal(getAllNlConfigNames(), nlConfigNames)
    
    for(nlType in getAllNlTypes())
      expect_equal(getAllNlConfigNames(nlType = nlType), nlConfigNames[nlType])
    
    expect_equal(getAllNlConfigNames("Unknown"), NA)
    
    expect_true(Rnightlights:::validNlConfigName(configName = "cf_cvg"))
    
    expect_equal(Rnightlights:::validNlConfigName(configName = unlist(nlConfigNames)), rep(TRUE, length(unlist(nlConfigNames))))
    
    for(nlType in names(nlConfigNames))
      expect_equal(Rnightlights:::validNlConfigName(configName = nlConfigNames[[nlType]], nlType = nlType), rep(TRUE, length(unlist(nlConfigNames[nlType]))))
    
    expect_equal(Rnightlights:::validNlConfigName(configName = unlist(nlConfigNames)), rep(TRUE, length(unlist(nlConfigNames))))
    
    expect_false(Rnightlights:::validNlConfigName(configName = "Unknown"))
    
    expect_true(Rnightlights:::validNlConfigName(configName = "cf_cvg", nlType = "OLS.Y"))
    
    expect_false(Rnightlights:::validNlConfigName(configName = "vcmcfg", nlType = "OLS.Y"))
  })
  
  test_that("nlTiles and nlConfigNames names are correct", {
    expect_equal(Rnightlights:::getCtryTileList(ctryCodes = "KEN", nlType = "OLS.Y"), "DUMMY")
    
    expect_equal(unname(Rnightlights:::getCtryTileList(ctryCodes = "KEN", nlType = "VIIRS.M")), c("75N060W", "00N060W"))
    
    expect_equal(unname(Rnightlights:::getCtryTileList(ctryCodes = "RUS", nlType = "VIIRS.M")), c("75N180W", "75N060W", "75N060E"))
  })
  
  test_that("nlTiles are available on NOAA", {
    
    skip_if_not(internetAvailable(), "Internet not available")
    skip_if_not(siteIsAvailable(site = pkgOptions("ntLtsIndexUrlOLS.Y")), "NOAA OLS index url not found")

    allNlPeriodsOLS <- unlist(Rnightlights::getAllNlPeriods("OLS.Y"))
    
    #nlUrls
    for(nlPeriod in allNlPeriodsOLS)
      expect_match(Rnightlights:::getNlUrlOLS(nlPeriod), "^https\\:\\/\\/.*.tar$")
    
    skip_if_not(siteIsAvailable(site = pkgOptions("ntLtsIndexUrlVIIRS.M")), "NOAA VIIRS index url not found")
    
    allNlPeriodsVIIRS <- unlist(Rnightlights::getAllNlPeriods("VIIRS.M"))
    
    #recently been delays so don't check the last 2 months
    allNlPeriodsVIIRS <- allNlPeriodsVIIRS[-c((length(allNlPeriodsVIIRS)-2):length(allNlPeriodsVIIRS))]
    
    #Check only up to 201712 as 201801 missing for some reason. Investigating ...
    for(nlPeriod in grep("2018", allNlPeriodsVIIRS, invert = T, value = T))
      for(tileNum in 1:6)
      {
        message(Sys.time(), ":", nlPeriod, tileNum)
        testthat::expect_match(Rnightlights:::getNlUrlVIIRS(nlPeriod, tileNum, "VIIRS.M"), "^https\\:\\/\\/.*.tgz$")
      }
  })
}