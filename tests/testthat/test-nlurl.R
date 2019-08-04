library(testthat)
library(Rnightlights)

context("nlUrls")

test_that("nlUrls work", {
  skip_if_not(internetAvailable(), "Internet not available")
  
  nlUrlOLS.Y_2012 <- Rnightlights:::getNlUrlOLS(nlPeriod = "2012")
  
  currMonth1st <- gsub("-", "", lubridate::floor_date(x = lubridate::today(), unit = "month"))
  nlUrlVIIRS.D_CurrMonth1st <- Rnightlights:::getNlUrlVIIRS(nlPeriod = currMonth1st, tileNum = 1, nlType = "VIIRS.D")
  nlUrlVIIRS.M_201204 <- Rnightlights:::getNlUrlVIIRS(nlPeriod = "201204", tileNum = 1, nlType = "VIIRS.M")
  nlUrlVIIRS.Y_2016<- Rnightlights:::getNlUrlVIIRS(nlPeriod = "2016", tileNum = 1, nlType = "VIIRS.Y")
  
  expect_equal(nlUrlOLS.Y_2012, "https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/F182012.v4.tar", fixed=TRUE)
  expect_equal(nlUrlVIIRS.D_CurrMonth1st, paste0("https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/mosaics//", currMonth1st, "/SVDNB_npp_d", currMonth1st, ".d.75N180W.rade9.tif"), fixed=TRUE)
  expect_equal(nlUrlVIIRS.M_201204, "https://eogdata.mines.edu/wwwdata/viirs_products/dnb_composites/v10//201204/vcmcfg/SVDNB_npp_20120401-20120430_75N180W_vcmcfg_v10_c201605121456.tgz", fixed=TRUE)
  expect_equal(nlUrlVIIRS.Y_2016, "https://eogdata.mines.edu/wwwdata/viirs_products/dnb_composites/v10//2016/SVDNB_npp_20160101-20161231_75N180W_v10_c201807311200.tgz", fixed=TRUE)
})
