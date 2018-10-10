library(testthat)
library(Rnightlights)

context("admLevels")

test_that("admLevel lookups work", {
  skip_if_not(internetAvailable(), "Internet not available")
  
  #ctryCodeToName
  expect_equal(Rnightlights:::getCtryPolyAdmLevelNames("STP"), c("country", "Municipality_(Concelho)", "District"), fixed=TRUE)
  expect_equal(Rnightlights:::getCtryPolyAdmLevelNames("STP", "gadm36_STP_1"), c("country", "Municipality_(Concelho)"), fixed=TRUE)
  expect_equal(Rnightlights:::getCtryShpAllAdmLvls("STP"), list(STP=c("gadm36_STP_0", "gadm36_STP_1", "gadm36_STP_2")), fixed=TRUE)
  expect_equal(unname(unlist(Rnightlights::searchAdmLevel("STP", "Municipality"))),"gadm36_STP_1", fixed=TRUE)
  expect_equal(unname(Rnightlights::searchAdmLevel("STP", "highest")),"gadm36_STP_1", fixed=TRUE)
  expect_true(Rnightlights:::allValidCtryAdmLvls("STP", c("gadm36_STP_0","gadm36_STP_1","gadm36_STP_2")))
})
