library(testthat)
library(Rnightlights)

context("admLevels")

test_that("admLevel lookups work", {
  skip_if_not(internetAvailable(), "Internet not available")
  
  #ctryCodeToName
  expect_equal(Rnightlights:::getCtryPolyAdmLevelNames("STP"), c("country", "Municipality_(Concelho)", "District"), fixed=TRUE)
  expect_equal(Rnightlights:::getCtryPolyAdmLevelNames("STP", "STP_adm1"), c("country", "Municipality_(Concelho)"), fixed=TRUE)
  expect_equal(Rnightlights:::getCtryShpAllAdmLvls("STP"), list(STP=c("STP_adm0", "STP_adm1", "STP_adm2")), fixed=TRUE)
  expect_equal(unname(unlist(Rnightlights::searchAdmLevel("STP", "Municipality"))),"STP_adm2", fixed=TRUE)
  expect_equal(unname(Rnightlights::searchAdmLevel("STP", "highest")),"STP_adm1", fixed=TRUE)
  expect_true(Rnightlights:::allValidCtryAdmLvls("STP", c("STP_adm0","STP_adm1","STP_adm2")))
})
