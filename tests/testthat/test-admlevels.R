library(testthat)
library(Rnightlights)

context("admLevels")

test_that("admLevel lookups work", {
  skip_if_not(internetAvailable(), "Internet not available")
  
  #ctryCodeToName
  expect_equal(Rnightlights:::getCtryPolyAdmLevelNames("STP"), c("Municipality_(Concelho)", "District"), fixed=TRUE)
  expect_equal(Rnightlights:::getCtryPolyAdmLevelNames("STP", "1"), c("Municipality_(Concelho)"), fixed=TRUE)
  expect_equal(Rnightlights:::getCtryShpAllAdmLvls("STP"), list(STP=c("STP_adm0", "STP_adm1", "STP_adm2")), fixed=TRUE)
  expect_equal(Rnightlights:::searchAdmLevel("STP", "Municipality"),list(STP="STP_adm1"), fixed=TRUE)
  expect_equal(Rnightlights:::searchAdmLevel("STP", "highest"),list(STP="STP_adm1"), fixed=TRUE)
  expect_true(Rnightlights:::validCtryAdmLvls("STP", c("District")))
})