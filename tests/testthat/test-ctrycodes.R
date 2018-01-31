library(testthat)
library(Rnightlights)

context("ctryCodes")

test_that("ctryCode lookups work", {
  #ctryCodeToName
  expect_match(ctryCodeToName("KEN"), "Kenya", fixed=TRUE)
  expect_match(ctryCodeToName("TZA"), "United Republic of Tanzania", fixed=TRUE)
  
  expect_that(ctryCodeToName(), is_a("data.frame"))
  expect_true(nrow(ctryCodeToName()) > 0)
  expect_true(identical(ctryCodeToName()$ISO3, getAllNlCtryCodes()))
  
  #ctryNameToCode
  expect_match(ctryNameToCode("Kenya"), "KEN", fixed=TRUE)
  expect_match(ctryNameToCode("tanzania"), "TZA", fixed=TRUE)
  
  expect_that(ctryNameToCode(), is_a("data.frame"))
  expect_true(nrow(ctryNameToCode()) > 0)
  
  #validCtryCodes
  expect_equal(validCtryCodes(c("RWA", "UGA", "ETH")), c(TRUE,TRUE,TRUE))
  expect_true(Rnightlights:::allValidCtryCodes(c("RWA", "UGA", "ETH")))
})
