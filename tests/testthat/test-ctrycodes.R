library(testthat)
library(Rnightlights)

context("ctryCodes")

test_that("ctryNameToCode input is valid", {
  expect_error(ctryNameToCode(7), "Invalid ctryName")
  expect_error(ctryNameToCode(NULL), "Invalid ctryName")
  expect_error(ctryNameToCode(NA), "Invalid ctryName")
  expect_error(ctryNameToCode(""), "Invalid ctryName")
})

test_that("ctryCodeToName input is valid", {
  expect_error(ctryCodeToName(7), "Invalid ctryCode")
  expect_error(ctryCodeToName(NULL), "Invalid ctryCode")
  expect_error(ctryCodeToName(NA), "Invalid ctryCode")
  expect_error(ctryCodeToName(""), "Invalid ctryCode")
})

test_that("ctryName/ctryCode lookups work", {
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
  
  expect_null(searchCountry())
  expect_identical(searchCountry("Kenya"), data.frame("ADMIN"="Kenya", "ISO3"="KEN"))
  
  
  #validCtryCodes
  expect_equal(validCtryCodes(c("RWA", "UGA", "ETH")), c(TRUE,TRUE,TRUE))
  expect_true(Rnightlights:::allValidCtryCodes(c("RWA", "UGA", "ETH")))
})
