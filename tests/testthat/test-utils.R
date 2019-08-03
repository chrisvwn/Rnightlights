library(testthat)
library(Rnightlights)

context("utils")

test_that("utils work", {
  Rnightlights:::nlCleanup()
  
  expect_equal(list.files(Rnightlights::getNlDir("dirNlTemp")), character(0))

  expect_true(Rnightlights:::allValid(testData = c(TRUE,TRUE,TRUE), testFun = all))
  
  expect_false(Rnightlights:::allValid(testData = c(TRUE,FALSE,TRUE), testFun = all))
})