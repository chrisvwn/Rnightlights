library(testthat)
library(Rnightlights)

context("stats")

test_that("stats functions are working fine", {
  expect_equal(as.logical(Rnightlights:::validNlStats(c("sum"))), TRUE)
  #myZonal
  #ZonalPipe
  #fnAggRadGdal
  #fnAggRadRast
})
  