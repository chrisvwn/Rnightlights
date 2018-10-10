library(testthat)
library(Rnightlights)

context("ctrypoly")

test_that("polygons are available on GADM", {
  skip_if_not(internetAvailable(), "Internet not available")
})