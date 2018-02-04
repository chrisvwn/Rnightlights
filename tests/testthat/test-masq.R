library(testthat)

context("testmasq")

test_that("masq data extraction from rasters works", {
  expect_equal(sum(admSum, na.rm = T), 0)
  
})

unlink("STP_adm_shp.zip", recursive = T, force = T)
unlink("STP_adm_shp", recursive = T, force = T)