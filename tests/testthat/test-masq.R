library(testthat)

.runThisTest <- Sys.getenv("RunAllTests") == "yes"

if(.runThisTest)
{
  context("testmasq")
  
  test_that("masq data extraction from rasters works", {
    expect_equal(sum(admSumMasqOLS, na.rm = T), 590)
    expect_equal(sum(admSumMasqVIIRS, na.rm = T), 1105.12)
  })
}