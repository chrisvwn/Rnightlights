library(Rnightlights)

context("nlPeriods")

test_that("nlPeriod dates are correct", {
  expect_false(validNlPeriods(nlPeriod = "1991", nlType = "OLS.Y"))
  expect_true(validNlPeriods(nlPeriod = "1992", nlType = "OLS.Y"))
  expect_true(validNlPeriods(nlPeriod = "2013", nlType = "OLS.Y"))
  expect_false(validNlPeriods(nlPeriod = "2014", nlType = "OLS.Y"))
  expect_false(validNlPeriods(nlPeriod = "201401", nlType = "OLS.Y"))
  
  expect_false(validNlPeriods(nlPeriod = "201201", nlType = "VIIRS.M"))
  expect_true(validNlPeriods(nlPeriod = "201204", nlType = "VIIRS.M"))
  expect_false(validNlPeriods(nlPeriod = "201413", nlType = "VIIRS.M"))
  expect_false(validNlPeriods(nlPeriod = "2014", nlType = "VIIRS.M"))
})

test_that("nlPeriod nlTypes are correct", {
  expect_error(validNlPeriods(nlPeriod = "1992", nlType = "OOO"), "Invalid nlType", fixed=TRUE)
  expect_error(validNlPeriods(nlPeriod = "201401", nlType = "VIIIS"), "Invalid nlType", fixed=TRUE)
  
  expect_that(getAllNlPeriods("OLS.Y"), is_a("integer"))
  expect_equal(getAllNlPeriods("OLS.Y"), 1992:2013)
  expect_that(getAllNlPeriods("VIIRS.M"), is_a("character"))
  expect_equal(getAllNlPeriods("VIIRS.M"), gsub("-","",format.Date(as.character(seq.Date(from = as.Date("2012-04-01"), to = as.Date(date(), "%a %b %d %H:%M:%S %Y"), by = "month")), "%Y-%m")))
})