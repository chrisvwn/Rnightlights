library(Rnightlights)

context("nlPeriods")

test_that("nlPeriod dates are correct", {
  #OLS.Y
  expect_false(unlist(validNlPeriods(nlPeriods = "1991", nlTypes = "OLS.Y")))
  expect_true(unlist(validNlPeriods(nlPeriods = "1992", nlTypes = "OLS.Y")))
  expect_true(unlist(validNlPeriods(nlPeriods = "2013", nlTypes = "OLS.Y")))
  expect_false(unlist(validNlPeriods(nlPeriods = "2014", nlTypes = "OLS.Y")))
  expect_false(unlist(validNlPeriods(nlPeriods = "201401", nlTypes = "OLS.Y")))
  
  #VIIRS.D
  expect_false(unlist(validNlPeriods(nlPeriods = "20171119", nlTypes = "VIIRS.D")))
  expect_true(unlist(validNlPeriods(nlPeriods = "20171120", nlTypes = "VIIRS.D")))
  expect_false(unlist(validNlPeriods(nlPeriods = "20171301", nlTypes = "VIIRS.D")))
  expect_false(unlist(validNlPeriods(nlPeriods = "2017", nlTypes = "VIIRS.D")))
  
  #VIIRS.M
  expect_false(unlist(validNlPeriods(nlPeriods = "201201", nlTypes = "VIIRS.M")))
  expect_true(unlist(validNlPeriods(nlPeriods = "201204", nlTypes = "VIIRS.M")))
  expect_false(unlist(validNlPeriods(nlPeriods = "201413", nlTypes = "VIIRS.M")))
  expect_false(unlist(validNlPeriods(nlPeriods = "2014", nlTypes = "VIIRS.M")))
  
  #VIIRS.Y
  expect_false(unlist(validNlPeriods(nlPeriods = "2011", nlTypes = "VIIRS.Y")))
  expect_true(unlist(validNlPeriods(nlPeriods = "2015", nlTypes = "VIIRS.Y")))
  expect_false(unlist(validNlPeriods(nlPeriods = "201413", nlTypes = "VIIRS.Y")))
  expect_false(unlist(validNlPeriods(nlPeriods = "20171201", nlTypes = "VIIRS.Y")))
})

test_that("nlPeriod nlTypes are correct", {
  expect_error(unlist(validNlPeriods(nlPeriods = "1992", nlType = "OOO"), "Invalid nlType", fixed=TRUE))
  expect_error(unlist(validNlPeriods(nlPeriods = "201401", nlType = "VIIIS"), "Invalid nlType", fixed=TRUE))
  
  expect_that(unlist(getAllNlPeriods(nlTypes = "OLS.Y")), is_a("integer"))
  expect_equal(unname(unlist(getAllNlPeriods(nlTypes = "OLS.Y"))), 1992:2013)
  expect_that(unlist(getAllNlPeriods(nlTypes = "VIIRS.M")), is_a("character"))
  expect_equal(unname(unlist(getAllNlPeriods(nlTypes = "VIIRS.M"))), gsub("-","",format.Date(as.character(seq.Date(from = as.Date("2012-04-01"), to = as.Date(date(), "%a %b %d %H:%M:%S %Y"), by = "month")), "%Y-%m")))
})
