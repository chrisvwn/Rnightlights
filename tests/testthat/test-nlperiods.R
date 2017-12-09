library(Rnightlights)

context("nlPeriods")

test_that("nlPeriod dates are correct", {
  expect_false(validNlPeriod(nlPeriod = "1991", nlType = "OLS" ))
  expect_true(validNlPeriod(nlPeriod = "1992", nlType = "OLS" ))
  expect_true(validNlPeriod(nlPeriod = "2013", nlType = "OLS" ))
  expect_false(validNlPeriod(nlPeriod = "2014", nlType = "OLS" ))
  expect_false(validNlPeriod(nlPeriod = "201401", nlType = "OLS" ))
  
  expect_false(validNlPeriod(nlPeriod = "201201", nlType = "VIIRS" ))
  expect_true(validNlPeriod(nlPeriod = "201204", nlType = "VIIRS" ))
  expect_false(validNlPeriod(nlPeriod = "201413", nlType = "VIIRS" ))
  expect_false(validNlPeriod(nlPeriod = "2014", nlType = "VIIRS" ))
})

test_that("nlPeriod nlTypes are correct", {
  expect_error(validNlPeriod(nlPeriod = "1992", nlType = "OOO"), "Invalid nlType", fixed=TRUE)
  expect_error(validNlPeriod(nlPeriod = "201401", nlType = "VIIIS"), "Invalid nlType", fixed=TRUE)
  
  expect_that(getAllNlPeriods("OLS"), is_a("integer"))
  expect_equal(getAllNlPeriods("OLS"), 1992:2013)
  expect_that(getAllNlPeriods("VIIRS"), is_a("integer"))
  expect_equal(getAllNlPeriods("VIIRS"), gsub("-","",format.Date(as.character(seq.Date(from = as.Date("2012-04-01"), to = as.Date(date(), "%a %b %d %H:%M:%S %Y"), by = "month")), "%Y-%m")))
})