library(Rnightlights)

context("ctryCodes")

test_that("ctryCode lookups work", {
  expect_warning(ctryCodeToName("KE"), "Only 3-letter ISO3 codes allowed", fixed=TRUE)
  expect_equal(ctryCodeToName("KE"), NA)
  expect_match(ctryCodeToName("KEN"), "Kenya", fixed=TRUE)
  
  expect_that(ctryCodeToName(), is_a("data.frame"))
  expect_true(nrow(ctryCodeToName()) > 0)
  
  expect_that(ctryNameToCode(), is_a("data.frame"))
  expect_true(nrow(ctryNameToCode()) > 0)
  
  expect_match(ctryNameToCode("Kenya"), "KEN", fixed=TRUE)
})
