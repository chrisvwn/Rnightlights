library(testthat)

context("nlfilename")

test_that("nlfilenames are correct", {
  expect_equal(Rnightlights:::getNlTileZipLclNamePath("VIIRS.M", "201401", 1), file.path(getNlDir("dirNlTiles"), paste0("VIIRS.M_201401_", Rnightlights:::tileIdx2Name(tileNum = 1, nlType = "VIIRS.M"), ".tgz")))
  
  expect_equal(Rnightlights:::getNlTileZipLclNameVIIRS("201401", 1, "VIIRS.M"), "VIIRS.M_201401_75N180W.tgz")

  expect_equal(Rnightlights:::getNlTileTifLclNamePath("VIIRS.M", "201401", 1), file.path(getNlDir("dirNlTiles"), paste0("VIIRS.M_201401_", Rnightlights:::tileIdx2Name(tileNum = 1, nlType = "VIIRS.M"), ".tif")))
    
  expect_equal(Rnightlights:::getNlTileTifLclNameVIIRS("201412", 1, "VIIRS.M"), "VIIRS.M_201412_75N180W.tif")
  
  expect_equal(Rnightlights:::getNlTileTifLclNameOLS("1992"), "OLS_1992_00N180W.tif")
  
  
})
