library(Rnightlights)
library(testthat)

context("nlfilename")

test_that("nlfilenames are correct", {
  expect_equal(Rnightlights:::getNlTileZipLclNamePath("VIIRS.M", "201401", 1), file.path(getNlDir("dirNlTiles"), paste0("NL_TILE_VIIRS.M_201401_", Rnightlights:::tileIdx2Name(tileNum = 1, nlType = "VIIRS.M"), ".tgz")))
  
  expect_equal(Rnightlights:::getNlTileZipLclNameVIIRS("201401", 1, "VIIRS.M"), "NL_TILE_VIIRS.M_201401_75N180W.tgz")

  expect_equal(Rnightlights:::getNlTileTifLclNamePath("VIIRS.M", "201401", 1), file.path(getNlDir("dirNlTiles"), paste0("NL_TILE_VIIRS.M_201401_", Rnightlights:::tileIdx2Name(tileNum = 1, nlType = "VIIRS.M"), ".tif")))
    
  expect_equal(Rnightlights:::getNlTileTifLclNameVIIRS("201412", 1, "VIIRS.M"), "NL_TILE_VIIRS.M_201412_75N180W.tif")
  
  expect_equal(Rnightlights:::getNlTileTifLclNameOLS("1992"), "NL_TILE_OLS.Y_1992_00N180W.tif")
  
  expect_equal(Rnightlights:::getCtryNlDataFname(ctryCode = "KEN", admLevel = "KEN_adm0", gadmVersion = "2.8"), "NL_DATA_KEN_ADM0_GADM-2.8.csv")
  
  expect_equal(Rnightlights:::getPolyFname("KEN", gadmVersion = "2.8"),"SHP_KEN_GADM-2.8")
  
  expect_equal(Rnightlights:::getPolyFnamePath("KEN", "2.8"), file.path(getNlDir("dirPolygon"), "SHP_KEN_GADM-2.8"))
  
  expect_equal(Rnightlights:::getPolyFnameZip("KEN", "2.8"), file.path(getNlDir("dirPolygon"), "SHP_KEN_GADM-2.8.zip"))
  
  expect_equal(Rnightlights:::existsPolyFnamePath("KEN"), file.exists(file.path(file.path(getNlDir("dirPolygon"), "SHP_KEN_GADM-2.8"))))
  
  expect_equal(Rnightlights:::existsPolyFnameZip("KEN"), file.exists(file.path(getNlDir("dirPolygon"), "SHP_KEN_GADM-2.8.zip")))
  
  expect_equal(Rnightlights:::getCtryNlDataFnamePath("KEN", "KEN_adm0", gadmVersion = "2.8"), file.path(file.path(getNlDir("dirNlData"), "NL_DATA_KEN_ADM0_GADM-2.8.csv")))
  
  expect_equal(Rnightlights:::getCtryRasterOutputFname("KEN","VIIRS.M","201204",gadmVersion = "2.8"), "NL_KEN_VIIRS.M_201204_GADM-2.8.tif")
  
  expect_equal(Rnightlights:::getCtryRasterOutputFnamePath("KEN","VIIRS.M","201204", "3.6"), file.path(getNlDir("dirRasterOutput"), "NL_KEN_VIIRS.M_201204_GADM-3.6.tif"))
  
})
