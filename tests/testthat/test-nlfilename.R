library(Rnightlights)
library(testthat)

context("nlfilename")

test_that("nlfilenames are correct", {
  expect_equal(Rnightlights:::getNlTileZipLclNamePath(nlType = "VIIRS.M", nlPeriod = "201401", tileNum = 1),
               file.path(getNlDir("dirNlTiles"), "NL_TILE_VIIRS.M_VCMCFG_201401_75N180W.tgz"))
  
  expect_equal(Rnightlights:::getNlTileZipLclNameVIIRS(nlPeriod = "201401", tileNum = 1, nlType = "VIIRS.M"), "NL_TILE_VIIRS.M_VCMCFG_201401_75N180W.tgz")

  expect_equal(Rnightlights:::getNlTileTifLclNamePath(nlType = "VIIRS.M", nlPeriod = "201401", tileNum = 1), file.path(getNlDir("dirNlTiles"), "NL_TILE_VIIRS.M_VCMCFG_201401_75N180W.tif"))
    
  expect_equal(Rnightlights:::getNlTileTifLclNameVIIRS(nlType = "VIIRS.M", nlPeriod = "201412", tileNum = 1), "NL_TILE_VIIRS.M_VCMCFG_201412_75N180W.tif")
  
  expect_equal(Rnightlights:::getNlTileTifLclNameOLS(nlPeriod = "1992"), "NL_TILE_OLS.Y_STABLE_LIGHTS_1992_00N180W.tif")
  
  expect_equal(Rnightlights:::getCtryNlDataFname(ctryCode = "KEN", admLevel = "KEN_adm0", gadmVersion = "2.8"),
               "NL_DATA_KEN_ADM0_GADM-2.8-SHPZIP.csv")
  
  expect_equal(Rnightlights:::getPolyFname(ctryCode = "KEN", gadmVersion = "2.8"),
               "POLY_KEN_GADM-2.8-SHPZIP")
  
  expect_equal(Rnightlights:::getPolyFnamePath(ctryCode = "KEN", gadmVersion = "2.8"),
               file.path(getNlDir("dirPolygon"), "POLY_KEN_GADM-2.8-SHPZIP"))
  
  expect_equal(Rnightlights:::getPolyFnameZip(ctryCode = "KEN", gadmVersion = "2.8"),
               file.path(getNlDir("dirPolygon"), "POLY_KEN_GADM-2.8-SHPZIP.zip"))
  
  expect_equal(Rnightlights:::existsPolyFnameZip(ctryCode = "KEN"),
               file.exists(file.path(file.path(getNlDir("dirPolygon"), "POLY_KEN_GADM-3.6-SHPZIP.zip"))))
  
  expect_equal(Rnightlights:::existsPolyFnameRDS(ctryCode = "KEN"),
               file.exists(file.path(file.path(getNlDir("dirPolygon"), "POLY_KEN_GADM-3.6-SHPZIP.rds"))))
  
  expect_equal(Rnightlights:::existsPolyFnameZip(ctryCode = "KEN"),
               file.exists(file.path(getNlDir("dirPolygon"), "SHP_KEN_GADM-3.6.zip")))
  
  expect_equal(Rnightlights:::getCtryNlDataFnamePath(ctryCode = "KEN", admLevel = "KEN_adm0", gadmVersion = "2.8"), file.path(file.path(getNlDir("dirNlData"), "NL_DATA_KEN_ADM0_GADM-2.8-SHPZIP.csv")))
  
  expect_equal(Rnightlights:::getCtryRasterOutputFname(ctryCode = "KEN", nlType = "VIIRS.M", nlPeriod = "201204", gadmVersion = "2.8"), "NL_KEN_VIIRS.M_201204_VCMCFG-MTSALL-MEAN-RGFT_GADM-2.8-SHPZIP.tif")
  
  expect_equal(Rnightlights:::getCtryRasterOutputFnamePath(ctryCode = "KEN", nlType = "VIIRS.M", nlPeriod = "201204", "3.6"), file.path(getNlDir("dirRasterOutput"), "NL_KEN_VIIRS.M_201204_3.6-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP.tif"))
  
})
