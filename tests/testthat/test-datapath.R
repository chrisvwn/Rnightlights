library(testthat)
library(Rnightlights)

context("datapath")

test_that("datapaths work", {

  #get the current data path
  currPath <- getNlDataPath()
  
  #get a random tempdir
  tempPath <- tempdir()
  
  #set path temporarily to tempdir
  oldPath <- Rnightlights:::setNlDataPath(tempPath)
  
  #the returned path should equal currPath
  expect_equal(oldPath, tempPath)
  
  #data path should have changed to tempPath
  expect_equal(Rnightlights::getNlDataPath(), tempPath)

  #test remove non-interactive with confirm=FALSE
  expect_message(Rnightlights:::removeDataPath(dataPath = file.path(tempPath, ".Rnightlights")), "Aborted")
  
  #test remove non-interactive with confirm=TRUE
  expect_message(Rnightlights:::removeDataPath(dataPath=file.path(tempPath, ".Rnightlights"), confirm=TRUE), "Removed dataPath")
  
  expect_equal(list.files(file.path(tempPath, ".Rnightlights")), character(0))
})
  