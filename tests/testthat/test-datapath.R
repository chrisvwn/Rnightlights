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
  expect_equal(oldPath, currPath)
  
  #data path should have changed to tempPath
  expect_equal(Rnightlights::getNlDataPath(), tempPath)

  expect_message(Rnightlights:::removeDataPath(dataPath = file.path(tempPath, ".Rnightlights")), "Aborted")
  
  expect_message(Rnightlights:::removeDataPath(dataPath=file.path(tempPath, ".Rnightlights"), confirm=TRUE), "Removed dataPath")
  
  expect_equal(list.files(file.path(tempPath, ".Rnightlights")), character(0))
  
  #try setupDataPath with a new tempdir1
  
  #get a second tempdir
  tempPath2 <- tempdir()

  #should have changed to tempdir2
  expect_equal(Rnightlights:::setupDataPath(tempPath2), tempPath2)

  #reload library to restore the data path to the permanent path
  detach("package:Rnightlights", unload = T, character.only = T)
  
  library(Rnightlights)
  
  #should have restored to the currPath
  expect_equal(gsub("/.Rnightlights","", Rnightlights::getNlDir("dirNlDataPath")), currPath)
})
  