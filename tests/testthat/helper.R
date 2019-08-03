#set the dataPath to tempdir() to ensure a clean environment for tests
#thus forcing polygon and tile downloads and fresh raster
#clipping and data extraction
currDir <- getNlDataPath()

tempDir <- tempdir()

Rnightlights:::setNlDataPath(tempDir)

#test internet availability used by multiple tests
internetAvailable <- function()
{
  curl::has_internet()
}