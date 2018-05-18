#set the dataPath to tempdir() to ensure a new environment
#thus forcing polygon and tile downloads and fresh raster
#clipping and data extraction
currDir <- getNlDataPath()

tempDir <- setupDataPath(tempdir())