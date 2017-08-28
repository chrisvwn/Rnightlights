.onLoad <- function(libname, pkgname)
{
  # Setup the data path, possibly by prompting the user. if not found
  #if(is.null(getNlDataPath()))
  #  setupDataPath(tempdir())
}

.onAttach <- function(libname, pkgname)
{
  #Setup the data path, possibly by prompting the user. if not found
  if(is.null(getNlDataPath()))
    setupDataPath()
  
  #global constants
  map <- rworldmap::getMap()
  map <- cleangeo::clgeo_Clean(map)
  shpTopLyrName <- "adm0"

  #projection system to use
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #ntLtsIndexUrlVIIRS = "https://www.ngdc.noaa.gov/eog/viirs/download_monthly.html"
  
  compiler::enableJIT(3)
}

.onDetach <- function(libname)
{
  #remove any global vars we created in .onLoad
  #suppressWarnings(rm(map, shpTopLyrName, wgs84, nlTiles, tilesSpPolysDFs))
  
  #cleanup by removing any global vars created etc
  nlCleanup();
  compiler::enableJIT(0)
}

.onUnload <- function(libname, pkgname)
{
}
