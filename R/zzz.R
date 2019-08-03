.onLoad <- function(libname, pkgname)
{
  credits <- "DMSP data collected by US Air Force Weather Agency
Image and data processing by NOAA's National Geophysical Data Center
(https://www.ngdc.noaa.gov/eog/download.html)

Maps distributed by GADM
(https://gadm.org)"
  
  printCredits(credits)
}

######################## .onAttach ###################################

#' Set global variables in .onAttach since we want the package to be
#'     usable without active loading via library(Rnightlights)
#'
.onAttach <- function(libname, pkgname)
{
  #Setup the data path, possibly by prompting the user. if not found
  if(is.null(getNlDataPath()))
    setupDataPath()
  
  #global constants
  
  #world map and clean it
  #may take a sec or two so let's do it once
  #clean now (2019) shows a progressbar which is not ideal
  #we may move this back into the main code and maybe instantiate
  #it globally the first time we need it
  map <- rworldmap::getMap()
  
  map <- cleangeo::clgeo_Clean(map)
  
  #still needed?
  shpTopLyrName <- "adm0"

  #projection system to use
  #still needed?
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  #try to speed up the code
  #does this work?
  #enabled in DESCRIPTION file
  compiler::enableJIT(3)
  
  upgradeRnightlights()
}

.onUnload <- function(libpath)
{

}

.onDetach <- function(libpath)
{
  
}

######################## .Last.lib ###################################

#' Clean up the environment
#'
#' Clean up the environment and delete temp files when the package is
#'     detached.
#'     
#'     Use .Last.lib rather than .onDetach or .onUnload which may not be run
#'     at the end of the session. And rather than reg.finalizer which is
#'     called when gc() runs which we do not want to happen for nlCleanup()
#'     especially since we do call gc() in some instances.
#'
#' @export
.Last.lib <- function(libpath)
{
  map <- NULL
  shpTopLyrName <- NULL
  wgs84 <- NULL
  nlTiles <- NULL
  tilesSpPolysDFs <- NULL
  
  #remove any global vars we created in .onAttach
  suppressWarnings(rm(map, shpTopLyrName, wgs84, nlTiles, tilesSpPolysDFs))
  
  #cleanup by removing any global vars created etc
  nlCleanup()
  
  compiler::enableJIT(0)
}
