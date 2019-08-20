.onLoad <- function(libname, pkgname)
{
  credits <- "DMSP data collected by US Air Force Weather Agency
Image and data processing by NOAA's National Geophysical Data Center
(https://www.ngdc.noaa.gov/eog/download.html)

Maps distributed by GADM
(https://gadm.org)"
  
  #printCredits(credits)
}

getWorldMap <- function()
{
  if(!exists(".RnightlightsEnv"))
    .RnightlightsEnv <<- new.env(parent = emptyenv())
  
  if(!exists(x = "map", envir = .RnightlightsEnv))
  {
    #world map and clean it
    #may take a sec or two so let's do it once
    #clean now (2019) shows a progressbar which is not ideal
    #we may move this back into the main code and maybe instantiate
    #it globally the first time we need it
    map <- rworldmap::getMap()
    
    #capture cleangeo progressbar output
    out <- utils::capture.output(map <- cleangeo::clgeo_Clean(map))
    
    rm(out)
    
    assign(x = "map", value = map, envir = .RnightlightsEnv)
  }
  
  get(x = "map", envir = .RnightlightsEnv)
}

getCRS <- function()
{
  if(!exists(".RnightlightsEnv"))
    .RnightlightsEnv <<- new.env(parent = emptyenv())
  
  if(!exists(x = "CRS", envir = .RnightlightsEnv))
  {
    assign(x = "CRS", value = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs", envir = .RnightlightsEnv)
  }
  
  get(x = "CRS", envir = .RnightlightsEnv)
}

.RnightlightsEnv <- new.env(parent = emptyenv())


######################## .onAttach ###################################

.onAttach <- function(libname, pkgname)
{
  # Set global variables in .onAttach since we want the package to be
  #     usable without active loading via library(Rnightlights)
  #
  
  #Setup the data path, possibly by prompting the user. if not found
  if(is.null(getNlDataPath()))
    setupDataPath()
  
  #global constants
  #map <- getWorldMap()
  
  #still needed?
  shpTopLyrName <- "adm0"

  #projection system to use
  #still needed?
  #.RnightlightsEnv$wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs"
  
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
  suppressWarnings(rm(.RnightlightsEnv))
}

######################## .Last.lib ###################################

#' @export
.Last.lib <- function(libpath)
{
  # Clean up the environment and delete temp files when the package is
  #     detached.
  #    
  #     Use .Last.lib rather than .onDetach or .onUnload which may not be run
  #     at the end of the session. And rather than reg.finalizer which is
  #     called when gc() runs which we do not want to happen for nlCleanup()
  #     especially since we do call gc() in some instances.

  nlTiles <- NULL
  tilesSpPolysDFs <- NULL
  
  #remove any global vars we created in .onAttach
  suppressWarnings(rm(map, shpTopLyrName, wgs84, nlTiles, tilesSpPolysDFs))
  
  #cleanup by removing any global vars created etc
  nlCleanup()
  
  compiler::enableJIT(0)
}
