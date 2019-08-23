.onLoad <- function(libname, pkgname)
{
  credits <- "DMSP data collected by US Air Force Weather Agency
Image and data processing by NOAA's National Geophysical Data Center
(https://www.ngdc.noaa.gov/eog/download.html)

Maps distributed by GADM
(https://gadm.org)"
  
  #printCredits(credits)
}

######################## .RnightlightsENV ###################################

#' A package-wide hidden environment
#' 
#' A package-wide hidden environment used to store variables/settings and
#'     functions to retrieve them
#' 
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
  
  #try to speed up the code
  #does this work?
  #also enabled in DESCRIPTION file
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

  #cleanup by removing any global vars created etc
  nlCleanup()
  
  compiler::enableJIT(0)
}
