######################## RNIGHTLIGHTSOPTIONS ###################################

RNIGHTLIGHTSOPTIONS <- settings::options_manager(
  #Change the temp dir to use e.g. if the system temp dir does not have enough space
  tmpDir = raster::tmpDir(),
  
  ntLtsIndexUrlVIIRS = "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html",
  
  ntLtsIndexUrlOLS = "https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/",
  
  stats = c("sum", "mean"),
  
  dirNlDataPath = ".Rnightlights",
  
  #Set directory path
  dirNlTiles = "tiles",
  
  dirRasterOutput = "outputrasters",
  
  dirRasterWeb = "outputrasters_web",
  
  dirZonals = "zonals",
  
  dirPolygon = "polygons",
  
  dirNlData = "data",
  
  #cropMaskMethod" Method used to crop and mask tiles to country polygons. 
  #options: "gdal" or "rast" gdal is usually faster but requires gdal to be installed on the system
  cropMaskMethod = "gdal",
  
  extractMethod = "gdal",
  
  #gdal_cachemax Speeds up gdal_rasterize calculation of stats in function ZonalPipe with more cache (advice: max 1/3 of your total RAM) see: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  gdal_cachemax = 2000,
  
  #downloadMethod used options: auto, aria, curl, libcurl, wget
  downloadMethod = "auto",
  
  omitCountries = "missing",
  
  deleteTiles = FALSE,
  
  .allowed = list(
    cropMaskMethod = settings::inlist("gdal","rast"),
    extractMethod = settings::inlist("gdal", "rast"),
    downloadMethod = settings::inlist("aria", "auto", "curl", "libcurl", "wget"),
    omitCountries = settings::inlist("error", "missing", "long", "all", "none")
  )
)

######################## pkgOptions ###################################

#' Set or get options for the Rnightlights package
#' 
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{dirNlTiles}}{(\code{character}) The directory in which to store the downloaded VIIRS raster tiles }
#'  \item{\code{dirRasterOutput}}{(\code{character}) The directory in which to store the clipped country rasters }
#'  \item{\code{dirRasterWeb}}{(\code{character}) The directory in which to store the rasters resampled for web display }
#'  \item{\code{dirZonals}}{(\code{character}) The directory in which to store the zonal statistics country polygon }
#'  \item{\code{dirPolygon}}{(\code{character}) The directory to store the downloaded country administration level polygons }
#'  \item{\code{dirNlData}}{(\code{character}) The directory to store the extracted data files in }
#'  \item{\code{cropMaskMethod}}{(\code{character}) The method to use to clip the nightlight raster tiles to the country boundaries }
#'  \item{\code{extractMethod}}{(\code{character}) The method to use to extract data from the rasters }
#'  \item{\code{downloadMethod}}{(\code{character}) The download method to use }
#'  \item{\code{omitCountries}}{(\code{character}) The countries to exclude in processing }
#' }
#' @return if an option name is supplied as a parameter this returns the value, else a list of all options is returned.
#' 
#' @examples
#' #retrieve the current cropMaskMethod
#' pkgOptions("cropMaskMethod")
#' 
#' #set the cropMaskMethod
#' pkgOptions(cropMaskMethod="gdal")
#' 
#' #retrieve all options
#' pkgOptions()
#' 
#' @export
pkgOptions <- function(...)
{
  settings::stop_if_reserved(...)
  
  RNIGHTLIGHTSOPTIONS(...)
}

######################## pkgReset ###################################

#' Reset global options for the Rnightlights package
#' 
#' Reset global options for the Rnightlights package
#'
#' @examples
#' #get cropMaskMethod
#' pkgOptions("cropMaskMethod")
#' 
#' #set cropMaskMethod to "gdal"
#' pkgOptions(cropMaskMethod="gdal")
#' 
#' #check cropMaskMethod has changed
#' pkgOptions("cropMaskMethod")
#' 
#' #reset pkgOptions
#' pkgReset()
#' 
#' #check cropMaskMethod has been reset
#' pkgOptions("cropMaskMethod")
#'
#' @export
pkgReset <- function()
{
  settings::reset(RNIGHTLIGHTSOPTIONS)
}