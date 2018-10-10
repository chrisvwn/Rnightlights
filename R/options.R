######################## RNIGHTLIGHTSOPTIONS ###################################

RNIGHTLIGHTSOPTIONS <- settings::options_manager(
  #Specify the regex to uniquely identify the tile file
  #to extract from the download tile tar.gz
  #more info at: https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html
  configName_VIIRS.D = "vcmcfg",
  
  configName_VIIRS.M = "vcmcfg",
  
  configName_VIIRS.Y = "vcm-orm-ntl",
  
  #cropMaskMethod" Method used to crop and mask tiles to country polygons. 
  #options: "gdal" or "rast" gdal is usually faster but requires gdal to be installed on the system
  cropMaskMethod = "rast",
  
  deleteTiles = FALSE,
  
  #Set directory paths
  dirNlData = "data",
  
  dirNlRoot = ".Rnightlights",
  
  dirNlTiles = "tiles",
  
  dirPolygon = "polygons",
  
  dirRasterOutput = "outputrasters",
  
  dirRasterWeb = "outputrasters_web",
  
  dirZonals = "zonals",
  
  dirNlTemp = "temp",
  
  #downloadMethod used options: auto, aria, curl, libcurl, wget
  downloadMethod = "auto",
  
  #methods to extract data. Options: raster, gdal
  extractMethod = "rast",

  #the gadm polygons to use
  gadmVersion = "3.6",
  
  #gdalCacheMax Speeds up gdal_rasterize calculation of stats in function ZonalPipe with more cache (advice: max 1/3 of your total RAM) see: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  gdalCacheMax = 1024,

  #stats to calculate in processNlData. Can be added to if the function exists
  #i.e. if not a standard function can be created in workspace
  nlStats = c("sum", "mean"),
  
  #urls for raster tile listings. In theory, can be used to override the 
  #url if it is changed while the package is being updated
  ntLtsIndexUrlOLS = "https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/",
  
  ntLtsIndexUrlVIIRS.D = "https://ngdc.noaa.gov/eog/viirs/download_ut_mos_tile_iframe.html",
  
  ntLtsIndexUrlVIIRS.M = "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html",
  
  ntLtsIndexUrlVIIRS.Y = "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html",

  numCores = 2,
  
  #countries to not process. useful if many countries being processed
  #and want to exclude a few
  omitCountries = "missing",
  
  #Change the temp dir to use e.g. if the system temp dir does not have enough space
  #Not used yet
  tmpDir = raster::tmpDir(),

  .allowed = list(
    configName_VIIRS.D = settings::inlist("vcmcfg", "vcmsl"),
    configName_VIIRS.M = settings::inlist("vcmcfg", "vcmsl"),
    configName_VIIRS.Y = settings::inlist("vcm-orm", "vcm-orm-ntl", "vcm-ntl"),
    cropMaskMethod = settings::inlist("gdal","rast"),
    extractMethod = settings::inlist("gdal", "rast"),
    downloadMethod = settings::inlist("aria", "auto", "curl", "libcurl", "wget"),
    gadmVersion = settings::inlist("2.8", "3.6"),
    omitCountries = settings::inlist("error", "missing", "long", "all", "none")
  )
)

######################## pkgOptions ###################################

#' Set or get options for the Rnightlights package
#' 
#' @param ... Option names to retrieve option values or \code{[key]=[value]}
#'     pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \describe{
#'  \item{\code{configName.VIIRS.D}}{(\code{character}) The regex to uniquely
#'      identify the tile file to use out of the downloaded tile .tgz. The
#'      version 1 monthly series is run globally using two different 
#'      configurations.
#'      
#'      The first excludes any data impacted by stray light. The second
#'      includes these data if the radiance vales have undergone the stray-
#'      light correction procedure (Reference). These two configurations
#'      are denoted in the filenames as "vcm" and "vcmsl" respectively.
#'      The "vcmsl" version, that includes the stray-light corrected data,
#'      will have more data coverage toward the poles, but will be of reduced
#'      quality.
#'      
#'      It is up to the users to determine which set is best for their
#'      applications. The annual versions are only made with the "vcm"
#'      version, excluding any data impacted by stray light.}
#'      
#'  \item{\code{configName.VIIRS.M}}{(\code{character}) The regex to uniquely
#'      identify the tile file to use out of the downloaded monthly .tgz
#'      tile. Has the same options as configName.VIIRS.D}
#'  
#'  \item{\code{configName.VIIRS.Y}}{(\code{character}) The regex to uniquely
#'      identify the tile file to use out of the downloaded tile .tgz. The
#'      annual products can have other values for the config shortname (Field 5).
#'      They are:
#'      \itemize{
#'        \item vcm-orm \emph{(VIIRS Cloud Mask - Outlier Removed)}: This product
#'          contains cloud-free average radiance values that have undergone
#'          an outlier removal process to filter out fires and other ephemeral
#'          lights.
#'        \item vcm-orm-ntl \emph{(VIIRS Cloud Mask - Outlier Removed - Nighttime Lights)}:
#'          This product contains the "vcm-orm" average, with background
#'          (non-lights) set to zero.
#'        \item vcm-ntl \emph{(VIIRS Cloud Mask - Nighttime Lights)}: This product
#'          contains the "vcm" average, with background
#'          (non-lights) set to zero.}}
#'  \item{\code{cropMaskMethod}}{(\code{character}) The method to use to 
#'      clip the nightlight raster tiles to the country boundaries }
#'  \item{\code{deleteTiles}}{(\code{character}) whether to delete tiles 
#'      after processing may be useful where diskspace is a concern }
#'  \item{\code{dirNlData}}{(\code{character}) The directory to store 
#'      the extracted data files in }
#'  \item{\code{dirNlRoot}}{\code{character}) The root directory 
#'      storing the package data}
#'  \item{\code{dirNlTiles}}{(\code{character}) The directory in which 
#'      to store the downloaded VIIRS raster tiles }
#'  \item{\code{dirPolygon}}{(\code{character}) The directory to store 
#'      the downloaded country administration level polygons }
#'  \item{\code{dirRasterOutput}}{(\code{character}) The directory in 
#'      which to store the clipped country rasters }
#'  \item{\code{dirRasterWeb}}{(\code{character}) The directory in which 
#'      to store the rasters resampled for web display }
#'  \item{\code{dirZonals}}{(\code{character}) The directory in which to 
#'      store the zonal statistics country polygon }
#'  \item{\code{downloadMethod}}{(\code{character}) The download method 
#'      to use }
#'  \item{\code{extractMethod}}{(\code{character}) The method to use to 
#'      extract data from the rasters }
#'  \item{\code{gdalCacheMax}}{(\code{character}) The maximum memory gdal 
#'      should use in gdal_rasterize }
#'  \item{\code{ntLtsIndexUrlOLS}}{(\code{character}) The url with the OLS 
#'      tile index }
#'  \item{\code{ntLtsIndexUrlVIIRS}}{(\code{character}) The url with the 
#'      VIIRS tile index }
#'  \item{\code{numCores}}{(\code{character}) The number of processor cores 
#'      to use when extractMethod = "raster" }
#'  \item{\code{omitCountries}}{(\code{character}) The countries to exclude 
#'      in processing }
#'  \item{\code{stats}}{(\code{character}) The statistics to calculate for 
#'      country regions. The default are sum and mean. Any other aggregate 
#'      statistics can be included. Also any aggregate function accessible 
#'      in the current environment can be added. }
#'  \item{\code{tmpDir}}{(\code{character}) Change the temporary directory
#'      for processing rasters. Not in use }
#' }
#' 
#' @return if an option name is supplied as a parameter this returns the 
#'     value, else a list of all options is returned.
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
#' pkgOptions("cropMaskMethod") #returns default "rast"
#' 
#' #set cropMaskMethod to "gdal"
#' pkgOptions(cropMaskMethod="gdal") #sets to "gdal"
#' 
#' #check cropMaskMethod has changed
#' pkgOptions("cropMaskMethod") #returns "gdal"
#' 
#' #reset pkgOptions
#' pkgReset()
#' 
#' #check cropMaskMethod has been reset
#' pkgOptions("cropMaskMethod") #returns default "rast"
#'
#' @export
pkgReset <- function()
{
  settings::reset(RNIGHTLIGHTSOPTIONS)
}