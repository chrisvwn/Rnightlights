######################## RNIGHTLIGHTSOPTIONS ###################################

RNIGHTLIGHTSOPTIONS <- settings::options_manager(
  #Specify the regex to uniquely identify the tile file
  #to extract from the download tile compressed file
  
  #the amount of RAM to provide to the gdal batch processing calculations
  #can be a percentage or fixed value with KB/MB/GB suffixed
  ##try approx 10% of **free** RAM
  batchBytes = "10%",
  
  #for OLS: more info at: https://ngdc.noaa.gov/eog/gcv4_readme.txt
  configName_OLS.Y = "stable_lights",
  
  #for VIIRS: more info at: https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html
  configName_VIIRS.D = "vcmcfg",
  
  configName_VIIRS.M = "vcmcfg",
  
  configName_VIIRS.Y = "vcm-orm-ntl",
  
  #cropMaskMethod" Method used to crop and mask tiles to country polygons. 
  #options: "gdal" or "rast" gdal is usually faster but requires gdal to be installed on the system
  cropMaskMethod = "rast",
  
  deleteTiles = FALSE,
  
  #Set directory paths
  dirNlData = "data",
  
  dirNlGasFlares = "gasflares",
  
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
  
  #the polygon type to download from GADM
  #essentially the options on the gadm site that we support
  #we support:
  #shpZip: shapefile zip folder(default),
  #spRds: R spatial polygons for R (sp package) in Rds format
  #we list but do not yet support:
  #gpkgZip: Geopackage zip
  #kmlZip: kml zip
  #sfRds: simple features for R (sf package) in Rds format
  gadmPolyType = "shpZip",
  
  #gdalCacheMax Speeds up gdal_rasterize calculation of stats in function
  #ZonalPipe with more cache (advice: max 1/3 of your total RAM)
  #ref: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  gdalCacheMax = 2048,

  #if multiTileStrategy is set to merge where tiles are merged
  #before processing set the function to merge with. The function
  #is sent to gdal
  multiTileMergeFun = "mean",
  
  #how should we handle a situation where we have multiple tiles
  #for the same nlPeriod? Possible options:
  #  + first - use only the first tile
  #  + last  - use only the last tile
  #  + all - 
  #  + delimited ints (1,2) - use specified tile indices
  
  multiTileStrategy = "all",
  
  #default stats to calculate in processNlData. Can be added to if the
  #function exists i.e. if not a standard function can be created in workspace
  #used if nlStats is not supplied to getCtryNlData et al.
  nlStats = list(list("sum", "na.rm=T"), list("mean", "na.rm=TRUE")),
  
  #urls for raster tile listings. In theory, can be used to override the 
  #url if it is changed and before the package is updated
  ntLtsIndexUrlOLS.Y = "https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html",
  
  ntLtsIndexUrlVIIRS.D = "https://ngdc.noaa.gov/eog/viirs/download_ut_mos_tile_iframe.html",
  
  #updated: 20190731. Data at old site stopped 15th July, 2019
  ntLtsIndexUrlVIIRS.M_OLD = "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html",
  
  ntLtsIndexUrlVIIRS.Y_OLD = "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html",
  
  ntLtsIndexUrlVIIRS.M = "https://eogdata.mines.edu/download_dnb_composites_iframe.html",
  
  ntLtsIndexUrlVIIRS.Y = "https://eogdata.mines.edu/download_dnb_composites_iframe.html",

  #the number of cores to use for parallel processing
  numCores = 2,
  
  #the number of parallel connections to make per download
  #used by aria to possibly speed up downloads. 
  numParDnldConns = 2,
  
  #countries to not process. useful if many countries being processed
  #and want to exclude a few
  omitCountries = "missing",
  
  #should we apply gas flare removal to the rasters before calculating
  #nlStats. The default is TRUE
  removeGasFlares = TRUE,
  
  #Change the temp dir to use e.g. if the system temp dir does not have enough space
  #Not used yet
  tmpDir = raster::tmpDir(),

  .allowed = list(
    configName_OLS.Y = settings::inlist("avg_lights_x_pct", "avg_vis", "cf_cvg", "pct_lights", "stable_lights"),
    configName_VIIRS.D = settings::inlist("vcmcfg", "vcmsl"),
    configName_VIIRS.M = settings::inlist("vcmcfg", "vcmsl"),
    configName_VIIRS.Y = settings::inlist("vcm-orm", "vcm-orm-ntl", "vcm-ntl"),
    cropMaskMethod = settings::inlist("gdal", "rast"),
    deleteTiles = settings::inlist(FALSE, TRUE),
    downloadMethod = settings::inlist("aria", "auto", "curl", "libcurl", "wget"),
    extractMethod = settings::inlist("gdal", "rast"),
    gadmVersion = settings::inlist("2.8", "3.6"),
    gadmPolyType = settings::inlist("gpkgZip", "kmlZip", "shpZip", "sfRds", "spRds"),
    multiTileStrategy = settings::inlist("first","last", "all"),
    omitCountries = settings::inlist("all", "error", "long", "missing", "none"),
    removeGasFlares = settings::inlist(FALSE, TRUE)
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
#' \item{\code{configName_OLS.Y}}{(\code{character}) The regex to uniquely
#'      identify the tile file to use out of the downloaded tile .tar. The
#'      data are available as annual composited series:
#'      \itemize{
#'          \item cf_cvg \emph{(Cloud-free coverages)}: 
#'           tally the total number of observations that went into each 30 arc
#'            second grid cell. This image can be used to identify areas with
#'            low numbers of observations where the quality is reduced.
#'            In some years there are areas with zero cloud- free observations
#'            in certain locations.
#'      
#'          \item avg_vis \emph{(Raw average visible band)}: contains the
#'           average of the visible band digital number values with no further
#'           filtering. Data values range from 0-63. Areas with zero cloud-free
#'           observations are represented by the value 255.
#'          
#'          \item stable_lights \emph{(Cleaned up avg_vis)}: contains the
#'           lights from cities, towns, and other sites with persistent 
#'           lighting, including gas flares. Ephemeral events, such as fires 
#'           have been discarded. Then the background noise was identified and
#'           replaced with values of zero. Data values range from 1-63. Areas 
#'           with zero cloud-free observations are represented by the value 255.
#'          
#'          \item pct_lights \emph{(Percent detection freq)}: the percentage of
#'          observations where light is detected per grid cell.
#'          
#'          \item avg_lights_x_pct \emph{(Avg vis band x percent detection freq)}:
#'           derived from the average visible band digital number(DN) of
#'            cloud-free light detections multiplied by the percent frequency
#'            of light detection. The inclusion of the percent frequency 
#'            of detection term normalizes the resulting digital values for 
#'            variations in the persistence of lighting. For instance, the 
#'            value for a light only detected half the time is discounted by 50\%.
#'            Note that this product contains detections from fires and a 
#'            variable amount of background noise. This is the product used 
#'            to infer gas flaring volumes from the nighttime lights.
#'            }
#'          }
#'      
#'  \item{\code{configName_VIIRS.D}}{(\code{character}) The regex to uniquely
#'      identify the tile file to use out of the downloaded tile .tgz. The
#'      version 1 monthly series is run globally using two different 
#'      configurations:
#'      \itemize{
#'          \item vcmcfg \emph{(Stray Light Removed)}: Excludes any
#'          data impacted by stray light.
#'      
#'          \item vcmsl \emph{(Stray Light Corrected)}: includes data
#'           impacted by stray light if the radiance values have undergone the stray-
#'          light correction procedure.
#'      }
#'      
#'      The "vcmsl" version, that includes the stray-light corrected data,
#'      will have more data coverage toward the poles, but will be of reduced
#'      quality.
#'          
#'      It is up to the users to determine which set is best for their
#'      applications. The annual versions are only made with the "vcm"
#'      version, excluding any data impacted by stray light.}
#'      
#'  \item{\code{configName_VIIRS.M}}{(\code{character}) The regex to uniquely
#'      identify the tile file to use out of the downloaded monthly .tgz
#'      tile. Has the same options as configName.VIIRS.D}
#'  
#'  \item{\code{configName_VIIRS.Y}}{(\code{character}) The regex to uniquely
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
#'  \item{\code{gdalCacheMax}}{(\code{numeric}) The maximum memory gdal 
#'      should use in gdal_rasterize }
#'  \item{\code{ntLtsIndexUrlOLS.Y}}{(\code{character}) The url with the OLS 
#'      tile index }
#'  \item{\code{ntLtsIndexUrlVIIRS}}{(\code{character}) The url with the 
#'      VIIRS tile index }
#'  \item{\code{numCores}}{(\code{integer}) The number of processor cores 
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

######################## pkgDefaults ###################################

#' Retrieve default global options for the Rnightlights package
#' 
#' Retrieve default global options for the Rnightlights package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]}
#'     pairs to set options. See \code{pkgOptions} for details.
#'
#' @examples
#' #get cropMaskMethod
#' pkgDefaults("cropMaskMethod") #returns default "rast"
#' 
#' @export
pkgDefaults <- function(...)
{
  settings::defaults(RNIGHTLIGHTSOPTIONS)[...]
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