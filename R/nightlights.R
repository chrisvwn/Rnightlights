#TODO:
#+ gzip all outputrasters and extract/delete tifs as required :V2
#+ delete the 2nd tif in the tiles (avg_rad_...). :V2
#+ keep tiles gzipped until required. extract/delete as needed: V2
#+ modularize everything, processNlData especially: DONE
#+ give functions better names more descriptive: DONE
#+ validation of inputs, error handling: PARTIALLY DONE
#+ give temp files unique names to avoid problems in case of parallelization: DONE
#+ settings and default settings list/DF: DONE
#+ optimize download of tiles: PARTIALLY DONE aria2
#+     Check available methods and try in prioritized order : V2
#+     Retry number of times, resume downloads
#+ zone files functions: DONE
#+ logging
#+ debug mode
#+ do not export internal functions?: DONE
#+ remove dependency on rworldmap?: NA
#+ aggregating by date e.g. quarterly, semi-annually, annually :V2
#+ verify treatment of ATA i.e. single adm level countries: DONE
#+ logic of getCtryPolyAdmLevelNames esp lvlEngName assignment needs scrutiny: DONE
#+ OLS : DONE
#+ store data in RDS format instead of CSV(?): V2

#Notes: gdalwarp is not used for cropping because the crop_to_cutline option causes a shift in the cell locations which then affects the stats extracted. A gdal-based crop to extent would be highly desirable for performance reasons though so seeking other gdal-based workarounds

# if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')
#
# pacman::p_load(readr, dplyr, lubridate, rgdal, raster, sp, rgeos, rworldmap, cleangeo, foreach, doParallel, compiler, gdalUtils, data.table, ff, validate)
#
# require(readr)
# require(dplyr)
# library(data.table)
# require(ff)
# require(validate)
# 
# require(lubridate)
#
# require(rgdal)
# require(gdalUtils)
# require(raster)
#
# require(sp)
# require(rgeos)
# require(rworldmap)
# require(cleangeo)
#
# require(foreach) #Enables for-each statements to be used for parallel processing
# require(doParallel) #Allows for parallel processing using multiple cores
#
# require(compiler)

######################## processNLCountry ###################################

#' Processes nightlights for an individual country in a particular nlPeriod 
#'
#' Given a \code{countryCode}, \code{yearMonth} and preferred processing methods 
#'     \code{cropMaskMethod} and \code{extractMethod}, this function will first check if the 
#'     data already exists in the cache. First it will check if the data file exists and if it 
#'     does not it will create a dataframe of the country data containing only the administrative
#'     properties and move to processing. If the data file exists it will check to see if the 
#'     particular year month already exists. If it exists, it will exit with a message. If it does
#'     not exist, it will load the country data file and move on to processing.
#' 
#'     Processing consists of:
#'     \enumerate{ 
#'        \item Reading in the country polygon in ESRI Shapefile format
#'        \item Reading in the tiles that the particular country intersects with and then clipping 
#'        the tile(s) to the country boundary
#'        \item Extract the data from the clipped raster and compute various statistics at the lowest admin level in the country.
#'        \item Finally, these stats are appended to the data frame and written to the data file.
#'     }
#' 
#'     NOTE: \code{processNLCountry()} assumes that all inputs are available and will not 
#'     attempt to download them. It should ideally be called from the function \code{processNlData()}
#'     which does all the preparation for processing. \code{processNlData()} which can process 
#'     multiple countries and time periods will download all the required tiles and polygons prior to
#'     calling \code{processnlcountry}. \code{getCtryNlData} can also be used with the option
#'     \code{ignoreMissing=FALSE} which will call \code{processNlData} in the background.
#'
#' @param ctryCode character string The ctryCode of interest
#'
#' @param nlPeriod character string The nlPeriod of interest
#' 
#' @param nlType character string The nlType of interest
#'
#' @param cropMaskMethod ("rast" or "gdal") Whether to use rasterize or gdal-based functions to 
#'     crop and mask the country rasters
#'     
#' @param extractMethod ("rast" or "gdal") Whether to use rasterize or gdal-based functions 
#'     to crop and mask the country rasters
#' 
#' @param fnStats the statistics to calculate. If not provided will calculate the stats specified 
#'     in \code{pkgOptions("stats")}
#'
#' @return None
#'
#' @examples
#' 
#' #calculate only the sum of VIIRS radiances for Dec 2012 using gdal
#' #for both cropMask and extraction for KEN
#' \dontrun{processNLCountry("KEN", "201212", "VIIRS", "gdal", "gdal", "sum")}
#'
processNLCountry <- function(ctryCode, nlPeriod, nlType, cropMaskMethod=pkgOptions("cropMaskMethod"), extractMethod=pkgOptions("extractMethod"), fnStats=pkgOptions("stats"))
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(missing(nlType))
    stop("Missing required parameter nlType")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validNlPeriod(nlPeriod, nlType))
    stop("Invalid nlPeriod: ", nlPeriod, " for nlType ", nlType)
  
  if(!validNlType(nlType))
    stop("Invalid nlType: ", nlType)
  
  message("processNLCountry: ", ctryCode, " ", nlType, " ", nlPeriod)

  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  message("Check for existing data file")
  
  if (existsCtryNlDataFile(ctryCode))
  {
    message("Data file found: ", getCtryNlDataFnamePath(ctryCode))
    
    if(all(sapply(fnStats, function(stat) existsCtryNlData(ctryCode, nlPeriod, stat, nlType))))
    {
      message("All stats exist for ", ctryCode, " ", nlPeriod, ". Skipping")

      return(-1)
    }
    
    message("Load country data file")
    ctryNlDataDF <- utils::read.csv(getCtryNlDataFnamePath(ctryCode))
    
    message("Load country polygon lowest admin level")
    ctryPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLowestLyrName(ctryCode))
    
    ctryExtent <- raster::extent(ctryPoly)
    
    raster::projection(ctryPoly) <- sp::CRS(wgs84)
    
  } else
  {
    message("Data file not found. Creating ...")
    
    ctryNlDataDF <- createCtryNlDataDF(ctryCode)
    
    message("Data file not found. Creating ... DONE")
    
    message("Load country polygon lowest admin level")
    
    ctryPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLowestLyrName(ctryCode))
  }
  
  if(!file.exists(getCtryRasterOutputFname(ctryCode = ctryCode, nlPeriod = nlPeriod, nlType = nlType)))
  {
    message("Begin processing ", nlPeriod, " ", base::date())
    
    message("Reading in the rasters " , base::date())
    
    tileList <- getCtryTileList(ctryCode, nlType)
    
    ctryRastCropped <- NULL
    
    for (tile in tileList)
    {
      rastFilename <- getNlTileTifLclNamePath(nlType, nlPeriod, tileName2Idx(tile, nlType))
      
      rastTile <- raster::raster(rastFilename)
      
      raster::projection(rastTile) <- sp::CRS(wgs84)
      
      message("Cropping the rasters", base::date())
      
      #extTempCrop <- crop(rastTile, ctryExtent)
      
      tempCrop <- raster::crop(rastTile, ctryPoly)
      
      if(is.null(ctryRastCropped))
      {
        ctryRastCropped <- tempCrop
        
        #ctryExtCropped <- extTempCrop
      }
      else
      {
        ctryRastMerged <- ctryRastCropped
        
        ctryRastCropped <- NULL
        
        ctryRastCropped <- raster::merge(ctryRastMerged, tempCrop)
        
        rm(ctryRastMerged)
      }
      
      rm(tempCrop)
    }
    
    rm(rastTile)
    
    gc()
    
    message("Masking the merged raster ", base::date())
    
    if (cropMaskMethod == "rast")
    {
      
      #RASTERIZE
      message("Crop and mask using rasterize ", base::date())
      ctryRastCropped <- raster::rasterize(ctryPoly, ctryRastCropped, mask=TRUE) #crops to polygon edge & converts to raster
      
      message("Writing the merged raster to disk ", base::date())
      
      raster::writeRaster(x = ctryRastCropped, filename = getCtryRasterOutputFname(ctryCode, nlType, nlPeriod), overwrite=TRUE)
      
      message("Crop and mask using rasterize ... Done", base::date())
    }
    else if (cropMaskMethod == "gdal")
    {
      message("Crop and mask using gdalwarp ... ", base::date())
      
      #GDALWARP
      rstTmp <- file.path(getNlDir("dirNlTiles"), paste0(basename(tempfile()), ".tif"))
      
      message("Writing merged raster to disk for gdal")
      
      raster::writeRaster(ctryRastCropped, rstTmp)
      
      output_file_vrt <- file.path(getNlDataPath(), paste0(ctryCode, "_", nlType, "_", nlPeriod, ".vrt"))
      
      if (file.exists(output_file_vrt))
        file.remove(output_file_vrt)
      
      message("gdalwarp ",base::date())
      
      gdalUtils::gdalwarp(srcfile=rstTmp, dstfile=output_file_vrt, s_srs=wgs84, t_srs=wgs84, cutline=getPolyFnamePath(ctryCode), cl= getCtryShpLyrName(ctryCode,0), multi=TRUE, wm=2048, wo="NUM_THREADS=ALL_CPUS")
      
      message("gdal_translate ", base::date())
      gdalUtils::gdal_translate(co = "compress=LZW", src_dataset = output_file_vrt, dst_dataset = getCtryRasterOutputFname(ctryCode, nlType, nlPeriod))
      
      message("Deleting the component rasters ", base::date())
      
      file.remove(rstTmp)
      file.remove(output_file_vrt)
      
      ctryRastCropped <- raster::raster(getCtryRasterOutputFname(ctryCode, nlType, nlPeriod))
      #GDALWARP
      message("Crop and mask using gdalwarp ... DONE", base::date())
    }
  }
  else
  {
    rastFilename <- getCtryRasterOutputFname(ctryCode, nlType, nlPeriod)
    
    ctryRastCropped <- raster::raster(rastFilename)
    
    raster::projection(ctryRastCropped) <- sp::CRS(wgs84)
  }
  
  message("Create web version of raster", base::date())
  
  #attempting to obtain QGIS display style grayscale stretch minmax of 2%-98% of values
  #message("calculating quantile 2 and 98 ", base::date())
  #system.time(qnts <- sapply(1:1000,FUN =  function(x) quantile(sampleRandom(ctryRastCropped,100), c(0.02,0.98)),simplify = T))
  
  #qnt2 <- mean(qnts[1,])
  #qnt98 <- mean(qnts[2,])
  
  #cmd <- paste0("gdal_translate -co TILED=YES -co COMPRESS=JPEG -ot Byte -scale ", qnt2, " ", qnt98," 0 255 ", getCtryRasterOutputFname(ctryCode,nlYearMonth), " ", dirRasterWeb, "/", ctryCode, "_", nlYearMonth, "_JPEG.tif")
  
  #message("Create web raster ", base::date())
  #system(cmd)
  
  message("Begin extracting the data from the merged raster ", base::date())
  
  if (extractMethod == "rast")
    sumAvgRad <- fnAggRadRast(ctryPoly, ctryRastCropped, fnStats, nlType)
  else if (extractMethod == "gdal")
    sumAvgRad <- fnAggRadGdal(ctryCode, ctryPoly, nlPeriod, fnStats, nlType)
  
  for(stat in fnStats)
    ctryNlDataDF <- insertNlDataCol(ctryNlDataDF, sumAvgRad[,stat], stat, nlPeriod, nlType = nlType)
  
  message("DONE processing ", ctryCode, " ", nlPeriod, " ", base::date())
  
  message("COMPLETE. Writing data to disk")
  
  #Write the country data dataframe to disk
  saveCtryNlData(ctryNlDataDF, ctryCode)
  
  #release the cropped raster
  rm (ctryRastCropped)
  
  #delete temporary raster files
  raster::removeTmpFiles(h = 0)
}

######################## getCtryRasterOutputFname ###################################

#' Get the full path to the file where the cropped VIIRS country raster is stored.
#'
#' Get the full path to the file where the cropped VIIRS country raster is stored. This file is created
#'     when processing the country before extracting the data. It can be used to re-process a country much faster
#'
#' @param ctryCode the ctryCode of interest
#' 
#' @param nlType the nlType of interest
#'
#' @param nlPeriod the nlPeriod of interest
#'
#' @return Character full path to the cropped VIIRS country raster for a country and a given year and month
#'
#' @examples
#' \dontrun{getCtryRasterOutputFname("KEN","VIIRS", "201412")}
#'
#'#export for exploreData() shiny app
#'@export
getCtryRasterOutputFname <- function(ctryCode, nlType, nlPeriod)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlType))
    stop("Missing required parameter nlType")
  
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validNlType(nlType))
    stop("Invalid nlType: ", nlType)
  
  if(!validNlPeriod(nlPeriod, nlType))
    stop("Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  return (file.path(getNlDir("dirRasterOutput"), paste0(ctryCode, "_", nlType, "_", nlPeriod,".tif")))
}

######################## processNlData ###################################

#' Downloads nightlight tiles and country polygons and calls the function to process them
#'
#' Downloads nightlight tiles and country polygons in preparation for the appropriate functions
#'     to process them. Given a list of countries and nlPeriods and an nlType, processNlData 
#'     will first determine which countries and periods do not actually have data i.e. have not 
#'     been previously processed. From the list of countries and prediods that need processing,
#'     it determines which nightlight tiles require to be downloaded. At the same time, it 
#'     downloads any country polygons which have not already been downloaded.
#' 
#'     processNlData then calls processNlCountry with the nlType supplied as a parameter. The
#'     processing is essentially the same for all nlTypes.
#' 
#'     This is the main entry-point to the package and the main user-facing function. However, it 
#'     works in batch mode and caches all data without returning any data to the user. However, it 
#'     will return TRUE/FALSE depending on whether it completed successfully. Since it saves state
#'     regularly it can be run multiply in case of failure until it finally returns TRUE. This is where 
#'     the only constraints are downloading and processing errors due to bandwidth or other 
#'     resource constraints. A good example is running a long-running batch on an AWS EC2 
#'     spot-priced machine where the server may be decommissioned at any time. See more in the examples.
#' 
#' @param ctryCodes the list of countries to be processed
#'
#' @param nlPeriods the nlPeriods of interest
#'
#' @param nlType the type of nightlights to process i.e. "OLS" or "VIIRS". Default "VIIRS"
#' 
#' @param stats the statistics to calculate. If not provided will calculate the stats specified 
#'     in \code{pkgOptions("stats")}
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' #Example 1: process VIIRS nightlights for all countries and all periods available e.g. to create 
#'     #a local cache or repo
#'     
#'     #Recommend running nlInit() to improve performance. It stores some global variables 
#'     #so that they do not have to be re-evaluated multiply
#'     nlInit() 
#'     
#'     processNlData() #process VIIRS nightlights for all countries and all periods
#'
#' #Example 2: process nightlights for all countries in 2012 only
#'     
#'     nlInit() #for performance. See Example 1
#'
#'     nlPeriods <- getAllNlYears("VIIRS") #get a list of all nightlight periods to present-day
#'
#'     nlPeriods <- nlPeriods[grep("^2012", nlPeriods)] #filter only periods in 2012
#'
#'     processNlData(nlPeriods=nlPeriods)
#'
#' #Example 3: process VIIRS nightlights for countries KEN & RWA in 2014 Jan to 2014 May only
#'     
#'     nlInit()
#'
#'     cCodes <- c("KEN", "RWA")
#'
#'     nlPeriods <- getAllNlPeriods("VIIRS")
#'
#'     nlPeriods <- nlPeriods[grep("^20120[1-5]", nlPeriods)]
#'
#'     processNlData(ctryCodes=cCodes, nlPeriods=nlPeriods)
#' 
#' #Example 4: process VIIRS nightlights for countries KEN & RWA in 2014 Oct to 2014 Dec only
#'
#'     processNlData(ctryCodes=c("KEN", "RWA"), nlPeriods=c("201410", "201411", "201412"))
#'     
#' #Example 5: process all nightlights, all countries, all stats in one thread
#' 
#'    processNlData() 
#'    
#' #Example 6: process all nightlights, all countries, all stats with each
#' #   year in a separate thread. Create a separate R script for each year as follows:
#' 
#'     library(Rnightlights)
#' 
#'     nlInit()
#' 
#'     nlPeriods <- getAllNlYears("VIIRS")
#' 
#'     nlPeriods_2012 <- nlPeriods[grep("^2012", nlPeriods)]
#' 
#'     processNlData(nlPeriods=nlPeriods_2012)
#' 
#'     #Run the script from the command line as:
#'     
#'     #R CMD BATCH script_name_2012.R
#'     }
#' @export
processNlData <- function (ctryCodes=getAllNlCtryCodes("all"), nlPeriods=getAllNlPeriods(nlType), nlType="VIIRS", stats=pkgOptions("stats"))
{
  #if the period is not given process all available periods
  if(missing("nlPeriods") || is.null(nlPeriods) || length(nlPeriods) == 0 || nlPeriods == "")
  {
    nlPeriods <- getAllNlPeriods(nlType)
  }
  
  if(!allValid(ctryCodes, validCtryCode))
    stop("Invalid ctryCode detected")
  
  if(!allValid(nlPeriods, validNlPeriod, nlType))
  {
    stop("Invalid nlPeriod(s) detected")
  }
  
  #if the tile mapping does not exist create it
  if (!exists("nlTiles"))
    nlTiles <- getNlTiles(nlType)
  
  #use supplied list of ctryCodes in ISO3 format else use default of all
  #TODO:
  #1.accept other formats and convert as necessary
  #2.verification & deduplication
  if (is.null(ctryCodes))
  {
    #get list of all country codes
    ctryCodes <- getAllNlCtryCodes(omit = "all")
  }
  
  ##First step: Determine which tiles are required for processing. This is determined by the
  #list of ctryCodes. Since theoretically we need the polygons of all countries to determine which
  #tiles to download we take the opportunity to download all required shapefiles
  #Practically, we can stop as soon as all 6 tiles are flagged for download.
  
  ##If any tiles cannot be found/downloaded then abort and try for next country
  #we probably need to flag failed downloads so we don't try to process them and report back to user
  
  #download all country polygons if they don't already exist
  for (ctryCode in ctryCodes)
  {
    dnldCtryPoly(ctryCode)
  }
  
  #for all nlPeriods check if the tiles exist else download
  for (nlPeriod in nlPeriods)
  {
    message("Checking tiles required for ", nlPeriod)
    
    #init the list of tiles to be downloaded
    tileList <- NULL
    
    #determine tiles to download
    ctryTiles <- NULL
    
    tileList <- NULL
    
    #For each country
    for (ctryCode in unique(ctryCodes))
    {
      #Check if all stats exist for the ctryCode
      if (all(sapply(stats, function(stat) existsCtryNlData(ctryCode, nlPeriod, stat, nlType))))
      {
        message ("All stats exist for ", ctryCode, ":", nlPeriod)

        next
      }
      
      message("Stats missing. Adding tiles for ", ctryCode)
      
      #get the list of tiles required for the ctryCode
      ctryTiles <- getCtryTileList(ctryCode, nlType)
      
      #combine the list of unique tiles across all ctryCodes in tileList
      tileList <- c(tileList, setdiff(ctryTiles, tileList))
      
      #if all the unique tiles have been listed no need to proceed checking
      if (length(tileList) == nrow(nlTiles))
      {
        message ("All tiles have been listed. No need to check other country tiles")
        
        break
      }
    }
    
    if (length(tileList) == 0)
    {
      message("No tiles needed for ", nlPeriod, ". Process next nlPeriod")
      
      next
    }
    else #if the cropped raster is not found try to download
    {
      if (!file.exists(getCtryRasterOutputFname(ctryCode, nlType, nlPeriod)))
      {
        if(!downloadNlTiles(nlType, nlPeriod, tileList))
        {
          message("Something went wrong with the tile downloads. Aborting ...")
          
          break
        }
      }
      else
      {
        message("Cropped raster already exists. Skipping tile download")
      }
    }
    
    #for all required countries
    for (ctryCode in unique(ctryCodes))
    {
      processNLCountry(ctryCode, nlPeriod, nlType, cropMaskMethod = pkgOptions("cropMaskMethod"), extractMethod = pkgOptions("extractMethod"), fnStats = stats)
    }
    
    #post-processing. Delete the downloaded tiles to release disk space
    if(pkgOptions("deleteTiles"))
      for (tile in tileList)
      {
        #del the tif file
        if (file.exists(getNlTileTifLclNamePath(nlType, nlPeriod, tileName2Idx(tile, nlType))))
          file.remove(file.path(getNlTileTifLclNamePath(nlType, nlPeriod, tileName2Idx(tile, nlType))))
        
        #del the zip file
        if (file.exists(file.path(getNlTileZipLclNamePath(nlType, nlPeriod, tileName2Idx(tile, nlType)))))
          file.remove(file.path(getNlTileZipLclNamePath(nlType, nlPeriod, tileName2Idx(tile, nlType))))
      }
  }
}