#TODO:
#+ gzip all outputrasters and extract/delete tifs as required/alt compress :V3
#+ delete the 2nd tif in the tiles (avg_rad_...). :Cancelled
#+ keep tiles gzipped until required. extract/delete as needed: V3
#+ modularize everything, processNlData especially: DONE
#+ give functions better names more descriptive: DONE
#+ validation of inputs, error handling: PARTIALLY DONE
#+ give temp files unique names to avoid problems in case of parallelization: DONE
#+ settings and default settings list/DF: DONE
#+ optimize download of tiles: PARTIALLY DONE aria2
#+     Check available methods and try in prioritized order : shelved
#+     Retry number of times, resume downloads: retry-shelved, resume-aria
#+ zone files functions: DONE
#+ logging: Not yet
#+ debug mode: combined with logging
#+ do not export internal functions?: DONE
#+ remove dependency on rworldmap?: Shelved
#+ aggregating by date e.g. quarterly, semi-annually, annually :V3
#+ verify treatment of ATA i.e. single adm level countries: DONE
#+ logic of getCtryPolyAdmLevelNames esp lvlEngName assignment needs scrutiny: DONE
#+ OLS : DONE
#+ store data in RDS/json format instead of CSV(?): Required?
#+ Name all parameters in function calls to future proof code: 90%
#+ Save shapefiles as RDS for quicker access?: DONE

#Notes: gdalwarp is not used for cropping because the crop_to_cutline option
#causes a shift in the cell locations which then affects the stats extracted.
#A gdal-based crop to extent would be highly desirable for performance reasons
#though so seeking other gdal-based workarounds

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
#' Given a \code{countryCode}, \code{yearMonth} and preferred processing 
#'     methods \code{cropMaskMethod} and \code{extractMethod}, this function 
#'     will first check if the data already exists in the cache. First it 
#'     will check if the data file exists and if it does not it will create 
#'     a dataframe of the country data containing only the administrative
#'     properties and move to processing. If the data file exists it will 
#'     check to see if the particular year month already exists. If it 
#'     exists, it will exit with a message. If it does not exist, it will 
#'     load the country data file and move on to processing.
#' 
#'     Processing consists of:
#'     \itemize{ 
#'        \item Reading in the country polygon in ESRI Shapefile format
#'        \item Reading in the tiles that the particular country intersects 
#'            with and then clipping 
#'        the tile(s) to the country boundary
#'        \item Extract the data from the clipped raster and compute various 
#'            statistics at the lowest admin level in the country.
#'        \item Finally, these stats are appended to the data frame and 
#'            written to the data file.
#'     }
#' 
#'     NOTE: \code{processNLCountry()} assumes that all inputs are available 
#'     and will not attempt to download them. It should ideally be called 
#'     from the function \code{processNlData()} which does all the 
#'     preparation for processing. \code{processNlData()} which can process 
#'     multiple countries and time periods will download all the required 
#'     tiles and polygons prior to calling \code{processNlCountry}. 
#'     \code{getCtryNlData} can also be used with the option 
#'     \code{ignoreMissing=FALSE} which will call \code{processNlData} 
#'     in the background.
#'
#' @param ctryCode \code{character} The ctryCode of interest
#' 
#' @param admLevel \code{character} The country admin level in the given 
#'     ctryCode at which to calculate stats
#'
#' @param nlType \code{character} The nlType of interest
#' 
#' @param configName character the type of raster being processed
#' 
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#' 
#' @param multiTileMergeFun character The function to use to merge tiles
#' 
#' @param removeGasFlares logical Whether to perform gas flare removal pre-processing
#'
#' @param nlPeriod \code{character} The nlPeriod of interest
#' 
#' @param nlStats the statistics to calculate. If not provided will calculate 
#'     the stats specified in \code{pkgOptions("nlStats")}
#' 
#' @param downloadMethod The method used to download polygons and rasters
#' 
#' @param cropMaskMethod \code{character} Whether to use rasterize or 
#'     gdal-based functions to crop and mask the country rasters
#'     
#' @param extractMethod ("rast" or "gdal") Whether to use rasterize or 
#'     gdal-based functions to crop and mask the country rasters
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return None
#'
#' @examples
#' 
#' #calculate only the sum of monthly VIIRS radiances for Dec 2014 using gdal
#' #for both cropMask and extraction for KEN
#' \dontrun{
#' Rnightlights:::processNLCountry("KEN", "KEN_adm2", "VIIRS.M", "201412", "gdal", "gdal", "sum")
#' }
#'
processNLCountry <- function(ctryCode,
                             admLevel,
                             nlType,
                             configName = pkgOptions(paste0("configName_", nlType)),
                             multiTileStrategy = pkgOptions("multiTileStrategy"),
                             multiTileMergeFun = pkgOptions("multiTileMergeFun"),
                             removeGasFlares = pkgOptions("removeGasFlares"),
                             nlPeriod,
                             nlStats=pkgOptions("nlStats"),
                             downloadMethod=pkgOptions("downloadMethod"),
                             cropMaskMethod=pkgOptions("cropMaskMethod"),
                             extractMethod=pkgOptions("extractMethod"),
                             gadmVersion=pkgOptions("gadmVersion"),
                             gadmPolyType=pkgOptions("gadmPolyType"),
                             custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop(Sys.time(), "Missing required parameter ctryCode")
  
  if(missing(nlPeriod))
    stop(Sys.time(), "Missing required parameter nlPeriod")
  
  if(missing(nlType))
    stop(Sys.time(), "Missing required parameter nlType")
  
  if(!validCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), "Invalid ctryCode: ", ctryCode)
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), "Invalid nlPeriod: ", nlPeriod, " for nlType ", nlType)
  
  if(!validNlTypes(nlTypes = nlType))
    stop(Sys.time(), "Invalid nlType: ", nlType)
  
  if(missing(admLevel))
    admLevel <- getCtryShpLowestLyrNames(ctryCodes=ctryCode,
                                         gadmVersion=gadmVersion,
                                         gadmPolyType = gadmPolyType,
                                         custPolyPath=custPolyPath)
  
  if(is.list(nlStats) &&
     length(nlStats) > 1 &&
     all(sapply(2:length(nlStats),
                function(i)
                  !is.list(nlStats[[i]]) &&
                (grepl("=", nlStats[i]) || length(names(nlStats[i])) > 0))))
    nlStats <- list(nlStats)
  
  funArgs <- match.call()[-1]
  
  message(Sys.time(), ": ProcessNlCountry: ", paste(names(funArgs), sapply(names(funArgs), function(x)eval(parse(text = x))), sep="=",collapse = ", "), "****")
  
  wgs84 <- getCRS()
  
  message(Sys.time(), ": Check for existing data file")
  
  ctryNlDataFnamePath <- getCtryNlDataFnamePath(ctryCode = ctryCode,
                                                   admLevel = admLevel,
                                                   gadmVersion = gadmVersion,
                                                   gadmPolyType = gadmPolyType,
                                                   custPolyPath = custPolyPath)

  message(Sys.time(), ": Load polygon layer for crop")
  ctryPolyAdm0 <- readCtryPolyAdmLayer(ctryCode = ctryCode,
                                       admLevel = unlist(getCtryShpLyrNames(ctryCodes = ctryCode,
                                                                            lyrNums = 0,
                                                                            gadmVersion = gadmVersion,
                                                                            gadmPolyType = gadmPolyType,
                                                                            custPolyPath = custPolyPath)),
                                       gadmVersion = gadmVersion,
                                       custPolyPath = custPolyPath)
  
  if (existsCtryNlDataFile(ctryCode = ctryCode,
                           admLevel = admLevel,
                           gadmVersion = gadmVersion,
                           gadmPolyType = gadmPolyType,
                           custPolyPath = custPolyPath))
  {
    message(Sys.time(), ": Data file found: ", ctryNlDataFnamePath)
    
    existStats <- sapply(nlStats, function(nlStat) existsCtryNlData(ctryCode = ctryCode,
                                                                    admLevel = admLevel,
                                                                    nlTypes = nlType,
                                                                    configNames = configName,
                                                                    multiTileStrategy = multiTileStrategy,
                                                                    multiTileMergeFun = multiTileMergeFun,
                                                                    removeGasFlares = removeGasFlares,
                                                                    nlPeriods = nlPeriod,
                                                                    nlStats = nlStat[[1]],
                                                                    gadmVersion = gadmVersion,
                                                                    gadmPolyType = gadmPolyType,
                                                                    custPolyPath = custPolyPath))
    
    if(all(existStats))
    {
      message(Sys.time(), ": **All stats exist for ", paste(ctryCode, admLevel, nlPeriod, sep=" "), ". Skipping")

      return(-1)
    }
    else
    {
      nlStats <- nlStats[!existStats]
      message(Sys.time(), ": Processing stats: ", paste0(nlStats, collapse = ","))
    }
    
    message(Sys.time(), ": Load country data file")
    ctryNlDataDF <- utils::read.csv(ctryNlDataFnamePath, header = TRUE, check.names = FALSE, encoding = "UTF-8")
    
    message(Sys.time(), ": Load country polygon admin level")
    
    ctryExtent <- raster::extent(x = ctryPolyAdm0)
    
    raster::projection(ctryPolyAdm0) <- sp::CRS(projargs = wgs84)
    
  } else
  {
    message(Sys.time(), ": Data file not found. Creating ...")
    
    ctryNlDataDF <- createCtryNlDataDF(ctryCode = ctryCode,
                                       admLevel = admLevel,
                                       gadmVersion = gadmVersion,
                                       gadmPolyType = gadmPolyType,
                                       custPolyPath = custPolyPath)
    
    message(Sys.time(), ": Data file not found. Creating ... DONE")
  }

  message(Sys.time(), ": Begin processing ", nlPeriod)
  
  #get the path we will use to save the cropped raster
  ctryRasterOutputFnamePath <- getCtryRasterOutputFnamePath(ctryCode = ctryCode,
                                                            nlType = nlType,
                                                            configName = configName,
                                                            multiTileStrategy = multiTileStrategy,
                                                            multiTileMergeFun = multiTileMergeFun,
                                                            removeGasFlares = removeGasFlares,
                                                            nlPeriod = nlPeriod,
                                                            gadmVersion = gadmVersion,
                                                            gadmPolyType = gadmPolyType,
                                                            custPolyPath = custPolyPath)
  
  if(!file.exists(ctryRasterOutputFnamePath))
  {
    message(Sys.time(), ": Country output raster not found. Creating")
    
    message(Sys.time(), ": Reading in the raster tiles")
    
    tileList <- getCtryTileList(ctryCodes = ctryCode, nlType = nlType)
    
    ctryRastCropped <- NULL
    
    for (tile in tileList)
    {
      rastFilename <- getNlTileTifLclNamePath(nlType = nlType,
                                              configName = configName,
                                              nlPeriod = nlPeriod,
                                              tileNum = tileName2Idx(tileName = tile,
                                                                     nlType = nlType))
      
      rastTile <- raster::raster(x = rastFilename)
      
      raster::projection(rastTile) <- sp::CRS(projargs = wgs84)
      
      ctryPolyAdm0 <- sp::spTransform(ctryPolyAdm0, sp::CRS(wgs84))
      
      message(Sys.time(), ": Cropping the raster tiles ")
      
      #extTempCrop <- crop(rastTile, ctryExtent)
      
      if(removeGasFlares)
      {
        #read in the mosaiced gas flare polys
        message(Sys.time(), ": Gas flare removal is ON")
        
        #does this country require gas flare removal
        if(hasNlCtryGasFlares(ctryCode = ctryCode,
                              gadmVersion = gadmVersion,
                              gadmPolyType = gadmPolyType,
                              custPolyPath = custPolyPath))
        {
          message(Sys.time(), ": Processing gas flare removal")
         
          ctryPolyAdm0 <- getNlCtryGasFlaresPoly(ctryCode,
                                                 gadmVersion = gadmVersion,
                                                 gadmPolyType = gadmPolyType,
                                                 custPolyPath = custPolyPath)
          
        } else
        {
          message(Sys.time(), ": Gas flare removal not required for: ", ctryCode)
        }
      } else
      {
        message(Sys.time(), ": Gas flare removal is OFF. Skipping")
      }
      
      message(Sys.time(), ": Cropping tile = ", tile)
      
      #we crop using raster for both rast and gdal
      tempCrop <- raster::crop(x = rastTile, y = ctryPolyAdm0, progress='text')
      
      #will only be non-null if there are multiple tiles to be mosaiced i.e. a country
      #that straddles multiple tiles. So in the 2nd+ loop ctryRastCropped will not be null
      if(is.null(ctryRastCropped))
      {
        ctryRastCropped <- tempCrop
        
        #ctryExtCropped <- extTempCrop
      }
      else
      {
        #if we are here we are mosaicing multiple tiles
        ctryRastMerged <- ctryRastCropped
        
        ctryRastCropped <- NULL
        
        #mosaic the tiles and store back in ctryRastCropped
        ctryRastCropped <- raster::merge(x = ctryRastMerged, y = tempCrop)
        
        rm(ctryRastMerged)
      }
      
      rm(tempCrop)
    }
    
    #unload the tile from memory
    rm(rastTile)
    
    #release unused memory
    gc()
    
    message(Sys.time(), ": Masking the raster ")
    
    if (cropMaskMethod == "rast")
    {
      #RASTERIZE
      message(Sys.time(), ": Mask using rasterize ")
      
      ctryRastCropped <- raster::rasterize(x = ctryPolyAdm0, y = ctryRastCropped, mask=TRUE, progress="text") #crops to polygon edge & converts to raster
      
      message(Sys.time(), ": Writing the raster to disk ")
      
      raster::writeRaster(x = ctryRastCropped,
                          filename = ctryRasterOutputFnamePath,
                          overwrite=TRUE, progress="text")
      
      message(Sys.time(), ": Crop and mask using rasterize ... Done")
    }
    else if (cropMaskMethod == "gdal")
    {
      message(Sys.time(), ": Crop and mask using gdalwarp ... ")
      
      #GDALWARP
      rstTmp <- file.path(getNlDir(dirName = "dirNlTemp"), paste0(basename(tempfile()), ".tif"))
      
      message(Sys.time(), ": Writing merged raster to disk for gdalwarp masking")
      
      raster::writeRaster(x = ctryRastCropped, filename = rstTmp, progress="text")
      
      ctryRastCropped <- NULL
      
      gc()
      
      #the polygon will already correspond to having gas flares removed or not
      ctryPolyAdm0TmpDir <- tools::file_path_sans_ext(rstTmp)
      
      rgdal::writeOGR(obj = methods::as(ctryPolyAdm0,"SpatialPolygonsDataFrame"), dsn = ctryPolyAdm0TmpDir, driver = "ESRI Shapefile", layer = "GID_0_IDX")
      
      outputFileVrt <- file.path(getNlDir(dirName = "dirNlTemp"), paste0(ctryCode, "_", nlType, "_", nlPeriod, ".vrt"), fsep = )
      
      if (file.exists(outputFileVrt))
        file.remove(outputFileVrt)
      
      message(Sys.time(), ": gdalwarp masking to VRT")
      
      gdalUtils::gdalwarp(srcfile=rstTmp,
                          dstfile=outputFileVrt,
                          s_srs=wgs84, t_srs=wgs84,
                          cutline=ctryPolyAdm0TmpDir,
                          cl="GID_0_IDX",
                          multi=TRUE,
                          wm=pkgOptions("gdalCacheMax"),
                          wo=paste0("NUM_THREADS=",
                                    pkgOptions("numCores")),
                          q = FALSE)

      message(Sys.time(), ": gdal_translate converting VRT to TIFF ")
      gdalUtils::gdal_translate(co = "compress=LZW",
                                src_dataset = outputFileVrt,
                                dst_dataset = ctryRasterOutputFnamePath)
      
      message(Sys.time(), ": Deleting the component rasters ")
      
      file.remove(rstTmp)
      file.remove(outputFileVrt)
      unlink(ctryPolyAdm0TmpDir, recursive = T, force = T)
      
      ctryRastCropped <- raster::raster(ctryRasterOutputFnamePath)
      
      #GDALWARP
      message(Sys.time(), ": Crop and mask using gdalwarp ... DONE")
    }
  }
  else
  {
    rastFilename <- ctryRasterOutputFnamePath
    
    message(Sys.time(), ": ", rastFilename, " already exists")
    
    ctryRastCropped <- raster::raster(x = rastFilename)
    
    raster::projection(x = ctryRastCropped) <- sp::CRS(projargs = wgs84)
  }
  
  #message(Sys.time(), ": Create web version of raster")
  
  #gdal_translate -co COMPRESS=JPEG -co PHOTOMETRIC=YCBCR -co TILED=YES 5255C.tif 5255C_JPEG_YCBCR.tif
  
  #gdalUtils::gdal_translate(src_dataset = rastFilename,
  #                          dst_dataset = rastWebFilename,
  #                          co = "COMPRESS=JPEG PHOTOMETRIC=YCBCR TILED=YES")
  
  #gdaladdo --config COMPRESS_OVERVIEW JPEG --config PHOTOMETRIC_OVERVIEW YCBCR 
  #--config INTERLEAVE_OVERVIEW PIXEL -r average 5255C_JPEG_YCBCR.tif 2 4 8 16
  
  #rastWebFilename <- file.path(getNlDir("dirRasterWeb"), basename(rastFilename))
  
  #gdalUtils::gdaladdo(filename = rastWebFilename, r = "average", levels = c(2, 4, 8, 16))
  
  #message(Sys.time(), ": End create web raster ")
  #system(cmd)
  
  message(Sys.time(), ": Begin extracting data from the raster ")
  
  ctryPoly <- readCtryPolyAdmLayer(ctryCode = ctryCode,
                                   admLevel = admLevel,
                                   gadmVersion = gadmVersion,
                                   gadmPolyType = gadmPolyType,
                                   custPolyPath = custPolyPath)
  
  if (extractMethod == "rast")
    sumAvgRad <- fnAggRadRast(ctryPoly=ctryPoly,
                              ctryRastCropped=ctryRastCropped,
                              nlType=nlType,
                              configName=configName,
                              nlStats=nlStats,
                              custPolyPath = custPolyPath)
  else if (extractMethod == "gdal")
    sumAvgRad <- fnAggRadGdal(ctryCode=ctryCode,
                              admLevel=admLevel,
                              ctryPoly=ctryPoly,
                              nlType=nlType,
                              configName=configName,
                              multiTileStrategy = multiTileStrategy,
                              multiTileMergeFun = multiTileMergeFun,
                              removeGasFlares = removeGasFlares,
                              nlPeriod=nlPeriod,
                              nlStats=nlStats,
                              gadmVersion=gadmVersion,
                              gadmPolyType = gadmPolyType,
                              custPolyPath = custPolyPath)
  
  nlStatNames <- sapply(nlStats, function(x) x[[1]])
  
  for(nlStatName in nlStatNames)
    ctryNlDataDF <- insertNlDataCol(ctryNlDataDF = ctryNlDataDF,
                                    dataCol = sumAvgRad[,nlStatName],
                                    nlStat = nlStatName,
                                    nlPeriod = nlPeriod,
                                    nlType = nlType,
                                    configName = configName,
                                    multiTileStrategy = multiTileStrategy,
                                    multiTileMergeFun = multiTileMergeFun,
                                    removeGasFlares = removeGasFlares)
  
  message(Sys.time(), ": DONE processing ", ctryCode, " ", nlPeriod)
  
  message(Sys.time(), ": **COMPLETE. Writing data to disk")
  
  #Write the country data dataframe to disk
  saveCtryNlData(ctryNlDataDF = ctryNlDataDF,
                 ctryCode = ctryCode,
                 admLevel = admLevel,
                 gadmVersion = gadmVersion,
                 gadmPolyType = gadmPolyType,
                 custPolyPath = custPolyPath)
  
  #release the cropped raster
  rm (ctryRastCropped)
  
  #delete temporary raster files
  raster::removeTmpFiles(h = 0)
}

######################## getCtryRasterOutputFname ###################################

#' Constructs the name of the output raster
#'
#' Constructs the name of the output raster
#'
#' @param ctryCode the ctryCode of interest
#' 
#' @param nlType the nlType of interest
#' 
#' @param configName character the type of raster being processed
#' 
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#' 
#' @param multiTileMergeFun character The function to use to merge tiles
#' 
#' @param removeGasFlares logical Whether to perform gas flare removal pre-processing
#'
#' @param nlPeriod the nlPeriod of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath The path to a custom polygon as an alternative
#'     to using GADM polygons
#'
#' @return Character the name of country raster for a country and a given 
#'     nlType and nlPeriod
#'
#' @examples
#' 
#' Rnightlights:::getCtryRasterOutputFname(ctryCode = "KEN", nlType = "VIIRS.M", nlPeriod = "201412")
#'
getCtryRasterOutputFname <- function(ctryCode,
                                     nlType,
                                     configName = pkgOptions(paste0("configName_", nlType)),
                                     multiTileStrategy = pkgOptions("multiTileStrategy"),
                                     multiTileMergeFun = pkgOptions("multiTileMergeFun"),
                                     removeGasFlares = pkgOptions("removeGasFlares"),
                                     nlPeriod,
                                     gadmVersion = pkgOptions("gadmVersion"),
                                     gadmPolyType=pkgOptions("gadmPolyType"),
                                     custPolyPath = NULL)
{
  if(missing(ctryCode))
    stop(Sys.time(), "Missing required parameter ctryCode")
  
  if(missing(nlType))
    stop(Sys.time(), "Missing required parameter nlType")
  
  if(missing(nlPeriod))
    stop(Sys.time(), "Missing required parameter nlPeriod")
  
  if(!validCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), "Invalid ctryCode: ", ctryCode)
  
  if(!validNlTypes(nlTypes = nlType))
    stop(Sys.time(), "Invalid nlType: ", nlType)
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), "Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)

  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  configName <- toupper(configName)
  
  fname <- if(is.null(custPolyPath))
             paste0("NL_", ctryCode, "_",
                    nlType, "_",
                    nlPeriod, "_",
                    configName,
                    "-MTS", toupper(multiTileStrategy), "-", toupper(multiTileMergeFun),
                    "-RGF", substr(as.character(removeGasFlares),1,1),
                    "_GADM-", gadmVersion, "-", toupper(gadmPolyType), ".tif")
           else
             paste0("NL_", ctryCode, "_",
                    nlType, "_",
                    nlPeriod, "_",
                    configName, "-MTS", toupper(multiTileStrategy), "-", toupper(multiTileMergeFun),
                    "-RGF", substr(as.character(removeGasFlares),1,1),
                    "_CUST-", basename(custPolyPath), "-SHPZIP.tif")
  return (fname)
}

######################## getCtryRasterOutputFnamePath ###################################

#' Get the full path to the file where the cropped VIIRS country raster is stored.
#'
#' Get the full path to the file where the cropped VIIRS country raster is stored. This file is created
#'     when processing the country before extracting the data. It can be used to re-process a country much faster
#'
#' @param ctryCode the ctryCode of interest
#' 
#' @param nlType the nlType of interest
#' 
#' @param configName character the type of raster being processed
#' 
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#' 
#' @param multiTileMergeFun character The function to use to merge tiles
#' 
#' @param removeGasFlares logical Whether to perform gas flare removal pre-processing
#'
#' @param nlPeriod the nlPeriod of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#'  
#' @param custPolyPath The path to a custom polygon as an alternative
#'     to using GADM polygons
#'     
#' @return Character full path to the cropped VIIRS country raster for a country and a given year and month
#'
#' @examples
#' \dontrun{
#' getCtryRasterOutputFnamePath("KEN","VIIRS.M", "201412")
#' }
#'
#'#export for exploreData() shiny app
#'@export
getCtryRasterOutputFnamePath <- function(ctryCode,
                                         nlType,
                                         configName,
                                         multiTileStrategy = pkgOptions("multiTileStrategy"),
                                         multiTileMergeFun = pkgOptions("multiTileMergeFun"),
                                         removeGasFlares = pkgOptions("removeGasFlares"),
                                         nlPeriod,
                                         gadmVersion=pkgOptions("gadmVersion"),
                                         gadmPolyType=pkgOptions("gadmPolyType"),
                                         custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop(Sys.time(), "Missing required parameter ctryCode")
  
  if(missing(nlType))
    stop(Sys.time(), "Missing required parameter nlType")
  
  if(missing(nlPeriod))
    stop(Sys.time(), "Missing required parameter nlPeriod")
  
  if(missing("configName") || is.na(configName) || length(configName) == 0 || configName=="")
    configName <- pkgOptions(paste0("configName_", nlType))
  
  if(!validCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), "Invalid ctryCode: ", ctryCode)
  
  if(!validNlTypes(nlTypes = nlType))
    stop(Sys.time(), "Invalid nlType: ", nlType)
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), "Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  return (file.path(getNlDir("dirRasterOutput"),
                    getCtryRasterOutputFname(ctryCode = ctryCode,
                                             nlType = nlType,
                                             configName = configName,
                                             multiTileStrategy = multiTileStrategy,
                                             multiTileMergeFun = multiTileMergeFun,
                                             removeGasFlares = removeGasFlares,
                                             nlPeriod = nlPeriod,
                                             gadmVersion = gadmVersion,
                                             gadmPolyType = gadmPolyType,
                                             custPolyPath = custPolyPath)))
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
#' @param admLevels the list of admin levels in the given countries at
#'     which to calculate stats
#'
#' @param nlTypes the types of nightlights to process
#' 
#' @param configNames character the types of raster being processed
#' 
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#' 
#' @param multiTileMergeFun character The function to use to merge tiles
#' 
#' @param removeGasFlares logical Whether to perform gas flare removal pre-processing
#' 
#' @param nlPeriods the nlPeriods of interest
#' 
#' @param nlStats the statistics to calculate. If not provided will calculate
#'     the stats specified in \code{pkgOptions("nlStats")}
#'
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @param downloadMethod The method used to download rasters and polygons
#' 
#' @param cropMaskMethod The method used to crop and mask satellite rasters
#' 
#' @param extractMethod The method used to extract and perform functions on raster data
#' 
#' @return None
#'
#' @examples
#' 
#' #long running examples which may require large downloads
#' \dontrun{
#' #Example 1: process monthly VIIRS nightlights for all countries at the
#'     lowest admin level and for all nlPeriods available e.g. to create a 
#'     local cache or repo
#'     
#'     processNlData() #process VIIRS nightlights for all countries and all periods
#'
#' #Example 2: process monthly VIIRS nightlights for all countries in 2014 only
#'
#'     nlPeriods <- getAllNlPeriods("VIIRS.M") #get a list of all nightlight periods to present-day
#'
#'     nlPeriods <- nlPeriods[grep("^2014", nlPeriods)] #filter only periods in 2014
#'
#'     processNlData(nlTypes="VIIRS.M", nlPeriods=nlPeriods)
#'
#' #Example 3: process OLS nightlights for countries KEN & RWA from 1992
#' #     to 2000
#'
#'     cCodes <- c("KEN", "RWA")
#'
#'     nlPeriods <- getAllNlPeriods("VIIRS.M")
#'
#'     nlPeriods <- nlRange("1992", "2000", "OLS.Y")
#'
#'     processNlData(ctryCodes=cCodes, nlPeriods=nlPeriods)
#' 
#' #Example 4: process VIIRS nightlights for countries KEN & RWA in 2014 Oct to 2014 Dec only
#'
#'     processNlData(ctryCodes=c("KEN", "RWA"), nlTypes="VIIRS.M", 
#'         nlPeriods=c("201410", "201411", "201412"))
#'     
#' #Example 5: process all nightlights, all countries, all stats in one thread
#' 
#'    processNlData() 
#'    
#' #Example 6: process all VIIRS monthly nightlights, all countries, all stats with each
#' #   year in a separate thread. Create a separate R script for each year as follows:
#' 
#'     library(Rnightlights)
#' 
#'     nlPeriods <- getAllNlPeriods("VIIRS.M")
#' 
#'     nlPeriods_2014 <- nlPeriods[grep("^2014", nlPeriods)]
#' 
#'     processNlData(nlPeriods=nlPeriods_2014)
#' 
#'     #Run the script from the command line as:
#'     
#'     #R CMD BATCH script_name_2014.R
#'     }
#' @export
processNlData <- function (ctryCodes,
                           admLevels,
                           nlTypes,
                           configNames,
                           multiTileStrategy = pkgOptions("multiTileStrategy"),
                           multiTileMergeFun = pkgOptions("multiTileMergeFun"),
                           removeGasFlares = pkgOptions("removeGasFlares"),
                           nlPeriods,
                           nlStats=pkgOptions("nlStats"),
                           custPolyPath=NULL,
                           gadmVersion=pkgOptions("gadmVersion"),
                           gadmPolyType=pkgOptions("gadmPolyType"),
                           downloadMethod=pkgOptions("downloadMethod"),
                           cropMaskMethod=pkgOptions("cropMaskMethod"),
                           extractMethod=pkgOptions("extractMethod"))
{
  if(missing(ctryCodes))
    ctryCodes <- getAllNlCtryCodes(omit = "error")
  
  if(missing(nlTypes))
    nlTypes <- getAllNlTypes()
  
  if(missing(configNames))
    configNames <- pkgOptions(paste0("configName_", nlTypes))
  
  #if the period is not given process all available periods
  if(missing("nlPeriods") || is.na(nlPeriods) || length(nlPeriods) == 0 || nlPeriods == "")
  {
    nlPeriods <- getAllNlPeriods(nlTypes = nlTypes)
  }
  
  if(!allValid(testData = ctryCodes, testFun = validCtryCodes))
    stop(Sys.time(), "Invalid ctryCode detected")
  
  if(!missing(admLevels) && is.list(admLevels))
  {
    if(length(ctryCodes) > 1 && length(ctryCodes) != length(admLevels))
      stop(Sys.time(), "admLevels do not match ctryCodes")
  } else
  {
    if(length(ctryCodes) > 1 && length(ctryCodes) != length(admLevels))
      stop(Sys.time(), "admLevels do not match ctryCodes")
  }
  
  funArgs <- match.call()[-1]

  message(Sys.time(), ": **** START PROCESSING: ", paste(names(funArgs), sapply(names(funArgs), function(x)eval(parse(text = x))), sep="=",collapse = ", "), "****")
  
  #uppercase vars
  ctryCodes <- toupper(x = ctryCodes)
  
  #Ensure we have all polygons before checking admLevels
  message(Sys.time(), ": Downloading country polygons ...")
  
  #download all country polygons if they don't already exist
  for (ctryCode in ctryCodes)
  {
    message(Sys.time(), ": Downloading polygon: ", ctryCode)
    dnldCtryPoly(ctryCode = ctryCode,
                 gadmVersion = gadmVersion,
                 gadmPolyType = gadmPolyType,
                 custPolyPath = custPolyPath,
                 downloadMethod = downloadMethod)
  }
  
  message(Sys.time(), ": Downloading country polygons ... DONE")
  
  if(missing(admLevels))
    admLevels <- "lowest"
  
  admLevels <- sapply(1:length(unlist(ctryCodes)), function(cCodeIdx)
  {
    ctryCode <- ctryCodes[[cCodeIdx]]
    
    ctryAdmLevels <- admLevels[[cCodeIdx]]
    
    unlist(sapply(ctryAdmLevels, function(admLevel)
    {
      #allow keywords/aliases instead of admLevels
      if(length(admLevel) == 1)
      {
        if(admLevel=="country")
          admLevel <- getCtryShpLyrNames(ctryCodes = ctryCode,
                                         lyrNums = 0,
                                         gadmVersion = gadmVersion,
                                         gadmPolyType = gadmPolyType,
                                         custPolyPath = custPolyPath)
        else if(admLevel %in% c("bottom", "lowest"))
          admLevel <- getCtryShpLowestLyrNames(ctryCodes = ctryCode,
                                               gadmVersion = gadmVersion,
                                               gadmPolyType = gadmPolyType,
                                               custPolyPath = custPolyPath)
        else if(admLevel %in% c("top","highest"))
          admLevel <- getCtryShpLyrNames(ctryCodes = ctryCode,
                                         lyrNums = 1,
                                         gadmVersion = gadmVersion,
                                         gadmPolyType = gadmPolyType,
                                         custPolyPath = custPolyPath)
        else if(admLevel=="all")
          admLevel <- getCtryShpAllAdmLvls(ctryCodes = ctryCode,
                                           gadmVersion = gadmVersion,
                                           gadmPolyType = gadmPolyType,
                                           custPolyPath = custPolyPath)
        else
        {
          tmpAdmLevel <- searchAdmLevel(ctryCodes = ctryCode,
                                        admLevelNames = admLevel,
                                        downloadMethod = downloadMethod,
                                        gadmVersion = gadmVersion,
                                        gadmPolyType = gadmPolyType,
                                        custPolyPath = custPolyPath)
          
          admLevel <- ifelse(is.na(tmpAdmLevel), admLevel, tmpAdmLevel)
        }
        
        #after processing admLevels if any are not in proper format e.g. KEN_adm0
        #check if they might have been supplied as e.g. adm0 or e.g. 0
        if(!length(grep(paste0(ctryCode,"_adm\\d+"), admLevel, ignore.case = T)) == length(admLevel))
        {
          if(length(grep("^adm\\d+$", admLevel, ignore.case = T)) > 0)
            admLevel <- paste(ctryCode, admLevel, sep="_")
          else if(length(grep("^\\d+$", admLevels, ignore.case = T)) > 0)
            admLevel <- paste(ctryCode, paste0("adm", admLevel), sep="_")
        }
      }
      
      admLevel
    }))
  })

  #if the admLevels are input as "adm0" convert to proper admLevel e.g. "KEN_adm0"
  for(i in 1:length(ctryCodes))
  if(!length(grep(paste0(ctryCodes[i],"_adm\\d+"), admLevels[i], ignore.case = T)) == length(admLevels[i]))
  {
    if(length(grep("^adm\\d+$", admLevels[i], ignore.case = T)) == length(admLevels[i]))
      admLevels[i] <- paste(ctryCodes[i], admLevels[i], sep="_")
    else if(length(grep("^\\d+$", admLevels[i], ignore.case = T)) == length(admLevels[i]))
      admLevels[i] <- paste(ctryCode, paste0("adm", admLevels[i]), sep="_")
  }
  
  # if(!allValidCtryAdmLvls(ctryCode = ctryCodes, admLevels = admLevels, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  #   stop(Sys.time(), "Invalid admLevels detected")
  
  #traverse and check all admLevel component lists/vectors
  if(!all(sapply(1:length(ctryCodes),
                function(i)
                {
                   if(is.list(admLevels))
                      allValidCtryAdmLvls(ctryCode = ctryCodes[i],
                                          admLevels = admLevels[[i]],
                                          gadmVersion = gadmVersion,
                                          gadmPolyType = gadmPolyType,
                                          custPolyPath = custPolyPath)
                   else
                   {
                     if(length(ctryCodes)==1)
                      allValidCtryAdmLvls(ctryCode = ctryCodes[i],
                                          admLevels = admLevels,
                                          gadmVersion = gadmVersion,
                                          gadmPolyType = gadmPolyType,
                                          custPolyPath = custPolyPath)
                     else
                       allValidCtryAdmLvls(ctryCode = ctryCodes[i],
                                           admLevels = admLevels[i],
                                           gadmVersion = gadmVersion,
                                           gadmPolyType = gadmPolyType,
                                           custPolyPath = custPolyPath)
                   }
                })))
    stop(Sys.time(), "Invalid admLevels detected")
  
  if(!allValidNlPeriods(nlTypes = nlTypes, nlPeriods = nlPeriods))
  {
    stop(Sys.time(), "Invalid nlPeriod(s) detected")
  }
  
  nlTiles <- NULL  

  ##First step: Determine which tiles are required for processing. This is determined by the
  #list of ctryCodes. Since theoretically we need the polygons of all countries to determine which
  #tiles to download we take the opportunity to download all required shapefiles
  #Practically, we can stop as soon as all nlTiles for an nlPeriod are flagged for download.
  
  ##If any tiles cannot be found/downloaded then abort and try for next country
  #we probably need to flag failed downloads so we don't try to process them and report back to user
  
  #extract nlPeriods ensuring they match with nlPeriods
  for(idxNlType in 1:length(nlTypes))
  {
    nlType <- nlTypes[idxNlType]
    
    configName <- configNames[idxNlType]

    if(length(nlTypes) == 1)
    {
      if(is.list(nlPeriods))
      {
        nlTypePeriods <- unlist(nlPeriods)
      }
      else
      {
        nlTypePeriods <- nlPeriods
      }
    }
    else
    {
      if(is.list(nlPeriods))
      {
        nlTypePeriods <- unlist(nlPeriods[idxNlType])
      }
      else
      {
        nlTypePeriods <- nlPeriods[[idxNlType]]
      }
    }

    #if the tile mapping does not exist create it
    if (!exists("nlTiles") || is.null(nlTiles))
      nlTiles <- getNlTiles(nlType = nlType)

    #for all nlPeriods check if the tiles exist else download
    for (nlPeriod in nlTypePeriods)
    {
      message(Sys.time(), ": **** PROCESSING nlType:", nlType, "configName: ", configName," nlPeriod:", nlPeriod, "****")
      message(Sys.time(), ": Checking tiles required for ", paste(nlType, nlPeriod))
      
      #init the list of tiles to be downloaded
      tileList <- NULL
      
      #determine tiles to download
      ctryTiles <- NULL
      
      tileList <- NULL
      
      #For each country
      for (idxCtryCode in 1:length(ctryCodes))
      {
        ctryCode <- ctryCodes[idxCtryCode]
        
        if(is.list(admLevels))
          ctryAdmLevels <- unlist(admLevels[idxCtryCode])
        else
          ctryAdmLevels <- admLevels[idxCtryCode]
        
        existAdmLvlStats <- unlist(lapply(ctryAdmLevels,
                                          function(admLevel)
                                            allExistsCtryNlData(ctryCodes = ctryCode,
                                                                admLevels = admLevel,
                                                                nlTypes = nlType,
                                                                configNames = configNames,
                                                                multiTileStrategy = multiTileStrategy,
                                                                multiTileMergeFun = multiTileMergeFun,
                                                                removeGasFlares = removeGasFlares,
                                                                nlPeriods = nlPeriod,
                                                                nlStats = nlStats,
                                                                gadmVersion = gadmVersion,
                                                                gadmPolyType = gadmPolyType,
                                                                custPolyPath = custPolyPath)))
        
        #Check if all stats exist for the ctryCode
        if (all(existAdmLvlStats))
        {
           message (Sys.time(), ": **", ctryCode, ": All stats exist")
         
           next
        }
        
        message(Sys.time(), ": ", ctryCode, ": Stats missing. Adding tiles")
        
        #get the list of tiles required for the ctryCode
        ctryTiles <- getCtryTileList(ctryCodes = ctryCode, nlType = nlType)
        
        #combine the list of unique tiles across all ctryCodes in tileList
        tileList <- c(tileList, setdiff(x = ctryTiles, y = tileList))
        
        #if all the unique tiles have been listed no need to proceed checking
        if (length(tileList) == nrow(nlTiles))
        {
          message (Sys.time(), ": All tiles have been listed. No need to check other country tiles")
          
          break
        }
      }
      
      message(Sys.time(), ": numTiles: ", length(tileList),", Required tiles: ", paste(tileList, collapse=","))
      
      if (length(tileList) == 0)
      {
        message(Sys.time(), ": No tiles needed for ", nlPeriod, ". Process next nlPeriod")
        
        next
      }
      else
      {
        rasterOutputFnamePath <- getCtryRasterOutputFnamePath(ctryCode = ctryCode,
                                                              nlType = nlType,
                                                              configName = configName,
                                                              multiTileStrategy = multiTileStrategy,
                                                              multiTileMergeFun = multiTileMergeFun,
                                                              removeGasFlares = removeGasFlares,
                                                              nlPeriod = nlPeriod,
                                                              gadmVersion = gadmVersion,
                                                              gadmPolyType = gadmPolyType,
                                                              custPolyPath = custPolyPath)
        
        #tile not found. if the cropped raster is not found try to download
        if (!file.exists(rasterOutputFnamePath))
        {
          if(!downloadNlTiles(nlType = nlType, configName = configName, multiTileStrategy = multiTileStrategy, nlPeriod = nlPeriod, tileList = tileList))
          {
            message(Sys.time(), ": Something went wrong with the tile downloads. Aborting ...")
            
            break
          }
        }
        else
        {
          message(Sys.time(), ": Cropped raster ", rasterOutputFnamePath, " already exists. Skipping tile download")
        }
      }
      
      #processNlCountry for all countries
      #Run through all countries since not guaranteed to check all countries in the
      #loop above checking existing stats. Premature exit allowed once all tiles listed
      for (idxCtryCode in 1:length(ctryCodes))
      {
        ctryCode <- ctryCodes[idxCtryCode]
        
        if(is.list(admLevels))
          ctryAdmLevels <- unlist(admLevels[idxCtryCode])
        else
          ctryAdmLevels <- admLevels[idxCtryCode]
        
        for(admLevel in ctryAdmLevels)
          processNLCountry(ctryCode = ctryCode,
                           admLevel = admLevel,
                           nlType = nlType,
                           configName = configName,
                           multiTileStrategy = multiTileStrategy,
                           multiTileMergeFun = multiTileMergeFun,
                           removeGasFlares = removeGasFlares,
                           nlPeriod = nlPeriod,
                           nlStats = nlStats,
                           downloadMethod = downloadMethod,
                           cropMaskMethod = cropMaskMethod,
                           extractMethod = extractMethod,
                           gadmVersion = gadmVersion,
                           gadmPolyType = gadmPolyType,
                           custPolyPath = custPolyPath)
      }
      
      #post-processing. Delete the downloaded tiles to release disk space
      if(pkgOptions("deleteTiles"))
      {
        for (tile in tileList)
        {
          tileTif <- getNlTileTifLclNamePath(nlType = nlType,
                                             nlPeriod = nlPeriod,
                                             tileNum = tileName2Idx(tile, nlType))
          
          message(Sys.time(), ": Deleting tile TIF: ", tileTif)
          
          #del the tif file
          if (file.exists(tileTif))
            unlink(file.path(tileTif), force = TRUE)
          
          tileZip <- file.path(getNlTileZipLclNamePath(nlType = nlType,
                                                       nlPeriod = nlPeriod,
                                                       tileNum = tileName2Idx(tileName = tile,
                                                                              nlType = nlType)))
          
          message(Sys.time(), ": Deleting tile ZIP: ", tileZip)
          
          #del the zip file
          if (file.exists(tileZip))
            unlink(x = file.path(tileZip), force = TRUE)
        }
      }
      
      message(Sys.time(), ": **** COMPLETED PROCESSING :", paste(names(funArgs), sapply(names(funArgs), function(x)eval(parse(text = x))), sep="=",collapse = ", "), "****")
    }
  }
}