######################## downloadNlTilesVIIRS ###################################

#' Download VIIRS nightlight tile
#'
#' Download VIIRS nightlight tile
#'
#' @param nlPeriod the nlPeriod of the tile to download
#'
#' @param tileNum the index of the tile as given by \code{getNlTiles}
#' 
#' @param downloadMethod The method to use for download.
#' 
#' @param nlType A character string of nlType
#' 
#' @param configName character the type of raster being processed
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(Rnightlights:::downloadNlTilesVIIRS("201401", "1"))
#'   print("download successful")
#'   }
#'
downloadNlTilesVIIRS <- function(nlPeriod,
                                 tileNum,
                                 downloadMethod=pkgOptions("downloadMethod"),
                                 nlType,
                                 configName=pkgOptions(paste0("configName_", nlType)))
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType")
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  if(!validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  rsltDnld <- NA
  
  #get the zip local names
  ntLtsZipLocalNamePathVIIRS <- getNlTileZipLclNamePath(nlType = nlType, nlPeriod = nlPeriod, tileNum = tileNum)
  
  #get the tif local names
  ntLtsTifLocalNamePathVIIRS <- getNlTileTifLclNamePath(nlType = nlType, nlPeriod = nlPeriod, tileNum = tileNum)

  #if the .tif doesn't exist download tgz tile. For aria and wget, if the tgz exists
  #it should attempt to complete it if incomplete else confirm it is complete and move
  #to extraction. For the other methods it will restart the download and overwrite
  if(!file.exists(ntLtsTifLocalNamePathVIIRS))
  {
    ntLtsFileUrl <- getNlUrlVIIRS(nlPeriod = nlPeriod, tileNum = tileNum, nlType = nlType)
    
    if(is.null(ntLtsFileUrl))
    {
      message(Sys.time(), ": ** Tile not available on the NOAA page.\n Please manually check for the ", nlPeriod, " tile at 'https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html'. If it exists please report this as a bug **")
      return(FALSE)
    }
    
    validDnldMethods <- c("auto", "curl", "libcurl", "wget", "aria")
    
    if(!(downloadMethod %in% validDnldMethods))
      downloadMethod <- "auto"
    
    if(downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
      rsltDnld <- utils::download.file(url = ntLtsFileUrl,
                                       destfile = ntLtsZipLocalNamePathVIIRS,
                                       mode = "wb",
                                       method = downloadMethod,
                                       extra = "-c")
    else if(downloadMethod == "aria")
      #downloads to path relative to -d if specified else local dir
      rsltDnld <- system(command = paste0("aria2c -c -x", pkgOptions("numParDnldConns"), " --show-console-readout=false --summary-interval=10 ",
                                          ntLtsFileUrl,
                                          " -d ", getNlDir("dirNlTiles"),
                                          " -o ", getNlTileZipLclNameVIIRS(nlType = nlType,
                                                                           configName = configName,
                                                                           nlPeriod = nlPeriod,
                                                                           tileNum = tileNum)))
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    message(Sys.time(), ": File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if(rsltDnld == 0)
  {
    message(Sys.time(), ": Extracting ", ntLtsZipLocalNamePathVIIRS)
    
    #for VIIRS.D 
    if(nlType == "VIIRS.D" && exists("ntLtsFileUrl"))
    {
      lenZipLclName <- nchar(x = ntLtsFileUrl)
      extZipLclName <- substr(x = ntLtsFileUrl, start = lenZipLclName - 2, stop = lenZipLclName)
      
      if(tolower(extZipLclName) != "tgz")
      {
        file.rename(from = ntLtsZipLocalNamePathVIIRS, to = ntLtsTifLocalNamePathVIIRS)
      }
    }
    else if(!file.exists(getNlTileTifLclNamePathVIIRS(nlPeriod = nlPeriod, tileNum = tileNum, nlType = nlType)))
    {
      message(Sys.time(), ": Getting list of files in ", ntLtsZipLocalNamePathVIIRS)
      
      tgzFileList <- utils::untar(tarfile = ntLtsZipLocalNamePathVIIRS, list = TRUE, tar = "internal")
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
      
      if(is.null(tgzFileList))
      {
        message(Sys.time(), ": Error extracting file list. ")
        
        return (FALSE)
      }
      
      #combined with section below to handle all VIIRS.* types
      # DELETE after confirmation
      # if(nlType == "VIIRS.Y")
      # {
      #   configShortName <- pkgOptions("configName_VIIRS.Y")
      #   
      #   tgzAvgRadFilename <- tgzFileList[grep(paste0("svdnb.*.", configShortName, ".*.avg_rade9.*.tif$"),tgzFileList, ignore.case = T)]
      # } else
      # {
      #   if(nlType == "VIIRS.D")
      #     configShortName <- pkgOptions("configName_VIIRS.D")
      #   else if(nlType == "VIIRS.M")
      #     configShortName <- pkgOptions("configName_VIIRS.M")
      #   
      #   tgzAvgRadFilename <- tgzFileList[grep(paste0("svdnb.*.", configShortName ,".*.avg_rade9.*.tif$"),tgzFileList, ignore.case = T)]
      # }
      
      configShortName <- pkgOptions(paste0("configName_", nlType))
      
      tgzAvgRadFilename <- tgzFileList[grep(pattern = paste0("svdnb.*.", configShortName ,".*.avg_rade9.*.tif$"),
                                            x = tgzFileList, ignore.case = T)]
      
      message(Sys.time(), ": Extracting ", tgzAvgRadFilename)
      
      if(!file.exists(getNlTileTifLclNamePathVIIRS(nlPeriod = nlPeriod, tileNum = tileNum, nlType = nlType)))
      {
        utils::untar(tarfile = ntLtsZipLocalNamePathVIIRS,
                     files = tgzAvgRadFilename,
                     exdir = getNlDir("dirNlTiles"),
                     tar = "internal")
        
        file.rename(from = file.path(getNlDir(dirName = "dirNlTiles"), tgzAvgRadFilename),
                    to = getNlTileTifLclNamePathVIIRS(nlPeriod = nlPeriod, tileNum = tileNum, nlType= nlType))
        
        #unlink(ntLtsZipLocalNamePathVIIRS, force = TRUE)
      }
    }
    else
    {
      message(Sys.time(), ": TIF file found")
    }
  }
  else
  {
    message(Sys.time(), ": An error occurred downloading")
    return(FALSE)
  }
  
  return (rsltDnld == 0)
}

######################## downloadNlTilesOLS ###################################

#' Download OLS nightlight tile
#'
#' Download OLS nightlight tile
#'
#' @param nlPeriod the nlPeriod of the tile
#'
#' @param downloadMethod The method to use for download.
#' 
#' @param nlType A character string of nlType
#' 
#' @param configName character the type of raster being processed
#' 
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(Rnightlights:::downloadNlTilesOLS("201405"))
#'   print("download successful")
#'   }
#'
downloadNlTilesOLS <- function(nlPeriod,
                               downloadMethod=pkgOptions("downloadMethod"),
                               nlType = "OLS.Y",
                               configName = pkgOptions(paste0("configName_", nlType)),
                               multiTileStrategy = pkgOptions("multiTileStrategy"))
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")

  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  rsltDnld <- 0
  
  nlUrlsOLS <- getNlUrlOLS(nlPeriod, configName)
  
  if(length(nlUrlsOLS) > 1)
  {
    message(Sys.time(), ": Multiple (", length(nlUrlsOLS), ") tiles found for ", nlType, ":", nlPeriod)
    
    if(multiTileStrategy == "first")
    {
      message(Sys.time(), ": MultiTile Strategy: Selecting tile: ", multiTileStrategy)
      nlUrlsOLS <- nlUrlsOLS[1]
    } else if(multiTileStrategy == "last")
    {
      message(Sys.time(), ": MultiTile Strategy: Selecting tile: ", multiTileStrategy)
      
      nlUrlsOLS <- nlUrlsOLS[length(nlUrlsOLS)]
    } else if(is.integer(multiTileStrategy))
    {
      message(Sys.time(), ": MultiTile Strategy: Selecting tile(s): ", multiTileStrategy)
      nlUrlsOLS <- nlUrlsOLS[multiTileStrategy]
    } else if(pkgOptions("multiTileStrategy") == "merge")
    {
      message(Sys.time(), ": MultiTile Strategy: Selecting all tiles: ", multiTileStrategy) 
    }
  }
   
  ntLtsZipLocalNameOLS <- getNlTileZipLclNameOLS(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
  ntLtsZipLocalNamePathOLS <- getNlTileZipLclNamePath(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
  ntLtsTifLocalNamePathOLS <- getNlTileTifLclNamePath(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
  
  if(!file.exists(ntLtsTifLocalNamePathOLS))
  {
    message(Sys.time(), ": Commencing download")
    
    for(i in 1:length(nlUrlsOLS))
    {
      rsltDnld <- NA
      
      #get the zip and tif local names
      ntLtsZipLocalNameOLSTemp <- getNlTileZipLclNameOLS(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
      ntLtsZipLocalNamePathOLSTemp <- getNlTileZipLclNamePath(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
      ntLtsTifLocalNamePathOLSTemp <- getNlTileTifLclNamePath(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
      
      ntLtsZipLocalNameOLSTemp <- gsub(pattern = "(\\.tar)", paste0("_", i, "\\1"), ntLtsZipLocalNameOLSTemp)
      ntLtsZipLocalNamePathOLSTemp <- gsub(pattern = "(\\.tar)", paste0("_", i, "\\1"), ntLtsZipLocalNamePathOLSTemp)
      ntLtsTifLocalNamePathOLSTemp <- gsub(pattern = "(\\.tif)", paste0("_", i, "\\1"), ntLtsTifLocalNamePathOLSTemp)
      
      if(!file.exists(ntLtsTifLocalNamePathOLSTemp))
      {
        if(!file.exists(ntLtsZipLocalNamePathOLSTemp))
        {
          #get the first only to cater for Where multiple tiles exist 
          ntLtsFileUrl <- nlUrlsOLS[i]
          
          ntLtsFileUrl <- gsub("\n", "", ntLtsFileUrl)
          
          validDnldMethods <- c(c("auto", "curl", "libcurl", "wget", "aria"))
          
          if(!(downloadMethod %in% validDnldMethods))
            downloadMethod <- "auto"
          
          message(Sys.time(), ": Downloading tile(", i, "/", length(nlUrlsOLS), "): ", ntLtsFileUrl)
          
          if(downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
            rsltDnld <- utils::download.file(ntLtsFileUrl, ntLtsZipLocalNamePathOLSTemp, mode = "wb", method = downloadMethod, extra = "-c")
          else if(downloadMethod == "aria")
            #downloads to path relative to -d if specified else local dir
            rsltDnld <- system(paste0("aria2c -c -x", pkgOptions("numParDnldConns"), " --show-console-readout=false --summary-interval=10 ",
                                      ntLtsFileUrl,
                                      " -d ",
                                      getNlDir("dirNlTiles"),
                                      " -o ", ntLtsZipLocalNameOLSTemp))
        } else
        {
          rsltDnld <- 0
        }
      }
      else
      {
        #if the file is found we can return positive? Probably not unless there's an overwrite option
        #for our purposes return true
        message(Sys.time(), "File exists, set Overwrite = TRUE to overwrite")
        
        rsltDnld <- 0
      }
    
      
      if(rsltDnld == 0)
      {
        message(Sys.time(), ": Extracting ", ntLtsZipLocalNamePathOLSTemp)
        
        tileNum <- "dummyTileNum"
        
        if(!file.exists(ntLtsTifLocalNamePathOLS))
        {
          message(Sys.time(), ": Getting list of files in ", ntLtsZipLocalNamePathOLSTemp)
          
          #get a list of files in the tar archive
          tarFileList <- utils::untar(ntLtsZipLocalNamePathOLSTemp, list = TRUE, tar="internal")
          
          #get the nightlight data filename
          #https://ngdc.noaa.gov/eog/gcv4_readme.txt
          #F1?YYYY.v4[b|c]_cf_cvg.tif: Cloud-free coverages tally 
          #F1?YYYY.v4[b|c]_avg_vis.tif: Raw avg_vis
          #F1?YYYY.v4[b|c]_stable_lights.avg_vis.tif: The cleaned up avg_vis 
          #F1?YYYY.v4[b|c]_stable_lights.lights_pct.tif
          #F1?YYYY.v4[b|c]_avg_lights_x_pct.tif
          
          tgzFile <- tarFileList[grep(paste0(".*\\.", configName, ".*\\.tif(\\.gz)*$"), tarFileList, ignore.case = T)]
          
          if(configName %in% toupper(c("cf_cvg", "avg_vis", "stable_lights")))
          {
            #extract the nightlight gz data file
            utils::untar(tarfile = ntLtsZipLocalNamePathOLSTemp, files = tgzFile, exdir = getNlDir("dirNlTiles"), tar = "internal")
          
            #the tif has the same name as the compressed file without the .gz
            tifFile <- stringr::str_replace(tgzFile, ".gz", "")
            
            #lights_pct and avg_lights_x_pct are not compressed
            
            message(Sys.time(), ": Decompressing ", tgzFile)
            
            if(!file.exists(ntLtsTifLocalNamePathOLSTemp))
              R.utils::gunzip(filename = file.path(getNlDir("dirNlTiles"), tgzFile), destname = ntLtsTifLocalNamePathOLSTemp, overwrite = TRUE)
          
            #unlink(ntLtsZipLocalNamePathOLS, force = TRUE)
          } else if(configName %in% toupper(c("pct_lights", "avg_lights_x_pct")))
          {
            message(Sys.time(), ": Decompressing ", tgzFile, " and renaming to ", ntLtsTifLocalNamePathOLSTemp)
            
            #the tifs are not compressed so extract directly and rename
            if(!file.exists(tgzFile))
              utils::untar(tarfile = ntLtsZipLocalNamePathOLSTemp, files = tgzFile, exdir = getNlDir("dirNlTiles"), tar = "internal")
            
            file.rename(file.path(getNlDir("dirNlTiles"), tgzFile), ntLtsTifLocalNamePathOLSTemp)
          }
        }
        else
        {
          message(Sys.time(), ": TIF file found")
        }
      }
    }
    
    if(length(nlUrlsOLS) == 1)
    {
      message(Sys.time(), ": Renaming single tile")
      
      file.rename(ntLtsTifLocalNamePathOLSTemp, ntLtsTifLocalNamePathOLS)
    } else
    {
      message(Sys.time(), ": Processing multiple tiles")
      
      wgs84 <- getCRS()
  
      ntLtsTifLocalNameOLS <- getNlTileTifLclNameOLS(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
  
      ntLtsTifLocalNamePathOLS <- getNlTileTifLclNamePath(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
      
      ntLtsTifList <- sapply(1:length(nlUrlsOLS), function(i){
        #get the zip and tif local names
        
        ntLtsTifLocalNamePathOLS <- gsub(pattern = "(\\.tif)", paste0("_", i, "\\1"), ntLtsTifLocalNamePathOLS)
      })
  
      message(Sys.time(), ": Merging Tifs")
      
      r <- raster::raster(x = ntLtsTifList[1])
      
      #get the extent and change to minx, miny, maxx, maxy order for use
      #in gdal_rasterize. Explanation below
      ext<-raster::extent(r)
      ext<-paste(ext[1], ext[3], ext[2], ext[4])
      
      #get the resolution of the raster. will be used in gdal_rasterize
      #for target resolution which should be the same as the source resolution.
      #Specifying makes it run faster (?)
      res<-paste(raster::res(r)[1], raster::res(r)[2])
  
      rm(r)
      
      outputFileVrt <-  gsub(".tif", ".vrt", ntLtsTifLocalNameOLS)
      
      outputFileVrt <- file.path(getNlDir("dirNlTemp"), outputFileVrt)
      
      if (file.exists(outputFileVrt))
        file.remove(outputFileVrt)
      
      message(Sys.time(), ": gdalwarp masking to VRT")
      
      gdalUtils::gdalbuildvrt(gdalfile = ntLtsTifList,
                          output.vrt = outputFileVrt,
                          te = as.character(ext),
                          tr = as.character(res),
                          tap = TRUE,
                          a_srs = wgs84,
                          multi = TRUE,
                          wm = pkgOptions("gdalCacheMax"),
                          wo = paste0("NUM_THREADS=", pkgOptions("numCores")))
      
      message(Sys.time(), ": gdal_translate converting VRT to TIFF ")
      gdalUtils::gdal_translate(#co = "LZW",
                                src_dataset = outputFileVrt,
                                dst_dataset = ntLtsTifLocalNamePathOLS)
      
      message(Sys.time(), ": Deleting the component rasters ")
      
      file.remove(ntLtsTifList)
      file.remove(outputFileVrt)
    }
  } else
  {
    message("Merged tile already exists")
  }
  
  return (rsltDnld == 0)
}
