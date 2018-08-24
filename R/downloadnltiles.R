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
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(Rnightlights:::downloadNlTilesVIIRS("201401", "1"))
#'   print("download successful")
#'   }
#'
downloadNlTilesVIIRS <- function(nlPeriod, tileNum, downloadMethod=pkgOptions("downloadMethod"), nlType)
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
  
  #get the zip and tif local names
  ntLtsZipLocalNamePathVIIRS <- getNlTileZipLclNamePath(nlType, nlPeriod, tileNum)
  ntLtsTifLocalNamePathVIIRS <- getNlTileTifLclNamePath(nlType, nlPeriod, tileNum)

  #if the .tif doesn't exist download tgz tile. For aria and wget, if the tgz exists
  #it should attempt to complete it if incomplete else confirm it is complete and move
  #to extraction. For the other methods it will restart the download and overwrite
  if (!file.exists(ntLtsTifLocalNamePathVIIRS))
  {
    ntLtsFileUrl <- getNlUrlVIIRS(nlPeriod, tileNum, nlType)
    
    if(is.null(ntLtsFileUrl))
    {
      message(Sys.time(), ": ** Tile not available on the NOAA page.\n Please manually check for the ", nlPeriod, " tile at 'https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html'. If it exists please report this as a bug **")
      return(FALSE)
    }
    
    validDnldMethods <- c(c("auto", "curl", "libcurl", "wget", "aria"), nlType)
    
    if (!(downloadMethod %in% validDnldMethods))
      downloadMethod <- "auto"
    
    if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
      rsltDnld <- utils::download.file(ntLtsFileUrl, ntLtsZipLocalNamePathVIIRS, mode = "wb", method = downloadMethod, extra = "-c")
    else if (downloadMethod == "aria")
      rsltDnld <- system(paste0("aria2c -c -x2 --show-console-readout=false --summary-interval=10 ", ntLtsFileUrl, " -d ", getNlDir("dirNlTiles"), " -o ", getNlTileZipLclNameVIIRS(nlPeriod, tileNum, nlType))) #downloads to path relative to -d if specified else local dir
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    message(Sys.time(), ": File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if (rsltDnld == 0)
  {
    message(Sys.time(), ": Extracting ", ntLtsZipLocalNamePathVIIRS)
    
    #for VIIRS.D 
    if(nlType == "VIIRS.D" && exists("ntLtsFileUrl"))
    {
      lenZipLclName <- nchar(ntLtsFileUrl)
      extZipLclName <- substr(ntLtsFileUrl, lenZipLclName - 2, lenZipLclName)
      
      if(tolower(extZipLclName) != "tgz")
      {
        file.rename(ntLtsZipLocalNamePathVIIRS, ntLtsTifLocalNamePathVIIRS)
      }
    }
    else
      if (!file.exists(getNlTileTifLclNamePathVIIRS(nlPeriod, tileNum, nlType)))
    {
      message(Sys.time(), ": Getting list of files in ", ntLtsZipLocalNamePathVIIRS)
      
      tgzFileList <- utils::untar(ntLtsZipLocalNamePathVIIRS, list = TRUE, tar = "internal")
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
      
      if (is.null(tgzFileList))
      {
        message(Sys.time(), ": Error extracting file list. ")
        
        return (-1)
      }
      
      if(nlType == "VIIRS.Y")
      {
        configShortName <- pkgOptions("configName_VIIRS.Y")
        
        tgzAvgRadFilename <- tgzFileList[grep(paste0("svdnb.*.", configShortName, ".*.avg_rade9.*.tif$"),tgzFileList, ignore.case = T)]
      } else
      {
        if(nlType == "VIIRS.D")
          configShortName <- pkgOptions("configName_VIIRS.D")
        else if(nlType == "VIIRS.M")
          configShortName <- pkgOptions("configName_VIIRS.M")
        
        tgzAvgRadFilename <- tgzFileList[grep(paste0("svdnb.*.", configShortName ,".*.avg_rade9.*.tif$"),tgzFileList, ignore.case = T)]
      }
      
      message(Sys.time(), ": Decompressing ", tgzAvgRadFilename)
      
      if(!file.exists(getNlTileTifLclNamePathVIIRS(nlPeriod, tileNum, nlType)))
      {
        utils::untar(ntLtsZipLocalNamePathVIIRS, files = tgzAvgRadFilename, exdir = getNlDir("dirNlTiles"), tar="internal")
        
        file.rename(file.path(getNlDir("dirNlTiles"), tgzAvgRadFilename), getNlTileTifLclNamePathVIIRS(nlPeriod, tileNum, nlType))
        
        unlink(ntLtsZipLocalNamePathVIIRS, force = TRUE)
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
    return(-1)
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
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(Rnightlights:::downloadNlTilesOLS("201405"))
#'   print("download successful")
#'   }
#'
downloadNlTilesOLS <- function(nlPeriod, downloadMethod=pkgOptions("downloadMethod"))
{
  nlType <- "OLS.Y"
  
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  rsltDnld <- NA
  
  #get the zip and tif local names
  ntLtsZipLocalNamePathOLS <- getNlTileZipLclNamePath(nlType, nlPeriod)
  ntLtsTifLocalNamePathOLS <- getNlTileTifLclNamePath(nlType, nlPeriod)
  
  #if (!file.exists(ntLtsZipLocalNameVIIRS) && !file.exists(ntLtsTifLocalNameVIIRS))
  if (!file.exists(ntLtsTifLocalNamePathOLS))
  {
    #get the first only to cater for Where multiple tiles exist 
    ntLtsFileUrl <- getNlUrlOLS(nlPeriod)[1]
    
    ntLtsFileUrl <- gsub("\n", "", ntLtsFileUrl)
    
    validDnldMethods <- c(c("auto", "curl", "libcurl", "wget", "aria"))
    
    if (!(downloadMethod %in% validDnldMethods))
      downloadMethod <- "auto"
    
    message(Sys.time(), ": Downloading ", ntLtsFileUrl)
    
    if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
      rsltDnld <- utils::download.file(ntLtsFileUrl, ntLtsZipLocalNamePathOLS, mode = "wb", method = downloadMethod, extra = "-c")
    else if (downloadMethod == "aria")
      rsltDnld <- system(paste0("aria2c -c -x2 --show-console-readout=false --summary-interval=10 ", ntLtsFileUrl, " -d ", getNlDir("dirNlTiles"), " -o ", getNlTileZipLclNameOLS(nlPeriod))) #downloads to path relative to -d if specified else local dir
    
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    message(Sys.time(), "File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if (rsltDnld == 0)
  {
    message(Sys.time(), "Extracting ", ntLtsZipLocalNamePathOLS)
    
    tileNum <- "dummyTileNum"
    
    if (!file.exists(getNlTileTifLclNamePathOLS(nlPeriod, tileNum)))
    {
      message(Sys.time(), ": Getting list of files in ", ntLtsZipLocalNamePathOLS)
      
      #get a list of files in the tar archive
      tarFileList <- utils::untar(ntLtsZipLocalNamePathOLS, list = TRUE, tar="internal")
      
      #get the nightlight data filename
      #the nightlight data filename has the format "web.avg_vis.tif.gz"
      #    tgz_file <- tar_file_list[grep(".*web\\.avg_vis\\.tif\\.gz$",tar_file_list, ignore.case = T)]
      tgzFile <- tarFileList[grep(".*stable_lights\\.avg_vis\\.tif\\.gz$", tarFileList, ignore.case = T)]
      
      #extract the nightlight data file
      utils::untar(tarfile = ntLtsZipLocalNamePathOLS, files = tgzFile, exdir = getNlDir("dirNlTiles"), tar = "internal")
      
      #the tif has the same name as the compressed file without the .gz
      tifFile <- stringr::str_replace(tgzFile, ".gz", "")
      
      message(Sys.time(), ": Decompressing ", tgzFile, " ", date())
      
      R.utils::gunzip(file.path(getNlDir("dirNlTiles"), tgzFile), ntLtsTifLocalNamePathOLS)
      
      unlink(ntLtsZipLocalNamePathOLS, force = TRUE)
    }
    else
    {
      message(Sys.time(), ": TIF file found")
    }
  }
  
  return (rsltDnld == 0)
}
