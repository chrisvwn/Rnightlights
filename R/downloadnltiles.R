######################## downloadNlTilesVIIRS ###################################

#' Download VIIRS nightlight tile
#'
#' Download VIIRS nightlight tile
#'
#' @param nlYearMonth the year in "YYYYMM" format e.g. "201204"
#'
#' @param tileNum the index of the tile as given by getNlTiles("VIIRS")
#' 
#' @param downloadMethod The method to use for download.
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(downloadNlTilesVIIRS("201205", "1"))
#'   print("download successful")
#'   }
#'
downloadNlTilesVIIRS <- function(nlYearMonth, tileNum, downloadMethod=pkgOptions("downloadMethod"))
{
  nlType <- "VIIRS"
  
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYear")
  
  if(missing(tileNum))
    stop("Missing required parameter tileNum")
  
  if(!validNlPeriodVIIRS(nlYearMonth))
    stop("Invalid nlYear: ", nlYear)
  
  if(!validNlTileNumVIIRS(tileNum))
    stop("Invalid tileNum: ", tileNum)
  
  rsltDnld <- NA
  
  #get the zip and tif local names
  ntLtsZipLocalNamePathVIIRS <- getNlTileZipLclNamePath("VIIRS", nlYearMonth, tileNum)
  ntLtsTifLocalNamePathVIIRS <- getNlTileTifLclNamePath("VIIRS", nlYearMonth, tileNum)

  #if the .tif doesn't exist download tgz tile. For aria and wget, if the tgz exists
  #it should attempt to complete it if incomplete else confirm it is complete and move
  #to extraction. For the other methods it will restart the download and overwrite
  if (!file.exists(ntLtsTifLocalNamePathVIIRS))
  {
    ntLtsFileUrl <- getNlUrlVIIRS(nlYearMonth, tileNum)
    
    validDnldMethods <- c(c("auto", "curl", "libcurl", "wget", "aria"))
    
    if (!(downloadMethod %in% validDnldMethods))
      downloadMethod <- "auto"
    
    if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
      rsltDnld <- utils::download.file(ntLtsFileUrl, ntLtsZipLocalNamePathVIIRS, mode = "wb", method = downloadMethod, extra = "-c")
    else if (downloadMethod == "aria")
      rsltDnld <- system(paste0("aria2c -c -x2 ", ntLtsFileUrl, " -d ", getNlDir("dirRasterVIIRS"), " -o ", getNlTileZipLclNameVIIRS(nlYearMonth, tileNum))) #downloads to path relative to -d if specified else local dir
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    message("File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if (rsltDnld == 0)
  {
    message("Extracting ", ntLtsZipLocalNamePathVIIRS, " ", base::date())
    
    if (!file.exists(getNlTileTifLclNamePathVIIRS(nlYearMonth, tileNum)))
    {
      message("Getting list of files in ", ntLtsZipLocalNamePathVIIRS, " ", base::date())
      
      tgzFileList <- utils::untar(ntLtsZipLocalNamePathVIIRS, list = TRUE, tar = "internal")
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
      
      if (is.null(tgzFileList))
      {
        message("Error extracting file list. ")
        
        return (-1)
      }
      
      tgzAvgRadFilename <- tgzFileList[grep("svdnb.*.avg_rade9.tif$",tgzFileList, ignore.case = T)]
      
      message("Decompressing ", tgzAvgRadFilename, " ", base::date())
      
      if(!file.exists(getNlTileTifLclNamePathVIIRS(nlYearMonth, tileNum)))
      {
        utils::untar(ntLtsZipLocalNamePathVIIRS, files = tgzAvgRadFilename, exdir = getNlDir("dirRasterVIIRS"), tar="internal")
        
        file.rename(file.path(getNlDir("dirRasterVIIRS"), tgzAvgRadFilename), getNlTileTifLclNamePathVIIRS(nlYearMonth, tileNum))
        
        file.remove(ntLtsZipLocalNamePathVIIRS)
      }
    }
    else
    {
      message("TIF file found")
    }
  }
  
  return (rsltDnld == 0)
}

######################## downloadNlTilesOLS ###################################

#' Download OLS nightlight tile
#'
#' Download OLS nightlight tile
#'
#' @param nlYear the year in "YYYY" format e.g. "2012"
#'
#' @param downloadMethod The method to use for download.
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(downloadNlTilesOLS("201205"))
#'   print("download successful")
#'   }
#'
downloadNlTilesOLS <- function(nlYear, downloadMethod=pkgOptions("downloadMethod"))
{
  nlType <- "OLS"
  
  if(missing(nlYear))
    stop("Missing required parameter nlYear")
  
  if(!validNlPeriodOLS(nlYear))
    stop("Invalid nlYear: ", nlYear)
  
  rsltDnld <- NA
  
  #get the zip and tif local names
  ntLtsZipLocalNamePath <- getNlTileZipLclNamePath("OLS", nlYear)
  ntLtsTifLocalNamePath <- getNlTileTifLclNamePath("OLS", nlYear)
  
  #if (!file.exists(ntLtsZipLocalNameVIIRS) && !file.exists(ntLtsTifLocalNameVIIRS))
  if (!file.exists(ntLtsTifLocalNamePath))
  {
    ntLtsFileUrl <- getNlUrlOLS(nlYear)
    
    ntLtsFileUrl <- gsub("\n", "", ntLtsFileUrl)
    
    validDnldMethods <- c(c("auto", "curl", "libcurl", "wget", "aria"))
    
    if (!(downloadMethod %in% validDnldMethods))
      downloadMethod <- "auto"
    
    if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
      rsltDnld <- utils::download.file(ntLtsFileUrl, ntLtsZipLocalNamePath, mode = "wb", method = downloadMethod, extra = "-c")
    else if (downloadMethod == "aria")
      rsltDnld <- system(paste0("aria2c -c -x2 ", ntLtsFileUrl, " -d ", getNlDir("dirRasterOLS"), " -o ", getNlTileZipLclNameOLS(nlYear))) #downloads to path relative to -d if specified else local dir
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    message("File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if (rsltDnld == 0)
  {
    message("Extracting ", ntLtsZipLocalNamePath, " ", base::date())
    
    if (!file.exists(getNlTileTifLclNamePathOLS(nlYear, tileNum)))
    {
      message("Getting list of files in ", ntLtsZipLocalNamePath, " ", base::date())
      
      #get a list of files in the tar archive
      tarFileList <- utils::untar(ntLtsZipLocalNamePath, list = TRUE, tar="internal")
      
      #get the nightlight data filename
      #the nightlight data filename has the format "web.avg_vis.tif.gz"
      #    tgz_file <- tar_file_list[grep(".*web\\.avg_vis\\.tif\\.gz$",tar_file_list, ignore.case = T)]
      tgzFile <- tarFileList[grep(".*stable_lights\\.avg_vis\\.tif\\.gz$", tarFileList, ignore.case = T)]
      
      #extract the nightlight data file
      utils::untar(tarfile = ntLtsZipLocalNamePath, files = tgzFile, exdir = getNlDir("dirRasterOLS"), tar = "internal")
      
      #the tif has the same name as the compressed file without the .gz
      tifFile <- stringr::str_replace(tgzFile, ".gz", "")
      
      message("Decompressing ", tgzFile, " ", date())
      
      R.utils::gunzip(file.path(getNlDir("dirRasterOLS"), tgzFile), ntLtsTifLocalNamePath)
      
    }
    else
    {
      message("TIF file found")
    }
  }
  
  return (rsltDnld == 0)
}
