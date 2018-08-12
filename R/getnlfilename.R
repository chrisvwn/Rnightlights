######################## getNlTileZipLclNamePath ###################################

#' Constructs the full path used to save/access the compressed downloaded tile
#'
#' Constructs the full path used to save/access the compressed downloaded tile
#'     Calls the relevant function for the given nlType
#'
#' @param nlType the nlType of interest
#' 
#' @param nlPeriod the nlPeriod in which the tile was created
#'
#' @param tileNum the index of the tile as given in nlTileIndex
#'
#' @return a character string filename of the compressed .tgz VIIRS tile
#'
#' @examples
#' \dontrun{
#' Rnightlights:::getNlTileZipLclNamePath("VIIRS.M", "201401", 1)
#'  #returns "/dataPath/VIIRS_2014_01_75N180W.tgz"
#'  }
#'  
#' \dontrun{
#' Rnightlights:::getNlTileZipLclNamePath("OLS.Y", "2004", 1)
#'  #returns "/dataPath/OLS.Y_2004_00N180W.tar"
#'  }
#'
getNlTileZipLclNamePath <- function(nlType, nlPeriod, tileNum)
{
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(stringr::str_detect(nlType, "VIIRS") && missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType: ", nlType)
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if(stringr::str_detect(nlType, "VIIRS") && !validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  if(stringr::str_detect(nlType, "OLS"))
    return (file.path(getNlDir("dirNlTiles"), getNlTileZipLclNameOLS(nlPeriod)))
  else if(stringr::str_detect(nlType, "VIIRS"))
    return (file.path(getNlDir("dirNlTiles"), getNlTileZipLclNameVIIRS(nlPeriod, tileNum, nlType)))
}

######################## getNlTileZipLclNameVIIRS ###################################

#' Constructs the filename used to save/access the downloaded VIIRS tile .tgz file
#'
#' Constructs the filename used to save/access the downloaded VIIRS tile .tgz file
#'
#' @param nlPeriod The nlPeriod in which the tile was created
#'
#' @param tileNum The index of the tile as given in nlTileIndex
#' 
#' @param nlType character the nlType
#'
#' @return A character string filename of the compressed .tgz VIIRS tile
#'
#' @examples
#' \dontrun{
#' Rnightlights:::getNlTileZipLclNameVIIRS("201401", 1)
#'  #returns "./tiles/VIIRS_2014_01_75N180W.tgz"
#'  }
#'
getNlTileZipLclNameVIIRS <- function(nlPeriod, tileNum, nlType)
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType: ", nlType)

  if(!validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  if(!allValidNlPeriods(nlPeriod, nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  return (paste0("NL_TILE_", nlType, "_", nlPeriod, "_", tileIdx2Name(tileNum, nlType), ".tgz"))
}

######################## getNlTileTifLclNamePath ###################################

#' Constructs the full path used to save/access the downloaded tile .tgz file
#'
#' Constructs the full path used to save/access the downloaded tile .tgz file
#'
#' @param nlType the nlType of interest
#'
#' @param nlPeriod the nlPeriod in which the tile was created
#'
#' @param tileNum the index of the tile as given in nlTileIndex
#'
#' @return a character string filename of the compressed .tgz VIIRS tile
#'
#' @examples
#' \dontrun{
#' Rnightlights:::getNlTileZipLclNamePath("OLS.Y", "2012", 1)
#'  #returns "/dataPath/OLS.Y_2012_00N180W.tgz"
#'  }
#'
#' \dontrun{
#' Rnightlights:::getNlTileZipLclNamePath("VIIRS.M", "201412", 1)
#'  #returns "/dataPath/VIIRS.M_201412_75N180W.tgz"
#'  }
#'
getNlTileTifLclNamePath <- function(nlType, nlPeriod, tileNum)
{
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(stringr::str_detect(nlType, "VIIRS") && missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType: ", nlType)
  
  if(!allValidNlPeriods(nlPeriod, nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if(stringr::str_detect(nlType, "VIIRS") && !validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  if(stringr::str_detect(nlType, "OLS"))
    return (file.path(getNlDir("dirNlTiles"), getNlTileTifLclNameOLS(nlPeriod)))
  else if(stringr::str_detect(nlType, "VIIRS"))
    return (file.path(getNlDir("dirNlTiles"), getNlTileTifLclNameVIIRS(nlPeriod, tileNum, nlType)))
}

######################## getNlTileTifLclNameVIIRS ###################################

#' Constructs the filename of the decompressed VIIRS .tif file
#'
#' Constructs the filename of the decompressed VIIRS .tif file
#'
#' @param nlPeriod the nlPeriod in which the tile was created
#'
#' @param tileNum the index of the tile as given in nlTileIndex
#' 
#' @param nlType character string the nlType
#'
#' @return a character vector filename of the .tif VIIRS tile
#'
#' @examples
#' #using default dirNlTiles
#' \dontrun{
#' Rnightlights:::getNlTileTifLclNameVIIRS("201401", 1, "VIIRS.M")
#'  #returns "VIIRS_201401_75N180W.tif"
#'  }
#'
getNlTileTifLclNameVIIRS <- function(nlPeriod, tileNum, nlType)
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")

  if(!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType: ", nlType)
    
  if(!allValidNlPeriods(nlPeriod, nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  if(!validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)

  return (paste0("NL_TILE_", nlType, "_", nlPeriod, "_", tileIdx2Name(tileNum, nlType), ".tif"))
}

######################## getNlTileTifLclNameOLS ###################################

#' Constructs the filename of the decompressed OLS .tif file
#'
#' Constructs the filename of the decompressed OLS .tif file
#'
#' @param nlPeriod the nlPeriod in which the tile was created
#'
#' @return a character vector filename of the .tif OLS tile
#'
#' @examples
#' #using default dirNlTiles
#' \dontrun{
#' Rnightlights:::getNlTileTifLclNameOLS("2004")
#'  #returns "OLS_2004_00N180W.tif"
#'  }
#'
getNlTileTifLclNameOLS <- function(nlPeriod)
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(!allValidNlPeriods(nlPeriod, "OLS.Y"))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  return (paste0("NL_TILE_OLS.Y_", nlPeriod, "_00N180W.tif"))
}

######################## getNlTileTifLclNamePathVIIRS ###################################

#' Constructs the full path used to save/access the decompressed VIIRS .tif file
#'
#' Constructs the full path used to save/access the decompressed VIIRS .tif file
#'
#' @param nlPeriod the yearMonth in which the tile was created
#'
#' @param tileNum the index of the tile as given in nlTileIndex
#' 
#' @param nlType The particular VIIRS type e.g. VIIRS_D for daily VIIRS
#'
#' @return a character vector filename of the .tif VIIRS tile
#'
#' @examples
#' #using default dirNlTiles
#' \dontrun{
#' Rnightlights:::getNlTileTifLclNamePathVIIRS("201401", 1)
#'  #returns "/dataPath/tiles/VIIRS_2014_01_75N180W.tif"
#'  }
#'
getNlTileTifLclNamePathVIIRS <- function(nlPeriod, tileNum, nlType)
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(!allValidNlPeriods(nlPeriod, nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  if(!validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  return (file.path(getNlDir("dirNlTiles"), getNlTileTifLclNameVIIRS(nlPeriod, tileNum, nlType)))
}

######################## getNlTileTifLclNamePathOLS ###################################

#' Constructs the full path used to save/access the decompressed OLS .tif file
#'
#' Constructs the full path used to save/access the decompressed OLS .tif file
#'
#' @param nlPeriod the year in which the tile was created
#' 
#' @param tileNum ignored
#'
#' @return a character vector filename of the .tif OLS tile
#'
#' @examples
#' #using default dirNlTiles
#' \dontrun{
#' Rnightlights:::getNlTileTifLclNamePathOLS("2012", 1)
#'  #returns "/dataPath/tiles/OLS_2012.tif"
#'  }
#'
getNlTileTifLclNamePathOLS <- function(nlPeriod, tileNum)
{
  nlType <- "OLS"
  
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(!allValidNlPeriods(nlPeriod, "OLS.Y"))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  return (file.path(getNlDir(dirName = "dirNlTiles"), getNlTileTifLclNameOLS(nlPeriod = nlPeriod)))
}

######################## getNlTileZipLclNameOLS ###################################

#' The name with which to save the OLS tile locally
#'
#' The name with which to save the OLS tile locally
#'
#' @param nlPeriod The year of the OLS tile
#'
#' @return character string filename
#'
#' @examples
#' \dontrun{
#' Rnightlights:::getNlTileZipLclNameOLS("2012")
#' #returns "OLS.Y_2012_00N180W.tar"
#' }
#'
getNlTileZipLclNameOLS <- function(nlPeriod)
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(!allValidNlPeriods(nlPeriod, "OLS.Y"))
    stop(Sys.time(), ": Invalid nlPeriod")
  
  return (paste0("NL_TILE_OLS.Y_", nlPeriod, "_00N180W.tar"))
}

######################## getNlTifLclNameOLS ###################################

#' Constructs the filename used to save/access the decompressed OLS .tif file
#'
#' Constructs the filename used to save/access the decompressed OLS .tif file
#'
#' @param nlPeriod the nlPeriod in which the tile was created
#'
#' @return a character vector filename of the .tif VIIRS tile
#'
#' @examples
#' #using default dirNlTiles
#' \dontrun{
#' Rnightlights:::getNlTifLclNameOLS("2004")
#'  #returns "OLS_2004.tif"
#'  }
#'
getNlTifLclNameOLS <- function(nlPeriod)
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(!allValidNlPeriods(nlPeriod, "OLS.Y"))
    stop(Sys.time(), ": Invalid nlPeriod")
  
  return (paste0("NL_TILE_OLS_", nlPeriod, ".tif"))
}