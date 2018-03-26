######################## newNlType ###################################

#' Convert pre-0.2.0 nlType names to their new names
#'
#' Convert pre-0.2.0 nlType names to their new names. Pre 0.2.0
#'     has only 2 nlTypes i.e. OLS and VIIRS. They are renamed
#'     as follows:
#'     \itemize{
#'         \item{OLS} { => "OLS.Y"}
#'         \item(VIIRS) { => "VIIRS.M"}
#'     }
#'
#' @param oldNlType The old nlType i.e. "OLS" or "VIIRS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::newNlType("VIIRS")
#'  #returns "VIIRS.M"
#'
newNlType <- function(oldNlType)
{
  if(!oldNlType %in% c("OLS","VIIRS"))
    stop("Invalid old name ", oldNlType)
  
  #pre 0.2.0 all OLS is annual and all
  #VIIRS is monthly. Convert name accordingly
  if(oldNlType == "OLS")
    return("OLS.Y")
  
  if(oldNlType == "VIIRS")
    return("VIIRS.M")
  
  message("Invalid old name ", oldNlType)
  return(NA)
}

######################## upgradeRnightlights ###################################

#' Perform upgrade functions to new package versions as required
#'
#' Perform upgrade functions to new package versions as required
#'     General Upgrade functions
#'     pre-0.2.0 to 0.2.0:
#'     \itemize{
#'         \item{Rename tiles}
#'         \item{Rename output rasters}
#'         \item{Rename data files}
#'         \item{Rename data column names}
#'         \item{Remove zonal rasters which will be regenerated when required}
#'     }
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{
#'   Rnightlights:::upgradeRnightlights()
#'   #returns TRUE/FALSE
#' }
#' @export
upgradeRnightlights <- function()
{
  #will only make alterations after the current package version updates
  pkgs <- as.data.frame(utils::installed.packages(), stringsAsFactors=F)
  
  pkg <- pkgs[pkgs$Package == "Rnightlights",c("Package", "Version")]
  
  if(nrow(pkg) == 0)
    return()
  
  if(pkg$Version >= "0.2.0")
  {
    #rename tiles using new format
    setwd(Rnightlights::getNlDir("dirNlTiles"))
    message("Renaming tiles")
    fileNames <- list.files(pattern = "^[[:alpha:]]{3,5}_[[:digit:]]{4,6}_[[:alnum:]]{7,8}\\.tif$")
    
    if(length(fileNames) > 0)
      for(fileName in fileNames)
      {
        #split tile filename into components
        splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
        
        nlType <- splits[1]
        
        nlType <- newNlType(nlType)
        
        nlPeriod <- splits[2]
        
        tileName <- splits[3]

        newTileName <- getNlTileTifLclNameVIIRS(nlPeriod,
                                                tileName2Idx(tileName,
                                                             nlType),
                                                nlType)
        
        message("Rename: '", tileName, "' -> '", newTileName, "' : ")
        if(file.rename(tileName, newTileName))
          cat("Success")
        else
          cat("Fail")
      }
    
    #rename data files using new format
    
    message("Renaming data files")
    setwd(Rnightlights::getNlDir("dirNlData"))
    fileNames <- list.files(pattern = "^[[:alpha:]]{3,5}_NLData\\.csv$")
    
    if(length(fileNames) > 0)
      for(fileName in fileNames)
      {
        #split filename into components
        
        splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
        
        ctryCode <- splits[1]
        
        admLevel <- unlist(getCtryShpLowestLyrNames(ctryCode))
       
        newFileName <- getCtryNlDataFname(ctryCode, admLevel)
        
        message("Rename: '", fileName, "' -> '", newFileName, "' : ", ifelse(file.rename(fileName, newFileName),"Success","Fail"))
        
        message("Renaming columns")
        
        ctryNlData <- utils::read.csv(newFileName, header = T)
        
        cols <- names(ctryNlData)
        
        ctryCols <- grep("NL_", cols, invert = T, value = T)
        nlCols <- grep("NL_", cols, value = T)
        
        newNlCols <- lapply(nlCols, function(x){
          colSplits <- unlist(strsplit(x, "_"))
          
          nlType <- colSplits[2]
          
          nlType <- newNlType(nlType)
          
          nlPeriod <- colSplits[3]
          
          nlStat <- tolower(colSplits[4])
          
          newColName <- getCtryNlDataColName(nlPeriod, nlStat, nlType)
        })
      
        names(ctryNlData) <- c(ctryCols, newNlCols)
        
        utils::write.table(ctryNlData, newFileName, row.names = F, sep = ",")
      }
    
    #rename rasters
    #rename tiles using new format
    message("Renaming country rasters")
    setwd(Rnightlights::getNlDir("dirRasterOutput"))
    fileNames <- list.files(pattern = "^[[:alpha:]]{3}_[[:digit:]]{3,5}_[[:alnum:]]{4,6}\\.tif$")
    
    if(length(fileNames) > 0)
      for(fileName in fileNames)
      {
        #split tile filename into components
        splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
        
        ctryCode <- splits[1]
        
        nlType <- splits[2]
        
        nlType <- newNlType(nlType)
        
        nlPeriod <- splits[3]
        
        newFileName <- getCtryRasterOutputFname(ctryCode, nlType, nlPeriod)
        
        message("Rename: '", fileName, "' -> '", newTileName, "' : ", ifelse(file.rename(fileName, newFileName), "Success", "Fail"))
      }
    
    #remove zonal rasters will be recreated at next run
    setwd(getNlDir("dirZonals"))
    if(all(file.remove(list.files())))
      message("Success")
    
    #log alterations for rollback
    
    #mark as upgraded
    
    #if we got here all went well
    return(TRUE)
  }
}