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
#' @param oldNlType \code{character} The old nlType i.e. "OLS" or "VIIRS"
#'
#' @return \code{character} The new nlType i.e. "OLS.Y" or "VIIRS.M"
#'
#' @examples
#' Rnightlights:::newNlType("VIIRS")
#'  #returns "VIIRS.M"
#'
newNlType <- function(oldNlType)
{
  if(missing(oldNlType))
    stop("Missing required parameters oldNlType")
  
  if(length(oldNlType) > 1 || !oldNlType %in% c("OLS","VIIRS"))
    return(NA)
  
  #pre 0.2.0 all OLS is annual and all
  #VIIRS is monthly. Convert name accordingly
  if(oldNlType == "OLS")
    return("OLS.Y")
  
  if(oldNlType == "VIIRS")
    return("VIIRS.M")

  #if anything else return NA  
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
  tryCatch(
  {
    #will only make alterations after the current package version updates
    pkgVersion <- utils::packageDescription("Rnightlights")$Version
    
    upgradeLog <- data.frame("idx"=NULL, "operation"=NULL, "params"=NULL)
    
    if(is.null(pkgVersion) || pkgVersion == "")
      return(0)
    
    dataVersionFile <- file.path(paste0(Rnightlights::getNlDir("dirNlData"), "/../data-version.txt"))
    
    if(file.exists(dataVersionFile))
    {
      con <- file(dataVersionFile)
      
      dataVersion <- readLines(con = con,warn = F)
      
      close(con)
  
      #if the data version == pkg version
      #we are already using the latest data version. Exit
      if(dataVersion == pkgVersion)
      {
        return(FALSE)
      }
    }
    
    #ver 0.2.0 is the first version employing upgrade
    if(pkgVersion >= "0.2.0")
    {
      message("Upgrading data directory to ver. ", pkgVersion)
      
      idx <- 1
      
      #rename tiles using new format
      origWd <- setwd(Rnightlights::getNlDir("dirNlTiles"))
      
      message("Renaming tiles:")
      
      fileNames <- list.files(pattern = "^[[a-zA-Z]]{3,5}_[[:digit:]]{4,6}_[[:alnum:]]{7,8}\\.tif$")
      
      if(length(fileNames) > 0)
      {
        for(fileName in fileNames)
        {
          #split tile filename into components
          splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
          
          nlType <- splits[1]
          
          nlType <- newNlType(nlType)
          
          nlPeriod <- splits[2]
          
          tileName <- splits[3]
  
          newTileName <- getNlTileTifLclNamePath(nlType,
                                                 nlPeriod,
                                                 tileName2Idx(tileName, nlType)
                                                 )
          
          message("Rename: '", fileName, "' -> '", newTileName, "' : ", ifelse(file.rename(fileName, newTileName), "Success", "Fail"))
          
          idx <- idx + 0.1
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx, "file.rename", paste0(tileName, newTileName, sep="|")))
        }
      }else
      {
        message("No upgrade required")
      }
      
      #rename data files using new format
      idx <- round(idx + 1)
      
      message("Renaming data files:")
      setwd(Rnightlights::getNlDir("dirNlData"))
      fileNames <- list.files(pattern = "^[[:alpha:]]{3,5}_NLData\\.csv$")
      
      if(length(fileNames) > 0)
      {
        for(fileName in fileNames)
        {
          idx <- idx + 0.1
          #split filename into components
          
          splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
          
          ctryCode <- splits[1]
          
          admLevel <- unlist(getCtryShpLowestLyrNames(ctryCode))
         
          newFileName <- getCtryNlDataFname(ctryCode, admLevel)
          
          message("Rename: '", fileName, "' -> '", newFileName, "' : ", ifelse(file.rename(fileName, newFileName),"Success","Fail"))
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx, "file.rename", paste0(fileName, newFileName, sep="|")))
          
          message("Renaming columns:")
          
          idx <- idx + 0.01
          
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
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx, "cols.rename", paste(nlCols, newNlCols, sep="|")))
          
          utils::write.table(ctryNlData, newFileName, row.names = F, sep = ",")
        }
      }else
      {
        message("No upgrade required")
      }
      
      #rename rasters
      idx <- round(idx + 1)
      
      #rename tiles using new format
      message("Renaming country rasters:")
      setwd(Rnightlights::getNlDir("dirRasterOutput"))
      fileNames <- list.files(pattern = "^[a-zA-Z]{3}_[a-zA-Z]{3,5}_[0-9]{4,6}\\.tif$")
      
      if(length(fileNames) > 0)
      {
        for(fileName in fileNames)
        {
          #split tile filename into components
          splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
          
          ctryCode <- splits[1]
          
          nlType <- splits[2]
          
          nlType <- newNlType(nlType)
          
          nlPeriod <- splits[3]
          
          newFileName <- getCtryRasterOutputFname(ctryCode, nlType, nlPeriod)
          
          message("Rename:: '", fileName, "' -> '", newFileName, "' : ", ifelse(file.rename(fileName, newFileName), "Success", "Fail"))
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx, "file.rename", paste0(fileName, newFileName, sep="|")))
          
        }
      }else
      {
        message("No upgrade required")
      }
      
      #remove zonal rasters will be recreated at next run
      message("Remove Old Zonal Files:")
      setwd(getNlDir("dirZonals"))
      
      if(length(list.files()) > 0)
        message(ifelse(all(file.remove(list.files())), "Success", "Fail"))
      else
        message("No upgrade required")
      
      #log alterations for rollback
      
      setwd(origWd)
      
      #if we got here all went well
      message("Upgrade complete!")
      
      return(TRUE)
    }
  }, error=function(err)
  {
    message(err)
    message("An error occurred in upgrading the Rnightlights data dir. 
            Some of your old data may not be accessible from the upgraded package. 
            Please open an issue on the package github page if you encounter
            any issues. Continuing")
    
    return(FALSE)
  },finally = {
    #mark as upgraded
    cat(pkgVersion, file = file.path(paste0(Rnightlights::getNlDir("dirNlData"), "/../data-version.txt")))
    if(nrow(upgradeLog) > 0)
    {
      con = file(file.path(paste0(Rnightlights::getNlDir("dirNlData"), "/../upgrade-",pkgVersion,".log")))
      writeLines(upgradeLog, con)
      close(con)
    }
  }
  )
}