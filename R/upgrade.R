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
    stop(Sys.time(), ": Missing required parameters oldNlType")
  
  if(length(oldNlType) > 1)
    return(NA)
  
  #check if already in new format return the same
  if(grepl(pattern = "(OLS|VIIRS)\\.[D|M|Y]", x = oldNlType))
    return(oldNlType)
  
  #pre 0.2.0 all OLS is annual and all
  #VIIRS is monthly. Convert name accordingly
  if(oldNlType == "OLS")
    return("OLS.Y")
  
  if(oldNlType == "VIIRS")
    return("VIIRS.M")

  #if anything else return NA  
  message(Sys.time(), ": Invalid old name ", oldNlType)
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
  #v0.2.3 - NL_DATA_KEN_ADM0_GADM-3.6.csv
  #v0.2.4 - NL_DATA_CHN_ADM1_GADM-3.6-SHPZIP.csv
  nlDataFilePatterns <- list("pre-v0.2.0"="",
                             "v0.2.0"="",
                             "v0.2.1"="",
                             "v0.2.2"="",
                             "v0.2.3"="NL_DATA_[A-Z]{3,4}_ADM\\d+_(GADM-[2|3]\\.[8|6]|CUST).*\\.csv",
                             "v0.2.4"="NL_DATA_[A-Z]{3,4}_ADM\\d+_(GADM-[2|3]\\.[8|6]|CUST)-(SHPZIP|SPRDS)\\.csv")
  
  #v0.2.3 - NL_OLS.Y_2012_SUM
  #v0.2.4 - NL_OLS.Y_STABLE_LIGHTS-MTSALL-MEAN-RGFF_1992_SUM
  nlDataColPatterns <- list("pre-v0.2.0"="",
                            "v0.2.0"="",
                            "v0.2.1"="",
                            "v0.2.2"="",
                            "v0.2.3"="[A-Z]{3,5}.[D|M|Y]_\\d{4,8}_.*",
                            "v0.2.4"="NL_[A-Z]{3,5}.[D|M|Y]_([A-Z]+_*)+-MTS.*-MEAN-RGF[F|T]_\\d{4,8}_.*")
  
  #v0.2.0,1 - NL_ETH_VIIRS.M_201701.tif
  #v0.2.2 - NL_ETH_VIIRS.M_201701_GADM-2.8.tif; NL_ETH_VIIRS.M_201701_CUST-Ethiopia_zip_all.tif
  #v0.2.3 - NL_KEN_OLS.Y_2012_GADM-3.6.tif
  #v0.2.4 - NL_CHN_OLS.Y_1992_STABLE_LIGHTS-MTSALL-MEAN-RGFF_GADM-3.6-SHPZIP.tif 
  nlRasterFilePatterns <- list("pre-v0.2.0"="",
                               "v0.2.0"="",
                               "v0.2.1"="NL_.*_[A-Z]{3,5}\\.[A-Z]_[0-9]{4,6}",
                               "v0.2.2"="NL_.*_[A-Z]{3,5}\\.[A-Z]_[0-9]{4,6}_(GADM|CUST)-.*",
                               "v0.2.3"="NL_[A-Z]{3,4}__[A-Z]{3,5}\\.[A-Z]_(GADM-[2|3]\\.[6|8]|CUST)\\.tif",
                               "v0.2.4"="NL_[A-Z]{3,4}_[A-Z]{3,5}\\.[A-Z]_[0-9]{4,6}_(GADM|CUST)-.*")
  
  #v0.2.3 - NL_TILE_OLS.Y_2012_00N180W.tif
  #v0.2.4 - NL_TILE_OLS.Y_STABLE_LIGHTS_1992_00N180W.tif
  nlTilePatterns <- list("pre-v0.2.0"="",
                         "v0.2.0"="",
                         "v0.2.1"="",
                         "v0.2.2"="",
                         "v0.2.3"="NL_TILE_[A-Z]{3,5}\\.[D|M|Y]_\\d{4,8}_\\d{2,3}[N|S]\\d{2,3}[E|W]\\.tif",
                         "v0.2.4"="NL_TILE_[A-Z]{3,5}\\.[D|M|Y]_.*_\\d{4,8}_\\d{2,3}[N|S]\\d{2,3}[E|W]\\.tif")

  tryCatch(
  {
    #will only make alterations after the current package version updates
    pkgVersion <- utils::packageDescription("Rnightlights")$Version
    
    upgradeLog <- data.frame("idx"=NULL, "operation"=NULL, "params"=NULL, "success"=NULL)
    
    if(is.null(pkgVersion) || pkgVersion == "")
    {
      #This should never happen. But if it does we choose to ignore the issue,
      #and skip the upgrade
      message("Could not determine the installed Rnightlights version. Skipping upgrade")
      
      return(TRUE)
    }
    
    #if the gasflares dir does not exist create it
    if(!dir.exists(getNlDir("dirNlGasFlares")))
      dir.create(getNlDir("dirNlGasFlares"))
    
    dataVersionFile <- file.path(getNlDir("dirNlDataPath"), "data-version.txt")
    
    if(file.exists(dataVersionFile))
    {
      con <- file(dataVersionFile)
      
      dataVersion <- readLines(con = con,warn = F)
      
      close(con)
  
      #if the data version == pkg version
      #we are already using the latest data version. Exit
      if(dataVersion == pkgVersion)
        return(TRUE)
      
    } else
    {
      #if the data version isnt found
      #assume a pre-0.2.0 folder to force all upgrades
      dataVersion <- "0.1.0"
    }
    
    #ver 0.2.0 is the first version employing upgrade
    #upgrades required for 0.2.0, 0.2.1 & 0.2.2
    if(pkgVersion >= "0.2.0" && dataVersion < "0.2.4")
    {
      message(Sys.time(), ": Upgrading data directory to ver. ", pkgVersion)
      
      idx <- 1
      
      #rename tiles using new format
      origWd <- setwd(getNlDir("dirNlTiles"))
      
      message(Sys.time(), ": Renaming tiles:")
      
      #fileNames <- list.files(pattern = "^[[a-zA-Z]]{3,5}|[[:digit:]]{4,6}|[[:alnum:]]{7,8}\\.tif$")
      fileNames <- list.files(pattern = "*.tif")
      
      if(length(fileNames) > 0)
      {
        for(fileName in fileNames)
        {
          #split tile filename into components
          #splits <- unlist(strsplit(tools::file_path_sans_ext(fileName), "_"))
          
          nlType <- stringr::str_extract(string = fileName, "(OLS|VIIRS)(\\.[D|M|Y])?")
          
          nlType <- newNlType(nlType)
          
          configName <- unlist(stringr::str_extract(string = fileName, pattern = "(CF_CVG|AVG_VIS|STABLE_LIGHTS|PCT_LIGHTS|AVG_LIGHTS_X_PCT|VCMCFG|VCMSL|VCMCFG|VCMSL|VCM-ORM|VCM-ORM-NTL|VCM-NTL)"))
          
          configName <- if(is.na(configName)) ifelse(grepl("OLS",nlType), "AVG_VIS", "VCMCFG")  else configName  
          
          nlPeriod <- stringr::str_extract(string = fileName, "\\d{4,8}")
          
          tileName <- stringr::str_extract(string = fileName, "\\d{2,3}[N|S]\\d{2,3}[E|W]")
  
          newFileName <- getNlTileTifLclNamePath(nlType = nlType,
                                                 configName = configName,
                                                 nlPeriod = nlPeriod,
                                                 tileNum = tileName2Idx(tileName = tileName,
                                                                        nlType =  nlType))
          
          newFileName <- basename(newFileName)
          
          res <- file.rename(fileName, newFileName)
          resTxt <- paste0("Rename: '", fileName, "' -> '", newFileName, "' : ", ifelse(res, "Success", "Fail"))
          message(Sys.time(), ": ", resTxt)
          
          idx <- idx + 0.1
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx=idx, operation="file.rename", params=paste(fileName, newFileName, sep="=", collapse="|"), success=res))
        }
      }else
      {
        message(Sys.time(), ": No upgrade required")
      }
      
      #rename data files using new format
      idx <- floor(idx + 1)
      
      message(Sys.time(), ": Renaming data files:")
      setwd(getNlDir("dirNlData"))
      
      #fileNames <- list.files(pattern = "^[[:alpha:]]{3,5}_NLData\\.csv$")
      fileNames <- grep(list.files(full.names = TRUE), pattern = "STRUCT", invert = TRUE, value = TRUE)
      
      if(length(fileNames) > 0)
      {
        for(fileName in fileNames)
        {
          idx <- idx + 0.1
          #split filename into components
          
          #splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
          
          ctryCodes <- gsub("_", "", unlist(stringr::str_extract_all(string = fileName, pattern = "_.{3}_")))
          
          ctryCode <- ctryCodes[validCtryCodes(ctryCodes)]
          
          admLevel <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "_ADM.?_"))
          
          gadmVersion <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "_GADM-\\d\\.\\d"))
          
          #if gadm version not found it is 2.8
          gadmVersion <- if(is.na(gadmVersion)) "2.8" else unlist(strsplit(gadmVersion,"-"))[2]
          
          gadmPolyType <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "sh?p(Zip|Rds)"))
          
          gadmPolyType <- if(is.na(gadmPolyType)) "shpZip" else gadmPolyType
          
          custPolyPath <- gsub("_|\\.", "", stringr::str_extract(string = fileName, pattern = "_CUST-.*\\."))
          
          custPolyPath <- if(is.na(custPolyPath)) NULL else custPolyPath
          
          newFileName <- getCtryNlDataFname(ctryCode = ctryCode,
                                                          admLevel = admLevel,
                                                          gadmVersion = gadmVersion,
                                                          gadmPolyType = gadmPolyType,
                                                          custPolyPath = custPolyPath)
          
          res <- file.rename(fileName, newFileName)
          resTxt <- paste0("Rename: '", fileName, "' -> '", newFileName, "' : ", ifelse(res, "Success", "Fail"))
          message(Sys.time(), ": ", resTxt)
          
          idx <- idx + 0.1
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx=idx, operation="file.rename", params=paste(fileName, newFileName, sep="=", collapse="|"), success=res))
          
          message(Sys.time(), ": Renaming columns:")
          
          idx <- idx + 0.01
          
          ctryNlData <- utils::read.csv(newFileName, header = TRUE, check.names = FALSE, encoding = "UTF-8")
          
          cols <- names(ctryNlData)
          
          ctryCols <- grep("NL_", cols, invert = T, value = T)
          nlCols <- grep("NL_", cols, value = T)
          
          newNlCols <- sapply(nlCols, function(x){
            #colSplits <- unlist(strsplit(x, "_"))
            
            nlType <- stringr::str_extract(string = x, "(OLS|VIIRS)(\\.[D|M|Y])?")
            
            nlType <- newNlType(nlType)
            
            nlPeriod <- stringr::str_extract(string = x, "\\d{4,8}")
            
            nlStat <- tolower(gsub("_", "", stringr::str_extract(string = x, "_[A-Z]+$")))
          
            configName <- unlist(stringr::str_extract(string = x, pattern = "(CF_CVG|AVG_VIS|STABLE_LIGHTS|PCT_LIGHTS|AVG_LIGHTS_X_PCT|VCMCFG|VCMSL|VCMCFG|VCMSL|VCM-ORM|VCM-ORM-NTL|VCM-NTL)"))
            
            #gsub always returns a string
            #if configName is null it is pre-0.2.4 where configName=AVG_VIS or VCM_CFG
            configName <- if(is.na(configName)) ifelse(grepl("OLS", nlType), "AVG_VIS", "VCMCFG")  else configName  
            
            extraOptions <- gsub("_", "", stringr::str_extract(string = x, pattern = "MTS[A-Z\\-]*_"))
            
            extraOptions <- unlist(strsplit(x = extraOptions, split = "-"))
            
            multiTileStrategy <- gsub("MTS","",extraOptions[1])
            
            #gsub always returns a string
            #if null use First
            multiTileStrategy <- if(is.na(multiTileStrategy)) "first" else multiTileStrategy
            
            multiTileMergeFun <- extraOptions[2]
            
            #gsub always returns a string
            #if null set multiMergeFun to default
            multiTileMergeFun <- if(is.na(multiTileMergeFun)) "MEAN" else multiTileMergeFun
            
            removeGasFlares <- gsub("RGF", "", extraOptions[3])
            
            #gsub always returns a string
            #if null set removeGasFlares to false
            removeGasFlares <- if(is.na(removeGasFlares)) FALSE else as.logical(removeGasFlares)

            newColName <- getCtryNlDataColName(nlPeriod = nlPeriod,
                                               nlStat = nlStat,
                                               nlType = nlType,
                                               configName = configName,
                                               multiTileStrategy = multiTileStrategy,
                                               multiTileMergeFun = multiTileMergeFun,
                                               removeGasFlares = removeGasFlares)
          })
        
          message("Renaming: ", paste(nlCols, collapse="|"), " to: ", paste(newNlCols, collapse="|"))
          
          names(ctryNlData) <- c(ctryCols, newNlCols)
          
          utils::write.table(ctryNlData, newFileName, row.names = F, sep = ",")
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx=idx, operation="cols.rename", params=paste(nlCols, newNlCols, sep="=", collapse="|"), success=TRUE))
        }
      }else
      {
        message(Sys.time(), ": No upgrade required")
      }
      
      #rename rasters
      idx <- round(idx + 1)
      
      #rename tiles using new format
      message(Sys.time(), ": Renaming country rasters:")

      setwd(getNlDir("dirRasterOutput"))
      #fileNames <- list.files(pattern = "^[a-zA-Z]{3}_[a-zA-Z]{3,5}_[0-9]{4,6}\\.tif$")
      fileNames <- list.files(pattern = ".tif")
      
      if(length(fileNames) > 0)
      {
        for(fileName in fileNames)
        {
          #split tile filename into components
          #splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
          
          ctryCodes <- gsub("_", "", unlist(stringr::str_extract_all(string = fileName, pattern = "_.{3}_")))
          
          ctryCode <- ctryCodes[validCtryCodes(ctryCodes)]
          
          nlType <- stringr::str_extract(string = fileName, "(OLS|VIIRS)(\\.[D|M|Y])?")
          
          nlType <- newNlType(nlType)
          
          nlPeriod <- stringr::str_extract(string = fileName, "\\d{4,8}")
          
          configName <- unlist(stringr::str_extract(string = fileName, pattern = "(CF_CVG|AVG_VIS|STABLE_LIGHTS|PCT_LIGHTS|AVG_LIGHTS_X_PCT|VCMCFG|VCMSL|VCMCFG|VCMSL|VCM-ORM|VCM-ORM-NTL|VCM-NTL)"))
          
          #gsub always returns a string
          #if configName is null it is pre-0.2.4 where configName=AVG_VIS
          configName <- if(is.na(configName)) "AVG_VIS" else configName  
          
          extraOptions <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "MTS[A-Z\\-]*_"))
          
          extraOptions <- unlist(strsplit(x = extraOptions, split = "-"))
          
          multiTileStrategy <- gsub("MTS","",extraOptions[1])
          
          #gsub always returns a string
          #if null use First
          multiTileStrategy <- if(is.na(multiTileStrategy)) "first" else multiTileStrategy
          
          multiTileMergeFun <- extraOptions[2]
          
          #if null set multiMergeFun to default
          multiTileMergeFun <- if(is.na(multiTileMergeFun)) "MEAN" else multiTileMergeFun
          
          removeGasFlares <- gsub("RGF", "", extraOptions[3])
          
          #if null set removeGasFlares to false
          removeGasFlares <- if(is.na(removeGasFlares)) FALSE else as.logical(removeGasFlares)          
          
          gadmVersion <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "_GADM-\\d\\.\\d"))
          
          #if gadm version not found it is 2.8
          gadmVersion <- if(is.na(gadmVersion)) "2.8" else unlist(strsplit(gadmVersion,"-"))[2]
          
          gadmPolyType <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "sh?p(Zip|Rds)"))
          
          gadmPolyType <- if(is.na(gadmPolyType)) "shpZip" else gadmPolyType
          
          custPolyPath <- gsub("_|\\.", "", stringr::str_extract(string = fileName, pattern = "_CUST-.*\\."))
          
          custPolyPath <- if(is.na(custPolyPath)) NULL else custPolyPath
          
          newFileName <- getCtryRasterOutputFname(ctryCode=ctryCode,
                                                  nlType=nlType,
                                                  nlPeriod=nlPeriod,
                                                  configName = configName,
                                                  multiTileStrategy = multiTileStrategy,
                                                  multiTileMergeFun = multiTileMergeFun,
                                                  removeGasFlares = removeGasFlares,
                                                  gadmVersion = gadmVersion,
                                                  gadmPolyType = gadmPolyType,
                                                  custPolyPath = custPolyPath)
          
          res <- file.rename(fileName, newFileName)
          resTxt <- paste0("Rename: '", fileName, "' -> '", newFileName, "' : ", ifelse(res, "Success", "Fail"))
          message(Sys.time(), ": ", resTxt)
          
          idx <- idx + 0.1
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx=idx, operation="file.rename", params=paste(fileName, newFileName, sep="=", collapse="|"), success=res))
        }
      }else
      {
        message(Sys.time(), ": No upgrade required")
      }
      
      #rename polygons
      idx <- round(idx + 1)
      
      #rename tiles using new format
      message(Sys.time(), ": Renaming country rasters:")
      
      setwd(getNlDir("dirPolygon"))
      
      #fileNames <- list.files(pattern = "^[a-zA-Z]{3}_[a-zA-Z]{3,5}_[0-9]{4,6}\\.tif$")
      fileNames <- list.files()
      
      if(length(fileNames) > 0)
      {
        for(fileName in fileNames)
        {
          #split tile filename into components
          #splits <- unlist(strsplit(substr(fileName, 1, nchar(fileName)-4), "_"))
          
          ctryCodes <- gsub("_", "", unlist(stringr::str_extract_all(string = fileName, pattern = "_?.{3}_")))
          
          ctryCode <- ctryCodes[validCtryCodes(ctryCodes)]
          
          gadmVersion <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "_GADM-\\d\\.\\d"))
          
          #if gadm version not found it is 2.8
          gadmVersion <- if(is.na(gadmVersion)) "2.8" else unlist(strsplit(gadmVersion,"-"))[2]
          
          gadmPolyType <- gsub("_", "", stringr::str_extract(string = fileName, pattern = "sh?p(Zip|Rds)"))
          
          gadmPolyType <- if(is.na(gadmPolyType)) "shpZip" else gadmPolyType
          
          custPolyPath <- gsub("_|\\.", "", stringr::str_extract(string = fileName, pattern = "_CUST-.*\\."))
          
          custPolyPath <- if(is.na(custPolyPath)) NULL else custPolyPath
          
          newFileName <- if(tools::file_ext(fileName)=="RDS")
            getPolyFnameRDS(ctryCode=ctryCode,
                            gadmVersion = gadmVersion,
                            gadmPolyType = gadmPolyType,
                            custPolyPath = custPolyPath)
          else if(tools::file_ext(fileName)=="zip")
            getPolyFnameZip(ctryCode=ctryCode,
                           gadmVersion = gadmVersion,
                           gadmPolyType = gadmPolyType,
                           custPolyPath = custPolyPath)
          else
            getPolyFname(ctryCode=ctryCode,
                        gadmVersion = gadmVersion,
                        gadmPolyType = gadmPolyType,
                        custPolyPath = custPolyPath)
          

          res <- file.rename(fileName, newFileName)
          resTxt <- paste0("Rename: '", fileName, "' -> '", newFileName, "' : ", ifelse(res, "Success", "Fail"))
          message(Sys.time(), ": ", resTxt)
          
          idx <- idx + 0.1
          
          upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx=idx, operation="file.rename", params=paste(fileName, newFileName, sep="=", collapse="|"), success=res))
        }
      }else
      {
        message(Sys.time(), ": No upgrade required")
      }
      
      #remove zonal rasters will be recreated at next run
      message(Sys.time(), ": Remove Old Zonal Files:")
      setwd(getNlDir("dirZonals"))
      
      if(length(list.files()) > 0)
        message("Delete zonal tiles: ", ifelse(all(file.remove(list.files())), "Success", "Fail"))
      else
        message(Sys.time(), ": No upgrade required")
      
      #log alterations for rollback
      
      setwd(origWd)
      
      #if we got here all went well
      message(Sys.time(), ": Upgrade complete!")
      
      return(TRUE)
    }
  }, error=function(err)
  {
    message(Sys.time(), ": ", err)
    message(Sys.time(), ": The package was unable to upgrade all the data in the Rnightlights data dir. 
            Some of your old data may not be accessible from the upgraded package
            but can be accessed manually from the Rnightlights data folder. 
            Please open an issue on the package github page if you encounter
            any issues. Continuing ...")
    
    upgradeLog <- rbind.data.frame(upgradeLog, cbind(idx=999, operation="error", params=paste(err, sep="=", collapse="|"), success=FALSE))
    
    return(TRUE)
  },finally = {
    #mark as upgraded
    cat(pkgVersion, file = file.path(getNlDir("dirNlDataPath"), "data-version.txt"))
    if(nrow(upgradeLog) > 0)
    {
      if(any(!upgradeLog$success))
        message(Sys.time(), ": The package was unable to upgrade all the data in the Rnightlights data dir. 
              Some of your old data may not be accessible from the upgraded package
              but can be accessed manually from the Rnightlights data folder. 
              Please open an issue on the package github page if you encounter
              any issues. Continuing ...")
      
      message(Sys.time(), ": Writing upgrade log:")

      #print(upgradeLog)
      
      logFile = file.path(getNlDir("dirNlDataPath"), paste0("upgrade-",pkgVersion,".log"))
      
      utils::write.table(x = upgradeLog, file = logFile, sep = ",")
    }
  })
}