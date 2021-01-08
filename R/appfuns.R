######################## getAllGadmVersions #############################

#' Return ctryCodes that have data
#'
#' Return ctryCodes that have data
#'
#' @return character vector valid ctryCodes
#'
#' @examples
#' \dontrun{
#'  getCtryCodesWithData()
#' }
#'
#' @export
getCtryCodesWithData <- function()
{
  print("ctryCodesWithData")
  
  existingData <- Rnightlights::listCtryNlData()
  
  ctryCodesWithData <- unique(existingData$ctryCode)
  
  ctryCodeNames <-
    lapply(ctryCodesWithData, function(x)
      Rnightlights::ctryCodeToName(x))
  
  ctryCodeNames[is.na(ctryCodeNames)] <- "---"
  
  if (!is.null(ctryCodesWithData))
    ctryCodesWithData <-
    stats::setNames(ctryCodesWithData, ctryCodeNames)
  
  ctryCodesWithData <- if(length(ctryCodesWithData) > 0) ctryCodesWithData else NULL
  
  ctryCodesWithData
}

######################## getCtryNlTypes #############################

#' Return the unique nlTypes available in the data
#'
#' Return the unique nlTypes available in the data
#'
#' @param ctryCodes the ISO3 code of the countries
#'
#' @param admLevel The country admin level of interest
#'
#' @param gadmPolySrc The GADM source to use
#' 
#' @param gadmPolyVer The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character vector available nlTypes
#'
#' @examples
#' \dontrun{
#'  getCtryNlTypes()
#' }
#'
#' @export
getCtryNlTypes <- function(ctryCodes, 
                           admLevel,
                           gadmPolySrc = "GADM",
                           gadmPolyVer = "3.6",
                           gadmPolyType = "SHPZIP",
                           custPolyPath)
{
  if (length(ctryCodes) == 0)
    return()

  if (is.null(admLevel))
    return()
  
  if ((length(ctryCodes) == 0 ||
       grepl("^\\s*$", ctryCodes)) &&
      (is.null(gadmPolySrc) ||
       gadmPolySrc == "" || is.null(gadmPolyVer) || gadmPolyVer == ""))
    return()
  
  if (!(length(ctryCodes) == 0 || grepl("^\\s*$", ctryCodes)))
    if (is.null(gadmPolySrc) ||
        gadmPolySrc == "" ||
        is.null(gadmPolyVer) ||
        gadmPolyVer == "" || is.null(gadmPolyType) || gadmPolyType == "")
      return()
  
  nlTypes <-
    unique(
      Rnightlights::listCtryNlData(
        ctryCodes = ctryCodes,
        admLevels = admLevel,
        polySrcs = gadmPolySrc,
        polyVers = gadmPolyVer,
        polyTypes = gadmPolyType
      )$nlType
    )
  
  return(nlTypes)
}

######################## getCtrysDataDirect #############################

#' Read the ctryNlData directly from files
#'
#' Read the ctryNlData directly from files
#'
#' @param ctryCodes the ISO3 code of the countries
#'
#' @param admLevel The country admin level of interest
#' 
#' @param nlType The nlType of interest
#'
#' @param gadmPolySrc The polygon source to use
#'
#' @param gadmPolyVer The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character vector available nlTypes
#'
#' @examples
#' \dontrun{
#'  getCtryNlTypes()
#' }
#'
#' @export
getCtrysDataDirect <- function(ctryCodes, admLevel, gadmPolySrc = "GADM",
                               gadmPolyVer = "3.6", gadmPolyType = "SHPZIP",
                               nlType = "VIIRS.M", custPolyPath = NULL)
{
  if (is.null(gadmPolySrc) ||
      gadmPolySrc == "" ||
      is.null(gadmPolyVer) ||
      gadmPolyVer == "" || is.null(gadmPolyType) || gadmPolyType == "")
    return(NULL)
  
  if (is.null(nlType))
    return(NULL)
  
  ctryData <- NULL

  if (length(ctryCodes) == 1)
  {
    if(missing(admLevel)||length(admLevel) ==0)
      admLevel <-
        unlist(
          getCtryShpAllAdmLvls(
            ctryCodes = ctryCodes,
            gadmVersion = gadmPolyVer,
            gadmPolyType = gadmPolyType,
            custPolyPath = custPolyPath
          )
        )[2]
    
    ctryNlDataFile <-
      getCtryNlDataFnamePath(
        ctryCode = ctryCodes,
        admLevel = admLevel,
        gadmVersion = gadmPolyVer,
        gadmPolyType = gadmPolyType,
        custPolyPath = custPolyPath
      )
    
    if (file.exists(ctryNlDataFile))
      ctryData <- data.table::fread(ctryNlDataFile)
    else
      ctryData <- NULL
  }
  else if (length(ctryCodes) > 1)
    #remove subcountry admin levels
  {
    for (ctryCode in ctryCodes)
    {
      admLevel <- paste0(ctryCode, "_adm0")
      #print(ctryCode)
      
      ctryNlDataFile <-
        getCtryNlDataFnamePath(
          ctryCode = ctryCode,
          admLevel = admLevel,
          gadmVersion = gadmPolyVer,
          gadmPolyType = gadmPolyType,
          custPolyPath = custPolyPath
        )
      
      if (file.exists(ctryNlDataFile))
        temp <- data.table::fread(ctryNlDataFile)
      else
        temp <- NULL
      
      ctryCols <-
        grep(paste0("country|area|NL_", nlType), names(temp))
      
      temp <- temp[, ctryCols, with = F]
      
      if (is.null(ctryData))
      {
        ctryData <- temp
      } else
      {
        ctryData <- merge(ctryData, temp, all = TRUE)
      }
    }
  }
  
  #get the nlType columns
  ctryCols <- names(ctryData)
  
  ctryNonNLCols <- grep("NL_", ctryCols, invert = T, value = T)
  ctryNLCols <- grep("NL_", ctryCols, value = T)
  
  ctryNLColsNlType <- grep(nlType, ctryNLCols, value = T)
  
  ctryData <-
    ctryData[, c(ctryNonNLCols, ctryNLColsNlType), with = F]
  
  return(ctryData)
}