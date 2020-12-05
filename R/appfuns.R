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
  
  ctryCodesWithData
}

getCtryNlTypes <- function(countries, admLevel, polySrc = "GADM", polyVer = "3.6", polyType = "SHPZIP")
{
  if (length(countries) == 0)
    return()

  if (is.null(admLevel))
    return()
  
  if ((length(countries) == 0 ||
       grepl("^\\s*$", countries)) &&
      (is.null(polySrc) ||
       polySrc == "" || is.null(polyVer) || polyVer == ""))
    return()
  
  if (!(length(countries) == 0 || grepl("^\\s*$", countries)))
    if (is.null(polySrc) ||
        polySrc == "" ||
        is.null(polyVer) ||
        polyVer == "" || is.null(polyType) || polyType == "")
      return()
  
  custPolyPath <- if (polySrc == "CUST")
    polyVer
  else
    NULL
  
  nlTypes <-
    unique(
      Rnightlights::listCtryNlData(
        ctryCodes = countries,
        admLevels = admLevel,
        polySrcs = polySrc,
        polyVers = polyVer,
        polyTypes = polyType
      )$nlType
    )
  
  return(nlTypes)
}

getCtrysDataDirect <- function(countries, polySrc = "GADM", polyVer = "3.6", polyType = "SHPZIP", nlType = "VIIRS.M")
{
  if (is.null(polySrc) ||
      polySrc == "" ||
      is.null(polyVer) ||
      polyVer == "" || is.null(polyType) || polyType == "")
    return(NULL)
  
  if (is.null(nlType))
    return(NULL)
  
  ctryData <- NULL
  
  custPolyPath <- if (polySrc == "CUST")
    polyVer
  else
    NULL
  
  if (length(countries) == 1)
  {
    admLevel <-
      unlist(
        Rnightlights:::getCtryShpAllAdmLvls(
          ctryCodes = countries,
          gadmVersion = polyVer,
          gadmPolyType = polyType,
          custPolyPath = custPolyPath
        )
      )[2]
    
    ctryNlDataFile <-
      Rnightlights:::getCtryNlDataFnamePath(
        ctryCode = countries,
        admLevel = admLevel,
        gadmVersion = polyVer,
        gadmPolyType = polyType,
        custPolyPath = custPolyPath
      )
    
    if (file.exists(ctryNlDataFile))
      ctryData <- data.table::fread(ctryNlDataFile)
    else
      ctryData <- NULL
  }
  else if (length(countries) > 1)
    #remove subcountry admin levels
  {
    for (ctryCode in countries)
    {
      admLevel <- paste0(ctryCode, "_adm0")
      #print(ctryCode)
      
      ctryNlDataFile <-
        Rnightlights:::getCtryNlDataFnamePath(
          ctryCode = ctryCode,
          admLevel = admLevel,
          gadmVersion = polyVer,
          gadmPolyType = polyType,
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
