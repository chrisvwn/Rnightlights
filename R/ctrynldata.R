######################## createCtryNlDataDF ###################################

#' Initiates the country nightlight dataframe with the country admin level 
#'     data read from the polygon
#'
#' Initiates the country admin level nightlight dataframe with the country 
#'     admin level data read from the polygon. This includes admin levels, 
#'     level names and area
#'
#' @param ctryCode the ISO3 code of the country
#' 
#' @param admLevel The country admin level of interest
#'
#' @return dataframe with the country admin level data
#'
#' @examples
#' \dontrun{
#' initCtryNlData <- Rnightlights:::createCtryNlDataDF("KEN")
#'  #returns a data frame
#' }
#' 
#'
createCtryNlDataDF <- function(ctryCode, admLevel=getCtryShpLowestLyrNames(ctryCode))
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  ctryPoly <- readCtryPolyAdmLayer(ctryCode, admLevel)
  
  ctryExtent <- raster::extent(ctryPoly)
  
  raster::projection(ctryPoly) <- sp::CRS(wgs84)
  
  #get the list of admin levels in the polygon shapefile
  ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode, admLevel)
  
  #conver to lower case for consistency
  ctryPolyAdmLevels <- tolower(ctryPolyAdmLevels)

  if (length(ctryPolyAdmLevels) > 0)
  {
    #When a country does not have lower administrative levels
    
    #the number of admin levels
    nLyrs <- ctryShpLyrName2Num(admLevel) #length(ctryPolyAdmLevels)
    
    ctryPolyAdmCols <- paste(c("NAME_"), 1:nLyrs, sep="")
    
    #pull the ID_ and NAME_ cols from layer1 to lowest layer (layer0 has country code not req'd)
    ctryNlDataDF <- as.data.frame(ctryPoly@data[,eval(ctryPolyAdmCols)])
    
    #add the area as reported by the polygon shapefile as a convenience
    areas <- raster::area(ctryPoly)/1e6
    
    #we add the country code to ensure even a renamed file is identifiable
    #repeat ctryCode for each row in the polygon. equiv of picking layer0
    ctryCodeCol <- rep(ctryCode, nrow(ctryNlDataDF))
    
    #combine the columns
    ctryNlDataDF <- cbind(ctryCodeCol, ctryNlDataDF, areas)
    
    #ctryPolyColNames <- paste(ctryPolyAdmLevels[nums, "name"], c("_id", "_name"), sep="")
    ctryPolyColNames <- ctryPolyAdmLevels
    
    #add the country code and area columns to the dataframe
    ctryPolyColNames <- c("country", ctryPolyColNames, "area_sq_km")
    
    names(ctryNlDataDF) <- ctryPolyColNames
  } else
  {
    #add the area as reported by the polygon shapefile as a convenience
    areas <- raster::area(ctryPoly)/1e6
    
    ctryNlDataDF <- data.frame("country"=ctryCode, "area_sq_km"=areas)
  }
  
  return(ctryNlDataDF)
}

######################## insertNlDataCol ###################################

#' Insert an aggregate nightlight data column in a country nightlights dataframe
#'
#' Insert an aggregate nightlight data column in a country nightlights dataframe. The number
#'     of elements in the vector MUST match the number of rows in the country dataframe.
#'
#' @param ctryNlDataDF dataframe with the country data to save
#' 
#' @param dataCol the numeric vector to be inserted as a column
#' 
#' @param statType the stat which produced the dataCol vector
#' 
#' @param nlPeriod the nlPeriod that the dataCol belongs to
#' 
#' @param nlType the type of nightlight data
#'
#' @return the updated dataframe
#'
#' @examples
#' 
#' \dontrun{
#' ctryNlDataDF <- Rnightlights:::insertNlDataCol(ctryNlDataDF, 
#'     dataCol, "sum", "201409", "VIIRS.D")
#'     }
#' 
#' \dontrun{
#' ctryNlDataDF <- Rnightlights:::insertNlDataCol(ctryNlDataDF, 
#'     dataCol, "mean", "2012", "OLS.Y")
#'     }
#'
insertNlDataCol <- function (ctryNlDataDF, dataCol, statType, nlPeriod, nlType)
{
  if(missing(ctryNlDataDF))
    stop("Missing required parameter ctryNlDataDF")
  
  if(missing(dataCol))
    stop("Missing required parameter dataCol")
  
  if(missing(statType))
    stop("Missing required parameter statType")
  
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(missing(nlType))
    warning("Missing required parameter nlType")
  
  
  #append the calculated means for the polygon as a new column
  ctryNlDataDF <- cbind(ctryNlDataDF, dataCol)
  
  #name the new column which is currently last with the yearmonth of the data
  names(ctryNlDataDF)[ncol(ctryNlDataDF)] <- getCtryNlDataColName(nlPeriod = nlPeriod, nlStat = statType, nlType = nlType)
  
  #re-arrange the columns
  #read in all column names in the dataframe afresh
  cols <- names(ctryNlDataDF)
  
  #get only the nightlight data columns
  nlDataColIdx <- grep("^NL_", cols)
  
  nlDataColNames <- cols[nlDataColIdx]
  
  #sort the column names ascending
  nlDataColNames <- nlDataColNames[order(nlDataColNames)]
  
  #combine the non-nightlight and the nightlight data columns
  newNlDataColNames <- c(cols[-nlDataColIdx], nlDataColNames)
  
  #write back the dataframe with the new column order
  ctryNlDataDF <- ctryNlDataDF[ , newNlDataColNames]
  
  return(ctryNlDataDF)
}

######################## deleteNlDataCol ###################################

#' Delete an aggregate nightlight data column in a country nightlights dataframe
#'
#' Delete an aggregate nightlight data column in a country nightlights dataframe. The number
#'     of elements in the vector MUST match the number of rows in the country dataframe.
#'
#' @param ctryCode country with the  data column to remove
#' 
#' @param nlType  the type of nightlight data
#' 
#' @param nlPeriod the nlPeriod that the dataCol belongs to
#' 
#' @param statType the stat which produced the dataCol vector
#'
#' @examples
#' 
#' \dontrun{
#' ctryNlDataDF <- Rnightlights:::deleteNlDataCol(ctryNlDataDF, 
#'      "VIIRS.M", "201409", "sum")
#'      }
#' 
#' \dontrun{
#' Rnightlights:::deleteNlDataCol(ctryNlDataDF, 
#'     "OLS.Y", "2012", "mean")
#'     }
#'
#' @export
deleteNlDataCol <- function (ctryCode,nlType, nlPeriod, statType)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(missing(nlType))
    warning("Missing required parameter nlType")
  
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(missing(statType))
    stop("Missing required parameter statType")
  
  ctryNlDataDF <- getCtryNlData(ctryCode = ctryCode,
                                admLevel = getCtryShpLowestLyrNames(ctryCode),
                                nlTypes = nlType,
                                nlPeriods = getAllNlPeriods(nlType), 
                                ignoreMissing = TRUE)
  
  #read in all column names in the dataframe
  cols <- names(ctryNlDataDF)
  
  colName <- paste0("NL", nlType, toupper(statType), collapse = "_")
  
  #get only the named nightlight data column(s)
  nlDataColIdx <- grep(colName, cols)
  
  if(length(nlDataColIdx) == 0)
    stop("Specified column not found")
  
  #write back the dataframe with the new column order
  ctryNlDataDF <- ctryNlDataDF[ , -c(nlDataColIdx)]
  
  saveCtryNlData(ctryNlDataDF, ctryCode)
  
  return(TRUE)
}

######################## saveCtryNlData ###################################

#' Save a data frame of a country's data to the appropriate location
#'
#' Saves the data frame created from processNlCountry* to the appropriate location. 
#'     Note: This function does not perform any validation error checking and will overwrite 
#'     existing data. Use with caution.
#'
#' @param ctryNlDataDF dataframe with the country data to save
#' 
#' @param ctryCode the ctryCode to which the data belongs
#' 
#' @param admLevel the country admin level to process
#'
#' @return None
#'
#' @examples
#' 
#' \dontrun{
#' Rnightlights:::saveCtryNlData(ctryNlDataDF, ctryCode)
#' }
#'
saveCtryNlData <- function(ctryNlDataDF, ctryCode, admLevel)
{
  if(missing(ctryNlDataDF))
    stop("Missing required parameter ctryNlDataDF")
  
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(admLevel))
    stop("Missing required parameter admLevel")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validCtryNlDataDF(ctryNlDataDF))
    stop("Invalid country dataframe")
  
  utils::write.table(ctryNlDataDF, getCtryNlDataFnamePath(ctryCode = ctryCode,admLevel = admLevel), row.names = F, sep = ",")
}

######################## validCtryNlDataDF ###################################

#' Check if a country dataframe is valid
#'
#' Check if a country dataframe is valid
#'
#' @param ctryNlDataDF the country dataframe
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{
#'   Rnightlights:::validCtryNlDataDF(nlCtryDataDF) #returns TRUE
#' }
#'  
#'
validCtryNlDataDF <- function(ctryNlDataDF)
{
  if(missing(ctryNlDataDF))
    stop("Missing required parameter ctryNlDataDF")
  
  if(class(ctryNlDataDF) == "data.frame" && !is.null(ctryNlDataDF) && names(ctryNlDataDF)[1] == "country")
    return(TRUE)
  else
    return(FALSE)
}

######################## getCtryNlDataFname ###################################

#' Construct the name of the country data file.
#'
#' Construct the name of the data file. This function can be altered to name the file as 
#'     required and consistently retrieve the name. Used in the function getCtryNlDataFnamePath 
#'     to concat the directory path and this filename. Currently all nlTypes are stored in one 
#'     file. Can be altered to separate VIIRS and OLS data files for example.
#'
#' @param ctryCode The ctryCode of interest
#' 
#' @param admLevel The country admin level of interest
#'
#' @return Character filename of the country data file
#'
#' @examples
#' ctryCode <- "KEN"
#' admLevel <- "KEN_adm0"
#' Rnightlights:::getCtryNlDataFname(ctryCode, admLevel)
#' #returns string of name of the ctry data file
#'
getCtryNlDataFname <- function(ctryCode, admLevel)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(admLevel))
    stop("Missing required parameter admLevel")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  return (paste0(paste("NL", "DATA", toupper(admLevel), sep="_"), ".csv"))
}

######################## getCtryNlDataFnamePath ###################################

#' Construct the full path to save the file containing the country data
#'
#' Construct the full path to save the file containing the country data. Note
#'     it does not indicate if the file exists
#' 
#' @param ctryCode \code{character string} The ctryCode of interest
#' 
#' @param admLevel \code{character string} The admin level of interest
#'
#' @return Character string the full path to the data file of a country
#'             admin level
#'
#' @examples
#' #get the full path to the file containing data for KEN counties
#' getCtryNlDataFnamePath("KEN", "KEN_adm0")
#'
#' #@export only due to exploreData() shiny app
#' @export
getCtryNlDataFnamePath <- function(ctryCode, admLevel)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(admLevel))
    stop("Missing required parameter admLevel")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)

  return (file.path(getNlDir("dirNlData"), getCtryNlDataFname(ctryCode, admLevel)))
}

######################## getCtryNlData ###################################

#' Returns nightlight statistics for the given ctryCode and nlType in the given nPeriods
#'
#' Returns nightlight data for the given ctryCode and nlStats in the given 
#'     nlPeriods and of the specified nlType. Note that getCtryNldata only
#'     processes one ctryCode at a time.
#'     \code{ignoreMissing} plays a significant role here. It can take 3 values:
#'     
#'     \itemize{
#'         \item NULL (default) only return data if found for all nlPeriods
#'            and all nlStats provided otherwise return NULL.
#'         \item TRUE return any partial data that is found for the provided 
#'            nlPeriods and nlStats. Ignore any missing data.
#'         \item FALSE return all data that is found and call \code{processNlData}
#'            to download and process any missing nlPeriods and nlStats.
#'     }
#'    
#'    Farther, if \code{nlPeriods} is missing, it is assigned values based on
#'    the value of ignoreMissing. If ignoreMissing is FALSE, nlPeriods is 
#'    assigned all existing nlPeriods to date. This is the equivalent of 
#'    retrieving all nightlight data for the given country and stats. If 
#'    ignoreMissing is TRUE or NULL then the existing data is returned.
#'
#' @param ctryCode the ISO3 code of the country. Only 1 country can be 
#'     processed at a time.
#'     
#' @param admLevel The country admin level of interest. Only 1 admLevel
#'     can be processed at a time.
#' 
#' @param nlTypes a vector of nlTypes. The nightlight types to process.
#'
#' @param nlPeriods a vector of nlPeriods. Must be appropriate nlPeriods
#'     for the nlType.
#' 
#' @param nlStats a vector of nlStats. If not supplied defaults to all nlStats
#'     as listed in pkgOptions("nlStats").
#' 
#' @param ignoreMissing controls how the function behaves if any data is not
#'     found in the data file.
#'     
#'     \itemize{
#'         \item NULL (default) only return data if found for ALL nlPeriods
#'            and ALL stats provided otherwise return NULL
#'         \item TRUE return any partial data that is found for the provided 
#'            nlPeriods and stats. Ignore any missing data
#'         \item FALSE return all data that is found and call \code{processNlData}
#'            to download and process any missing nlPeriods and stats
#'     }
#' 
#' @param source "local" or "remote" Whether to download and process the
#'     data locally or to download the pre-processed data from a remote 
#'     source/repo
#'     
#' @return dataframe of data for one country in one nlType in one or multiple
#'     nlPeriods
#'
#' @examples
#' #NOTE: missing stats implies all stats as given by pkgOptions("nlStats")
#' 
#' #long running examples which also require large downloads
#' \dontrun{
#' getCtryNlData("KEN", "KEN_adm0", "VIIRS.M", ignoreMissing=NULL)
#'     #returns either all requested data if it exists i.e. all nlPeriods
#'     #and all nlStats for KEN otherwise NULL
#' }
#'
#' \dontrun{
#' getCtryNlData("KEN", "KEN_adm0", "OLS.Y", ignoreMissing=TRUE)
#'     #Returns all requested data if it exists i.e. all nlPeriods and all
#'     #nlStats for KEN but omits any missing data
#'     }
#'  
#' \dontrun{
#' getCtryNlData(ctryCode="KEN", "KEN_adm0", "VIIRS.Y", ignoreMissing=FALSE)
#'     #Returns all requested data i.e. all nlPeriods and all
#'     #nlStats for KEN. All missing data will be downloaded and processed
#'     }
#'  
#' \dontrun{
#' getCtryNlData("KEN", "KEN_adm0", "VIIRS.M", nlPeriods=c("existingNlPeriod", "missingNlPeriod"),
#'     nlStats=c("sum", "unknownStat"), ignoreMissing=NULL)
#'     #Returns NULL due to missingNlPeriod and unknownStat not already existing
#'     #(ignoreMissing=NULL returns all data if exists or if any is missing returns NULL)
#'     }
#'
#' \dontrun{
#' getCtryNlData("KEN", "KEN_adm0", "VIIRS.D", nlPeriods=c("existingNlPeriod", "missingNlPeriod"),
#'     nlStats=c("existingStat", "missingStat"), ignoreMissing=TRUE)
#'    #Returns existingStat for existingNlPeriods omits missingNlPeriod and missingStat
#'    #(ignoreMissing=TRUE returns only existing data)
#'    }
#'  
#' \dontrun{
#' getCtryNlData("KEN", "KEN_adm0", "VIIRS.M", nlPeriods=c("existingNlPeriod", "missingNlPeriod"),
#'     nlStats=c("sum", "unknownStat"), ignoreMissing=FALSE)
#'     #Runs processNlData for missingStat in "missingNlPeriod" and returns
#'     #"existingStat" and "missingStat" for both "existingNlPeriod" and
#'     #"missingNlPeriod"
#'     #(ignoreMissing=FALSE must return all data: forces processing of any missing)
#'     }
#'  
#' @export
getCtryNlData <- function(ctryCode, admLevel, nlTypes, nlPeriods, nlStats=pkgOptions("nlStats"), ignoreMissing=NULL, source="local")
{
  if(source != "local")
    stop("Non-local sources not currently supported. \n
         Please request this feature at https://github.com/chrisvwn/Rnightlights to fasttrack it.")
  
  if(missing(ctryCode))
    stop("Missing required ctryCode")
  
  if(missing(admLevel))
    stop("Missing required admLevel")

  if(missing(nlTypes))
    stop("Missing required parameter nlTypes")
  
  #if both nlPeriods and ignoreMissing are not supplied we cannot deduce
  #the nlPeriods. Error and stop
  if(missing(nlPeriods) && missing(ignoreMissing))
    stop("Missing required parameter nlPeriods")
    
  #if nlPeriods is not provided and ignoreMissing is present and FALSE
  #process all nlPeriods
  if(missing(nlPeriods) && !missing(ignoreMissing))
    if (!ignoreMissing)
      nlPeriods <- getAllNlPeriods(nlTypes)
  
  #if(!allValid(nlPeriods, validNlPeriods, nlType))
  if(!allValidNlPeriods(nlTypes = nlTypes, nlPeriods = nlPeriods))
    stop("Invalid nlPeriods detected")
  
  #if(missing(ignoreMissing))
  #  ignoreMissing = TRUE
  
  if(length(ctryCode) > 1 || length(admLevel) > 1)
    stop("getCtryNlData can only process 1 ctryCode & 1 admLevel at a time")

  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode", ctryCode)
  
  if(admLevel=="country")
    admLevel <- getCtryShpLyrNames(ctryCode, 0)
  else if(admLevel %in% c("bottom", "lowest"))
    admLevel <- getCtryShpLowestLyrNames(ctryCode)
  else if(admLevel %in% c("top","highest"))
    admLevel <- getCtryShpLyrNames(ctryCode, 1)
  else if(admLevel=="all")
    admLevel <- getCtryShpAllAdmLvls(ctryCode)
  else
  {
    tmpAdmLevel <- searchAdmLevel(ctryCode, admLevel)
    
    admLevel <- ifelse(is.na(tmpAdmLevel), admLevel, tmpAdmLevel)
  }
    
  #after processing admLevels if any are not in proper format e.g. KEN_adm0
  #check if they might have been supplied as e.g. adm0 or e.g. 0
  if(!length(grep(paste0(ctryCode,"_adm\\d+"), admLevel, ignore.case = T)) == length(admLevel))
  {
    if(length(grep("^adm\\d+$", admLevel, ignore.case = T)) > 0)
      admLevel <- paste(ctryCode, admLevel, sep="_")
    else if(length(grep("^\\d+$", admLevel, ignore.case = T)) > 0)
      admLevel <- paste(ctryCode, paste0("adm", admLevel), sep="_")
  }
    
  if(!allValidCtryAdmLvls(ctryCode, admLevel))
    stop("Invalid admLevels detected")
    
  if(!is.null(ignoreMissing))
    if(ignoreMissing && !existsCtryNlDataFile(ctryCode, admLevel))
      stop("No data exists for ", ctryCode, ". Set IgnoreMissing=FALSE to download and process")
  
  if (!missing(nlPeriods)) #if nlPeriods is provided process else return all ctry data
  {
    #check if the stats exist in the given year months will test nlYm1+stat1, nlYm2+stat1, ..., nlYm1+stat2, nlYm2+stat2
    if(is.list(nlPeriods))
      a <- lapply(1:length(nlTypes), function(i) cbind(nlTypes[i], nlPeriods[[i]]))
    else
      a <- lapply(1:length(nlTypes), function(i) cbind(nlTypes[i], nlPeriods))
    
    a <- data.frame(do.call("rbind", a), stringsAsFactors = F)
    
    if(length(nlStats) == 1)
      nlPeriodStats <- data.frame(a, X3=nlStats, stringsAsFactors = F)
    else
      nlPeriodStats <- data.frame(apply(a, 2,function(x) rep(x, length(nlStats))), X3=as.vector(sapply(nlStats, rep, nrow(a))), stringsAsFactors = F)
    
    #nlPeriodStats <- nlPeriodStats[order(nlPeriodStats$X1, nlPeriodStats$X2),]
    
    existnlPeriodStats <- apply(nlPeriodStats, 1, function(x) existsCtryNlData(ctryCode = ctryCode, admLevel = admLevel, nlTypes =  x[1], nlPeriods = x[2], nlStats = as.character(x[3])))
    
    missingData <- paste0(apply(nlPeriodStats[!existnlPeriodStats,], 1, function(x)paste0(x[1], ":", x[2], ":", x[3])), collapse = ", ")
    
    if (!all(existnlPeriodStats))
    {
      if (is.null(ignoreMissing)) #default
      {
        message(paste0("No data found for ", ctryCode, ":", admLevel, " in ", missingData,
                       ". Returning NULL. \nNote: Set ignoreMissing=TRUE to",
                       "return only data found or \nignoreMissing=FALSE to download ",
                       "and extract missing data"))
        return (NULL)
      }
      else if(!ignoreMissing)
      {
        message(paste0("Processing missing data: ", ctryCode, ":", missingData, 
                ". This may take a while. \nNote: Set 'ignoreMissing=TRUE' to ",
                "return only data found or \n'ignoreMissing=NULL' to return NULL ",
                "if not all the data is found"))
        
        processNlData(ctryCode, admLevel, nlTypes = nlTypes, nlPeriods, nlStats = nlStats)
      }
      else if (ignoreMissing)
      {
        message(paste0("Ignoring missing data for ", ctryCode, ":", admLevel, " in ", missingData, 
                ". \nReturning existing data only."))
      }
      else
      {
        message(paste0("Invalid value for 'ignoreMissing'. Exiting.",
                       "\nNote: Set ignoreMissing=TRUE to return local data found ",
                       "or \nignoreMissing=FALSE to download and extract missing data"))
        return(NULL)
      }
    }
  }
  else
  {
    #else if missing nlPeriods
    #if !missing(stats) return only given stats
    #else return the whole data frame
    if(existsCtryNlDataFile(ctryCode, admLevel))
      ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode)))
    else
    {
      message("Data for ", ctryCode, " does not exist. Set IgnoreMissing=FALSE to download and process")
      ctryData <- NULL
    }
    
    return(ctryData)
  }
  
  #to remove any missing nlPeriods if ignoreMissing==TRUE
  if(is.null(ignoreMissing) || ignoreMissing) #shortcircuit if ignoreMissing is NULL to avoid crash
  {
    if(any(existnlPeriodStats))
      existingCols <- apply(nlPeriodStats[existnlPeriodStats,], 1, function(x) getCtryNlDataColName(nlType = x[1], nlPeriod = x[2], nlStat = x[3]))
    else
      existingCols <- NULL
  }
  else 
  {
    #ignoreMissing == FALSE so we should have the missing data
    #check again to see that processNlData was successful
    #check that each stat exists for given periods
    if (stringr::str_detect(nlTypes, "VIIRS"))
      existnlPeriodStats <- apply(nlPeriodStats, 1, function(x) existsCtryNlData(ctryCode = ctryCode, admLevel = admLevel, nlTypes = x[1], nlPeriods = x[2], x[3]))
    else if (stringr::str_detect(nlTypes, "OLS"))
      existnlPeriodStats <- apply(nlPeriodStats, 1, function(x) existsCtryNlData(ctryCode = ctryCode, admLevel = admLevel, x[1], x[2], x[3]))
    
    #if they all exist get the list of column names
    if(all(existnlPeriodStats))
    {
      existingCols <- apply(nlPeriodStats[existnlPeriodStats,], 1, function(x) getCtryNlDataColName(nlPeriod = x[2], nlType = x[1], nlStat = x[3]))
      
      message("All stats exist")
    }
    else #else processNlData was unsuccessful i.e. an error occurred
      stop("An error occurred")
  }
  
  #if no nightlight columns return NULL
  if(length(existingCols) < 1)
  {
    message("No nightlight data. Returning ctry admin data only")
    #return(NULL)
  }
  else #else we found nl data
  {
    message("Retrieving requested data")
  }
  
  ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel)))
  
  #get the names of the columns in the data file
  cols <- names(ctryData)
  
  #retain columns with country admin levels i.e. those that don't start with
  #"NL_"
  cols <- cols[grep("^[^NL_]", cols)]
  
  nlCols <- existingCols

  #combine country admin levels and  existing cols  
  cols <- c(cols, nlCols)
  
  #add the column with the relevant nlPeriod
  ctryData <- ctryData[,cols]
  
  return(ctryData)
}

######################## getCtryNlDataColName ###################################

#' Construct the name of a nightlight data column given the nightlight type and nlPeriod
#'
#' Construct the name of a nightlight data column given the nightlight type and nlPeriod Used in
#'     creating and retrieving data columns from the nightlight data file
#'
#' @param nlPeriod character vector The nlPeriod to process
#'
#' @param nlStat character vector The stat to be stored in the column
#' 
#' @param nlType character vector The type of nightlight
#'
#' @return character string
#'
#' @examples
#' 
#' Rnightlights:::getCtryNlDataColName("201612", "sum", nlType="VIIRS.M")
#'   
getCtryNlDataColName <- function(nlPeriod, nlStat, nlType)
{
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(missing(nlStat))
    stop("Missing required parameter stat")
  
  if(missing(nlType))
    stop("Missing required parameter nlType")
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop("Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if (!allValid(nlStat, validNlStats))
    stop("Invalid/unsupported nlStat detected")
  
  colName <- paste0("NL_", nlType, "_")
  
  colName <- paste0(colName, sapply(nlPeriod, function(x) paste0(x, "_", toupper(nlStat))))
  
  colName <- sort(colName)
  
  return(colName)
}

######################## existsCtryNlDataFile ###################################

#' Check if a country admin level data file exists
#'
#' Check if a country admin level data file exists. Stats are calculated
#'     and stored at the admin level of a country, hence, a country could
#'     have as many files as admin levels.
#'
#' @param ctryCode the ISO3 country code
#' 
#' @param admLevel The country admin level of interest.
#'
#' @return TRUE/FALSE
#'
#' @examples
#' ctryCode <- "KEN"
#' admLevel <- "KEN_adm0"
#' message("Data file for ", ctryCode, 
#'     ifelse(Rnightlights:::existsCtryNlDataFile(ctryCode, admLevel), 
#'         " FOUND", " NOT FOUND"))
#'
existsCtryNlDataFile <- function(ctryCode, admLevel)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(admLevel))
    stop("Missing required parameter admLevel")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid/Unknown ctryCode: ", ctryCode)
  
  #for polygons look for shapefile dir
  return(file.exists(getCtryNlDataFnamePath(ctryCode, admLevel)))
}

######################## existsCtryNlData ###################################

#' Check if VIIRS nightlight stats exist locally
#'
#' Check if VIIRS nightlight data for the country exists in the country 
#'     nightlight data file. First checks if the country nightlight data
#'     file exists.
#'
#' @param ctryCode character The ISO3 code of the country
#' 
#' @param admLevel character string The country admin level of interest
#'
#' @param nlPeriods character The nlPeriods
#' 
#' @param nlStats character The nlStats to check for
#' 
#' @param nlTypes character The nlTypes
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsCtryNlData("KEN", "KEN_adm0", "VIIRS.M","201401", "sum")
#'
existsCtryNlData <- function(ctryCode, admLevel, nlTypes, nlPeriods, nlStats)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(missing(admLevel))
    stop("Missing required parameter admLevel")
  
  if(missing(nlPeriods))
    stop("Missing required parameter nlPeriod")

  if(missing(nlStats))
    stop("Missing required parameter nlStats")

  if(missing(nlTypes))
    stop("Missing required parameter nlTypes")

  if(length(ctryCode) > 1 || length(admLevel) > 1)
    stop("Only 1 ctryCode and admLevel can be checked at a time")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode(s) ")

  if(!allValidNlPeriods(nlPeriods=nlPeriods, nlTypes=nlTypes))
    stop("Invalid nlPeriod: ", nlPeriods, " for nlType: ", nlTypes)

  if(!all(validNlStats(nlStats)))
    stop("Invalid nlStat: ", nlStats)

  if (existsCtryNlDataFile(ctryCode, admLevel))
    dta <- utils::read.csv(getCtryNlDataFnamePath(ctryCode, admLevel), nrow=1, header=TRUE)
  else
    dta <- NULL
  
  hdrs <- names(dta)

  searchCols <- paste("NL", nlTypes, nlPeriods, toupper(nlStats), sep="_")

  return(unlist(lapply(searchCols, function(x) length(grep(x, hdrs, ignore.case = T))>0)))
}

allExistsCtryNlData <- function(ctryCodes, admLevels, nlTypes, nlPeriods, nlStats)
{
  all(unlist(existsCtryNlData(ctryCode = ctryCodes, admLevel = admLevels, nlPeriods = nlPeriods, nlStats = nlStats, nlTypes = nlTypes)))
}

######################## listCtryNlData ###################################

#' List available data
#'
#' List available data. If source is "local" it lists data cached locally.
#'     If source is remote lists available data on the remote repository.
#' 
#' @param ctryCodes  A character vector of ctryCodes to filter by
#' 
#' @param admLevels A character vector of admLevels to filter by
#' 
#' @param nlPeriods A character vector of nlPeriods to filter by
#' 
#' @param nlTypes A character vector of nlTypes to filter by
#' 
#' @param source Character string. Whether to check data availability "local" or "remote"
#'     Not in use.
#' 
#' @return a list of countries and the periods and stats for each
#'
#' @examples
#' #list all data
#' listCtryNlData()
#' 
#' #list all data available for KEN
#' listCtryNlData(ctryCodes = "KEN")
#' 
#' #list all VIIRS.* data available for ECU
#' listCtryNlData(ctryCodes = "ECU", nlTypes = "VIIRS")
#' 
#' #list available OLS.Y data for KEN and RWA in 2012 & 2013
#' listCtryNlData(ctryCodes = c("KEN","RWA"), nlPeriods = c("2012", "2013"), nlTypes = "OLS.Y")
#'
#' @export
listCtryNlData <- function(ctryCodes=NULL, admLevels=NULL, nlPeriods=NULL, nlTypes=NULL, source="local")
{
  dataList <- NULL
  dataType <- NULL #appease CRAN note for global variables
  nlType <- NULL #appease CRAN note for global variables
  nlPeriod <- NULL #appease CRAN note for global variables
  
  #get a list of country data files present
  countries <- list.files(getNlDir("dirNlData"), pattern = "^NL_DATA_.*\\.csv$")
  
  #for each country filename
  for (ctry in countries)
  {
    #get first 3 chars which gives the ctryCode
    ctryCode <- substr(ctry, 9, 11)
    
    admLevel <- substr(ctry, nchar(ctry)-7, nchar(ctry)-4)
    
    #read in the header row
    ctryHdr <- data.table::fread(file.path(getNlDir("dirNlData"), ctry), header = F, nrows = 1)
    
    #grab only the NL cols
    nlCtryHdr <- grep("^NL", ctryHdr, value = T)
    
    #split the NL colnames into their components e.g. "NL_OLS_2012_MEAN"
    #= "NL"+nlType+nlPeriod+stat
    nlCtryHdr <- reshape2::colsplit(nlCtryHdr, "_", c("V1", "V2","V3","V4"))
    
    #aggregate (paste) the colnames into a single row with stats for each unique 
    #nlType+nlPeriod converted to a single field
    nlCtryHdr <- stats::aggregate(V4 ~ V1 + V2 + V3, data=nlCtryHdr, FUN=paste, collapse=",")
    
    #add a ctryCode column
    nlCtryHdr <- cbind(rep(ctryCode, nrow(nlCtryHdr)), rep(admLevel, nrow(nlCtryHdr)), nlCtryHdr)
    
    #combine into one table
    dataList <- rbind(dataList, nlCtryHdr)
  }
  
  if(is.null(dataList))
    return(NULL)
  
  #convert into a dataframe with numbered rownames
  dataList <- as.data.frame(dataList, row.names = 1:nrow(dataList))
  
  #label the columns
  names(dataList) <- c("ctryCode", "admLevel", "dataType", "nlType", "nlPeriod", "nlStats")

  dataList$ctryCode <- as.character(dataList$ctryCode)
  dataList$admLevel <- as.character(dataList$admLevel)
  dataList$nlPeriod <- as.character(dataList$nlPeriod)
    
  #filters
  #filter by ctryCode if supplied
  if(!is.null(ctryCodes))
    dataList <- dataList[which(dataList[,"ctryCode"] %in% ctryCodes),]
  
  if(!is.null(admLevels))
    dataList <- dataList[which(dataList[,"admLevel"] %in% admLevels),]
  
  #filter by nlType if supplied
  if(!is.null(nlTypes))
    dataList <- dataList[which(dataList[,"nlType"] %in% nlTypes),]
  
  #filter by nlPeriod if supplied
  if(!is.null(nlPeriods))
    dataList <- dataList[which(dataList[,"nlPeriod"] %in% nlPeriods),]
  
  #Reorder the columns
  dataList <- dplyr::select(dataList, dataType, ctryCode, admLevel, nlType, nlPeriod, dplyr::contains("stat"))
  
  #only return dataList if we have records esp. after filtering else return NULL
  if(nrow(dataList) > 0)
    return(dataList)
  else
    return(NULL)
}

######################## listCtryNlRasters ###################################

#' List available cropped country rasters
#'
#' List available data. If source is "local" it lists data cached locally.
#'     If source is remote lists available data on the remote repository.
#' 
#' @param ctryCodes  A character vector of ctryCodes to filter by
#' 
#' @param nlPeriods A character vector of nlPeriods to filter by
#' 
#' @param nlTypes A character vector of nlTypes to filter by
#' 
#' @param source Character string. Whether to check data availability
#'  "local" or "remote". Default is "local".
#' 
#' @return a list of existing cropped rasters
#'
#' @examples
#' #list all rasters
#' listCtryNlRasters()
#' 
#' #list all rasters available for KEN
#' listCtryNlRasters(ctryCodes = "KEN")
#' 
#' #list all VIIRS rasters available for ECU
#' listCtryNlRasters(ctryCodes = "ECU", nlTypes = "VIIRS")
#' 
#' #list available OLS rasters for KEN and RWA in 2012 & 2013
#' listCtryNlRasters(ctryCodes = c("KEN","RWA"), nlPeriods = c("2012", "2013"), nlTypes = "OLS.Y")
#'
#' @export
listCtryNlRasters <- function(ctryCodes=NULL, nlPeriods=NULL, nlTypes=NULL, source="local")
{
  ctryCode <- NULL #appease CRAN note for global variables
  nlType <- NULL #appease CRAN note for global variables
  nlPeriod <- NULL #appease CRAN note for global variables
  
  #get a list of country data files present
  rasterList <- list.files(getNlDir("dirRasterOutput"), pattern = "NL_.*\\.tif$")

  if(length(rasterList) == 0)
    return(NULL)
  
  rasterList <- strsplit(gsub(".tif", "", rasterList), "_")
  
  rasterList <- t(unlist(sapply(rasterList, rbind)))
  
  #convert into a dataframe with numbered rownames
  rasterList <- as.data.frame(rasterList)
  
  #label the columns
  names(rasterList) <- c("ctryCode", "nlType", "nlPeriod")
  
  #filters
  #filter by ctryCode if supplied
  if(!is.null(ctryCodes))
    rasterList <- rasterList[which(rasterList[,"ctryCode"] %in% ctryCodes),]
  
  #filter by nlType if supplied
  if(!is.null(nlTypes))
    rasterList <- rasterList[which(rasterList[,"nlType"] %in% nlTypes),]
  
  #filter by nlPeriod if supplied
  if(!is.null(nlPeriods))
    rasterList <- rasterList[which(rasterList[,"nlPeriod"] %in% nlPeriods),]
  
  #Reorder the columns
  rasterList <- dplyr::select(rasterList, ctryCode, nlType, nlPeriod)
  
  #only return dataList if we have records esp. after filtering else return NULL
  if(nrow(rasterList) > 0)
    return(rasterList)
  else
    return(NULL)
}

######################## listNlTiles ###################################

#' List locally cached tiles
#'
#' List the tiles which have been downloaded previously and are currently
#'     cached in the local tiles folder
#' 
#' @param nlPeriods A character vector of nlPeriods to filter by
#' 
#' @param nlTypes A character vector of nlTypes to filter by
#' 
#' @param source Character string. Whether to check data availability.
#'     "local" or "remote". Default is "local".
#' 
#' @return a list of locally cached nlTiles or NULL
#'
#' @examples
#' #list all tiles
#' listNlTiles()
#' 
#' #list all VIIRS tiles
#' listNlTiles(nlTypes = "VIIRS")
#' 
#' #list all VIIRS tiles available in the years 2014-2015. Note VIIRS data
#' #starts in 201401
#' listNlTiles(nlTypes = "VIIRS.M", nlPeriods = nlRange("201401", "201512"))
#' 
#' #filter data
#' listNlTiles(nlTypes = "OLS.Y", nlPeriods = c("2012", "2013"))
#'
#' @export
listNlTiles <- function(nlTypes=NULL, nlPeriods=NULL, source="local")
{
  nlType <- NULL #appease CRAN note for global variables
  nlPeriod <- NULL #appease CRAN note for global variables
  tileName <- NULL #appease CRAN note for global variables
  
  if(source=="remote")
  {
    message("Not yet implemented. Please post a comment on the github page to fasttrack this feature.")
    
    return(NULL)
  }
  
  #get a list of country data files present
  rasterList <- list.files(getNlDir("dirNlTiles"), pattern = "\\..*\\.tif$")
  
  if(length(rasterList) == 0)
    return(NULL)
  
  rasterList <- strsplit(gsub(".tif", "", rasterList), "_")
  
  rasterList <- t(unlist(sapply(rasterList, rbind)))
  
  #convert into a dataframe with numbered rownames
  rasterList <- as.data.frame(rasterList)
  
  #label the columns
  names(rasterList) <- c("nlType", "nlPeriod", "tileName")
  
  #filters
  #filter by nlType if supplied
  if(!is.null(nlTypes))
    rasterList <- rasterList[which(rasterList[,"nlType"] %in% nlTypes),]
  
  #filter by nlPeriod if supplied
  if(!is.null(nlPeriods))
    rasterList <- rasterList[which(rasterList[,"nlPeriod"] %in% nlPeriods),]
  
  #Reorder the columns
  rasterList <- dplyr::select(rasterList, nlType, nlPeriod, tileName)
  
  #only return list if we have records esp. after filtering else return NULL
  if(nrow(rasterList) > 0)
    return(rasterList)
  else
    return(NULL)
}