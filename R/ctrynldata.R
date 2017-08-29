######################## createCtryNlDataDF ###################################

#' Initiates the country nightlight dataframe with the country data read from the polygon
#'
#' Initiates the country nightlight dataframe with the country data read from the polygon. 
#'     This includes admin levels, level names and area
#'
#' @param ctryCode the ISO3 code of the country
#'
#' @return dataframe with the country admin level data
#'
#' @examples
#' \dontrun{initCtryNlData <- createCtryNlDataDF("KEN")}
#'  #returns a data frame
#'
createCtryNlDataDF <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  if(dir.exists(getPolyFnamePath(ctryCode)) && length(dir(getPolyFnamePath(ctryCode)))> 0)
    ctryPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLowestLyrName(ctryCode))
  
  ctryExtent <- raster::extent(ctryPoly)
  
  raster::projection(ctryPoly) <- sp::CRS(wgs84)
  
  #get the list of admin levels in the polygon shapefile
  ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode)
  
  #conver to lower case for consistency
  ctryPolyAdmLevels <- tolower(ctryPolyAdmLevels)
  
  #add the area as reported by the polygon shapefile as a convenience
  #converted to sq. km.
  areas <- raster::area(ctryPoly)/1e6
  
  if (length(ctryPolyAdmLevels) > 0)
  {
    #When a country does not have lower administrative levels
    
    #the number of admin levels
    nLyrs <- length(ctryPolyAdmLevels)
    
    #the repeat pattern required to create columns in the format 1,1,2,2,3,3 ...
    #for col names: admlevel1_id, admlevel1_name, ..., admleveN_id, admlevelN_name
    #and polygon data col names: ID_1, NAME_1, ..., ID_N, NAME_N
    #nums <- c(paste(1:nLyrs,1:nLyrs))
    
    #nums <- unlist(strsplit(paste(nums, collapse = " "), " "))
    
    #ctryPolyAdmCols <- paste(c("ID_", "NAME_"), nums, sep="")
    
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
#' @param nlType the type of nightlight data i.e. "OLS" or "VIIRS"
#'
#' @return Character full path to the cropped VIIRS country raster 
#'     for a country and a given year and month
#'
#' @examples
#' 
#' \dontrun{ctryNlDataDF <- insertNlDataCol(ctryNlDataDF, dataCol, "sum", "201209", "VIIRS")}
#' 
#' \dontrun{ctryNlDataDF <- insertNlDataCol(ctryNlDataDF, dataCol, "mean", "2012", "OLS")}
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
  names(ctryNlDataDF)[ncol(ctryNlDataDF)] <- getCtryNlDataColName(nlPeriod = nlPeriod, stat = statType, nlType = nlType)
  
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
#' @return None
#'
#' @examples
#' 
#' \dontrun{saveCtryNlData(ctryNlDataDF, ctryCode)}
#'
saveCtryNlData <- function(ctryNlDataDF, ctryCode)
{
  if(missing(ctryNlDataDF))
    stop("Missing required parameter ctryNlDataDF")
  
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validCtryNlDataDF(ctryNlDataDF))
    stop("Invalid country dataframe")
  
  utils::write.table(ctryNlDataDF, getCtryNlDataFnamePath(ctryCode), row.names= F, sep = ",")
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
#' \dontrun{validCtryNlDataDF(nlCtryDataDF)}
#'  #returns TRUE
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

#' Check if a month number is valid for a given nightlight type
#'
#' Get the name of the data file. This function can be altered to name the file as 
#'     required and consistently retrieve the name. Used in the function getCtryNlDataFnamePath 
#'     to concat the directory path and this filename. Currently all nlTypes are stored in one 
#'     file. Can be altered to separate VIIRS and OLS data files for example.
#'
#' @param ctryCode The ctryCode of interest
#'
#' @return Character filename of the country data file
#'
#' @examples
#' ctryCode <- "KEN"
#' \dontrun{getCtryNlDataFname(ctryCode)}
#'  #returns name of the ctry data file
#'
getCtryNlDataFname <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  return (paste0(ctryCode, "_NLData.csv"))
}

######################## getCtryNlDataFnamePath ###################################

#' Get the full path to the file containing the country data
#'
#' Get the full path to the file containing the country data
#' 
#' @param ctryCode character string The ctryCode of interest
#'
#' @return Character string the full path to the data file of a country
#'
#' @examples
#' #get the full path to the file containing data for KEN
#' getCtryNlDataFnamePath("KEN")
#' 
#' \dontrun{
#' ctryDF <- read.csv(getCtryNlDataFnamePath("KEN"))
#'  #returns DF with nightlight data for the country
#'  }
#'
#' #@export only due to exploreData() shiny app
#' @export
getCtryNlDataFnamePath <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  return (file.path(getNlDir("dirNlData"), getCtryNlDataFname(ctryCode)))
}

######################## getCtryNlData ###################################

#' Returns nightlight statistics for the given ctryCode and nlType in the given nPeriods
#'
#' Returns nightlight data for the given ctryCode and stats in the given 
#'     nlPeriods and of the specified nlType. Note that getCtryNldata only
#'     processes one ctryCode at a time.
#'     \code{ignoreMissing} plays a significant role here. It can take 3 values:
#'     
#'     \itemize{
#'         \item NULL (default) only return data if found for all nlPeriods
#'            and all stats provided otherwise return NULL.
#'         \item TRUE return any partial data that is found for the provided 
#'            nlPeriods and stats. Ignore any missing data.
#'         \item FALSE return all data that is found and call \code{processNlData}
#'            to download and process any missing nlPeriods and stats.
#'     }
#'    
#'    Farther, if \code{nlPeriods} is missing, it is assigned values based on
#'    the value of ignoreMissing. If ignoreMissing is FALSE, nlPeriods is 
#'    assigned all existing nlPeriods to date. This is the equivalent of 
#'    retrieving all nightlight data for the given country and stats. If 
#'    ignoreMissing is TRUE or NULL then the existing data is returned.
#'
#' @param ctryCode the ISO3 code of the country. Only 1 country can be 
#'     processed at a time
#'
#' @param nlPeriods a vector of nlPeriods. Must be appropriate nlPeriods
#'     for the nlType.
#' 
#' @param stats a vector of stats. if not supplied defaults to all stats
#'     as listed in pkgOptions("stats")
#' 
#' @param nlType the nightlight type i.e. "OLS" or "VIIRS" (default)
#' 
#' @param ignoreMissing controls how the function behaves if any data is not
#'     found in the data file
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
#' #missing stats implies all stats as given by pkgOptions("stats")
#' 
#' \dontrun{getCtryNlData("KEN", nlType="VIIRS", ignoreMissing=NULL)}
#'     #returns all existing data i.e. all nlPeriods and all stats for KEN
#'
#' \dontrun{getCtryNlData("KEN", ignoreMissing=TRUE)}
#'     #same as ignoreMissing=NULL. Returns all existing data i.e. all nlPeriods
#'     #and all stats for KEN
#'  
#' \dontrun{getCtryNlData(ctryCode="KEN", nlType="VIIRS", ignoreMissing=FALSE)}
#'     #for any missing data between 201204 to present download and process the
#'     #data then return all data
#'  
#' \dontrun{getCtryNlData("KEN", nlPeriod=c("existingNlPeriod", "missingNlPeriod"),
#'     stats=c("sum", "unknownStat"), ignoreMissing=NULL)}
#'     #Returns NULL
#'     #(ignoreMissing=NULL returns all data if exists or if any is missing returns NULL)
#'
#' \dontrun{getCtryNlData("KEN", nlPeriods=c("existingNlPeriod", "missingNlPeriod"),
#'     stats=c("existingStat", "missingStat"), ignoreMissing=TRUE)}
#'    #Returns existingStat for existingNlPeriods
#'    #(ignoreMissing=TRUE returns only existing data)
#'  
#' \dontrun{getCtryNlData("KEN", nlYearPeriods=c("existingNlPeriod", "missingNlPeriod"),
#'     stats=c("sum", "unknownStat"), ignoreMissing=FALSE)}
#'     #Runs processNlData for missingStat in "missingNlPeriod" and returns
#'     #"existingStat" and "missingStat" for both "existingNlPeriod" and
#'     #"missingNlPeriod"
#'     #(ignoreMissing=FALSE must return all data: forces processing of any missing)
#'  
#' @export
getCtryNlData <- function(ctryCode, nlPeriods, nlType, stats=pkgOptions("stats"), ignoreMissing=NULL, source="local")
{
  if(source != "local")
    stop("Non-local sources not currently supported. \n
         Please request this feature at https://github.com/chrisvwn/Rnightlights to fasttrack it.")
  
  if(missing(ctryCode))
    stop("Missing required ctryCode")

  #if both nlPeriods and ignoreMissing are not supplied we cannot deduce
  #the nlPeriods. Error and stop
  if(missing(nlPeriods) && missing(ignoreMissing))
    stop("Missing required parameter nlPeriods")
    
  #if nlPeriods is not provided and ignoreMissing is present and FALSE
  #process all nlPeriods
  if(missing(nlPeriods) && !missing(ignoreMissing))
    if (!ignoreMissing)
      nlPeriods <- getAllNlPeriods(nlType)
  
  #else
  #  stop("No data found for given nlPeriods.\n
  #       Set ignoreMissing=FALSE to process all nlPeriods")

  if(missing(nlType))
    stop("Missing required parameter nlType")
    
  if(!allValid(nlPeriods, validNlPeriod, nlType))
    stop("Invalid nlPeriods detected")
  
  #if(missing(ignoreMissing))
  #  ignoreMissing = TRUE
  
  if(length(ctryCode) > 1)
    stop("getCtryNlData can only process 1 ctryCode at a time")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode", ctryCode)
    
  if(!is.null(ignoreMissing))
    if(ignoreMissing && !existsCtryNlDataFile(ctryCode))
      stop("No data exists for ", ctryCode, ". Set IgnoreMissing= to download and process")
  
  if (!missing(nlPeriods)) #if nlPeriods is provided process else return all ctry data
  {
    #check if the stats exist in the given year months will test nlYm1+stat1, nlYm2+stat1, ..., nlYm1+stat2, nlYm2+stat2
    nlPeriodStats <- expand.grid(nlPeriods, stats)
    
    existnlPeriodStats <- apply(nlPeriodStats, 1, function(x) existsCtryNlData(ctryCode, x[1], x[2], nlType))
    
    missingData <- paste0(apply(nlPeriodStats[!existnlPeriodStats,], 1, function(x)paste0(x[1], ":", x[2])), collapse = ", ")
    
    if (!all(existnlPeriodStats))
    {
      if (is.null(ignoreMissing)) #default
      {
        message(paste0("No data found for ", ctryCode, " in ", missingData,
                       ". Returning NULL. \nNote: Set ignoreMissing=TRUE to",
                       "return only data found or \nignoreMissing=FALSE to download ",
                       "and extract missing data"))
        return (NULL)
      }
      else if(!ignoreMissing)
      {
        message(paste0("Processing missing data: ", ctryCode, " in ", missingData, 
                ". This may take a while. \nNote: Set 'ignoreMissing=TRUE' to ",
                "return only data found or \n'ignoreMissing=NULL' to return NULL ",
                "if not all the data is found"))
        
        processNlData(ctryCode, nlType = nlType, nlPeriods, stats = stats)
      }
      else if (ignoreMissing)
      {
        message(paste0("Ignoring missing data for ", ctryCode, " in ", missingData, 
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
    if(existsCtryNlDataFile(ctryCode))
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
      existingCols <- apply(nlPeriodStats[existnlPeriodStats,], 1, function(x) getCtryNlDataColName(x[1], x[2], nlType))
    else
      existingCols <- NULL
  }
  else 
  {
    #ignoreMissing == FALSE so we should have the missing data
    #check again to see that processNlData was successful
    #check that each stat exists for given periods
    if (nlType == "VIIRS")
      existnlPeriodStats <- apply(nlPeriodStats, 1, function(x) existsCtryNlData(ctryCode, x[1], x[2], nlType))
    else if (nlType == "OLS")
      existnlPeriodStats <- apply(nlPeriodStats, 1, function(x) existsCtryNlData(ctryCode, x[1], x[2], nlType))
    
    #if they all exist get the list of column names
    if(all(existnlPeriodStats))
      existingCols <- apply(nlPeriodStats[existnlPeriodStats,], 1, function(x) getCtryNlDataColName(x[1], x[2], nlType))
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
  
  ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode)))
  
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
#' @param stat character vector The stat to be stored in the column
#' 
#' @param nlType character vector The type of nightlight i.e. "OLS" or VIIRS. Default=VIIRS
#'
#' @return character string
#'
#' @examples
#' \dontrun{
#' ctryCode <- "KEN"
#' dt <- read.csv(getCtryNlDataFnamePath(ctryCode))
#' dt <- dt[,getCtryNlDataColName("201612", "sum", nlType="VIIRS")]
#'   #returns the column "NL_201612_SUM" if it exists in the KEN data file
#'   }
getCtryNlDataColName <- function(nlPeriod, stat, nlType)
{
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(missing(stat))
    stop("Missing required parameter stat")
  
  if(missing(nlType))
    stop("Missing required parameter nlType")
  
  if(!validNlPeriod(nlPeriod, nlType))
    stop("Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if (!allValid(stat, validStat))
    stop("Invalid/unsupported stat detected")
  
  colName <- "NL_"
  
  if (nlType=="VIIRS")
    colName <- paste0(colName, "VIIRS_")
  else if(nlType == "OLS")
    colName <- paste0(colName, "OLS_")

  colName <- paste0(colName, sapply(nlPeriod, function(x) paste0(x, "_", toupper(stat))))
  
  colName <- sort(colName)
  
  return(colName)
}

######################## existsCtryNlDataFile ###################################

#' Check if a country's data file exists
#'
#' Check if a country's data file exists
#'
#' @param ctryCode the ISO3 country code
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{
#' ctryCode <- "KEN"
#' if(existsCtryNlDataFile(ctryCode))
#'  message("Data file for ", ctryCode, " found")
#'  }
#'
existsCtryNlDataFile <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid/Unknown ctryCode: ", ctryCode)
  
  #for polygons look for shapefile dir
  return(file.exists(getCtryNlDataFnamePath(ctryCode)))
}

######################## existsCtryNlData ###################################

#' Check if VIIRS nightlight stats exist locally
#'
#' Check if VIIRS nightlight data for the country exists in the country 
#'     nightlight data file. First checks if the country nightlight data
#'     file exists.
#'
#' @param ctryCode character the ISO3 code of the country
#'
#' @param nlPeriod character the nlPeriod
#' 
#' @param stat character the stat to check for
#' 
#' @param nlType character the nlType
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{existsCtryNlData("KEN", "201204", "sum", "VIIRS")}
#'
existsCtryNlData <- function(ctryCode, nlPeriod, stat, nlType)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(missing(stat))
    stop("Missing required parameter stat")
  
  if(missing(nlType))
    stop("Missing required parameter nlType")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validNlPeriod(nlPeriod, nlType))
    stop("Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if(!validStat(stat))
    stop("Invalid stat: ", stat)
  
  if (!existsCtryNlDataFile(ctryCode))
    return (FALSE)
  
  dt <- utils::read.csv(getCtryNlDataFnamePath(ctryCode), nrow=1, header=TRUE)
  
  hd <- names(dt)
  
  if (length(grep(getCtryNlDataColName(nlPeriod, stat = stat, nlType), hd)) > 0)
    return(TRUE)
  else
    return(FALSE)
}

######################## listCtryNlData ###################################

#' List available data
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
#' #list all VIIRS data available for ECU
#' listCtryNlData(ctryCodes = "ECU", nlTypes = "VIIRS")
#' 
#' #list available OLS data for KEN and RWA in 2012 & 2013
#' listCtryNlData(ctryCodes = c("KEN","RWA"), nlPeriods = c("2012", "2013"), nlTypes = "OLS")
#'
#' @export
listCtryNlData <- function(ctryCodes=NULL, nlPeriods=NULL, nlTypes=NULL, source="local")
{
  dataList <- NULL
  dataType <- NULL #appease CRAN note for global variables
  nlType <- NULL #appease CRAN note for global variables
  nlPeriod <- NULL #appease CRAN note for global variables
  
  #get a list of country data files present
  countries <- list.files(getNlDir("dirNlData"))
  
  #for each country filename
  for (ctry in countries)
  {
    #get first 3 chars which gives the ctryCode
    ctryCode <- substr(ctry, 1, 3)
    
    #read in the header row
    ctryHdr <- data.table::fread(file.path(getNlDir("dirNlData"), ctry), header = F, nrows = 1)
    
    #grab only the NL cols
    nlCtryHdr <- grep("^NL", ctryHdr, value = T)
    
    #split the NL colnames into their components e.g. "NL_OLS_2012_MEAN"
    #= "NL"+nlType+nlPeriod+stat
    nlCtryHdr <- reshape2::colsplit(nlCtryHdr, "_", c("V1", "V2","V3","V4"))
    
    #aggregate the colnames into a single row with stats for each unique 
    #nlType+nlPeriod converted to a single field
    nlCtryHdr <- stats::aggregate(V4 ~ V1 + V2 + V3, data=nlCtryHdr, FUN=paste, collapse=",")
    
    #add a ctryCode column
    nlCtryHdr <- cbind(rep(ctryCode, nrow(nlCtryHdr)), nlCtryHdr)
    
    #combine into one table
    dataList <- rbind(dataList, nlCtryHdr)
  }
  
  if(is.null(dataList))
    return(NULL)
  
  #convert into a dataframe with numbered rownames
  dataList <- as.data.frame(dataList, row.names = 1:nrow(dataList))
  
  #label the columns
  names(dataList) <- c("ctryCode", "dataType", "nlType", "nlPeriod", paste0("stat", 1:(ncol(dataList)-4)))
  
  #filters
  #filter by ctryCode if supplied
  if(!is.null(ctryCodes))
    dataList <- dataList[which(dataList[,"ctryCode"] %in% ctryCodes),]
  
  #filter by nlType if supplied
  if(!is.null(nlTypes))
    dataList <- dataList[which(dataList[,"nlType"] %in% nlTypes),]
  
  #filter by nlPeriod if supplied
  if(!is.null(nlPeriods))
    dataList <- dataList[which(dataList[,"nlPeriod"] %in% nlPeriods),]
  
  #Reorder the columns
  dataList <- dplyr::select(dataList, dataType, ctryCode, nlType, nlPeriod, dplyr::contains("stat"))
  
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
#' listCtryNlData(ctryCodes = "KEN")
#' 
#' #list all VIIRS rasters available for ECU
#' listCtryNlData(ctryCodes = "ECU", nlTypes = "VIIRS")
#' 
#' #list available OLS rasters for KEN and RWA in 2012 & 2013
#' listCtryNlData(ctryCodes = c("KEN","RWA"), nlPeriods = c("2012", "2013"), nlTypes = "OLS")
#'
#' @export
listCtryNlRasters <- function(ctryCodes=NULL, nlPeriods=NULL, nlTypes=NULL, source="local")
{
  ctryCode <- NULL #appease CRAN note for global variables
  nlType <- NULL #appease CRAN note for global variables
  nlPeriod <- NULL #appease CRAN note for global variables
  
  #get a list of country data files present
  rasterList <- list.files(getNlDir("dirRasterOutput"), pattern = ".tif")

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
#' #list all VIIRS tiles available in the years 2012-2014. Note VIIRS data
#' #starts in 201204
#' listNlTiles(nlTypes = "VIIRS", nlPeriods = nlRange("201204", "201412"))
#' 
#' #filter data
#' listNlTiles(nlTypes = "OLS", nlPeriods = c("2012", "2013"))
#'
#' @export
listNlTiles <- function(nlTypes=NULL, nlPeriods=NULL, source="local")
{
  nlType <- NULL #appease CRAN note for global variables
  nlPeriod <- NULL #appease CRAN note for global variables
  tileName <- NULL #appease CRAN note for global variables
  
  if(source=="remote")
  {
    message("Not yet implemented. Please notify us to fasttrack this feature.")
    
    return(NULL)
  }
  
  #get a list of country data files present
  rasterList <- list.files(getNlDir("dirNlTiles"), pattern = ".tif$")
  
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