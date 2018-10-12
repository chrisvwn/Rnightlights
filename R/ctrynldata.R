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
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
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
createCtryNlDataDF <- function(ctryCode=NULL, admLevel, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    admLevel=getCtryShpLowestLyrNames(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  ctryPoly <- readCtryPolyAdmLayer(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(is.null(ctryPoly))
    stop(Sys.time(), ": Could not read admLevel '", admLevel, "' from given polygon")
  
  ctryExtent <- raster::extent(ctryPoly)
  
  raster::projection(ctryPoly) <- sp::CRS(wgs84)
  
  #get the list of admin levels in the polygon shapefile
  ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode = ctryCode, lowestAdmLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  #convert to lower case for consistency
  ctryPolyAdmLevels <- tolower(ctryPolyAdmLevels)
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  if(is.null(custPolyPath))
  {
    if (length(ctryPolyAdmLevels) > 1)
    {
      #When a country does not have lower administrative levels
      
      #the number of admin levels
      nLyrs <- length(ctryPolyAdmLevels)
      
      ctryPolyAdmCols <- paste(c("NAME_"), 0:(nLyrs-1), sep="")
      
      #pull the ID_ and NAME_ cols from layer1 to lowest layer (layer0 has country code not req'd)
      ctryNlDataDF <- as.data.frame(ctryPoly@data[,eval(ctryPolyAdmCols)])
      
      #add the area as reported by the polygon shapefile as a convenience
      areas <- raster::area(ctryPoly)/1e6
      
      #we add the country code to ensure even a renamed file is identifiable
      #repeat ctryCode for each row in the polygon. equiv of picking layer0
      ctryCodeCol <- rep(ctryCode, nrow(ctryNlDataDF))
      
      #combine the columns
      ctryNlDataDF <- cbind(ctryCodeCol, ctryNlDataDF, areas)
      
      #remove the NAME_0 column which has the country name. We want ctryCode only
      ctryNlDataDF <- ctryNlDataDF[,-which(names(ctryNlDataDF)=="NAME_0")]
      
      ctryPolyColNames <- ctryPolyAdmLevels
      
      #add the country code and area columns to the dataframe
      ctryPolyColNames <- c(ctryPolyColNames, "area_sq_km")
      
      names(ctryNlDataDF) <- ctryPolyColNames
    } else
    {
      #add the area as reported by the polygon shapefile as a convenience
      areas <- raster::area(ctryPoly)/1e6
      
      ctryNlDataDF <- data.frame("country"=ctryCode, "area_sq_km"=areas)
    }
  } else
  {
    #custpolyPath
    polyFnamePath <- getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    
    lyrNames <- rgdal::ogrListLayers(polyFnamePath)
    
    polyColNames <- sapply(lyrNames, function(lyrName) 
    {
      ctryPoly <- rgdal::readOGR(dsn = polyFnamePath, layer = lyrName)
      
      colNames <- names(ctryPoly@data)
      
      uniqueColVals <- nrow(ctryPoly@data)/sapply(colNames, function(x) length(unique(ctryPoly@data[[x]])))
      
      intColVals <-  sapply(colNames, function(x) length(grep("^\\d+$", ctryPoly@data[[x]])))
      
      possibleColNames <- intersect(names(uniqueColVals[which(uniqueColVals < 1.1)]), names(intColVals[which(intColVals == intColVals[which.min(intColVals)])]))
      
      if(length(possibleColNames) == 1)
        return(possibleColNames)
      
      missingColVals <- sapply(possibleColNames, function(x) sum(is.na(ctryPoly@data[[x]]) | ctryPoly@data[[x]] == ""))
      
      possibleColNames <- intersect(possibleColNames, names(missingColVals[which(missingColVals == missingColVals[which.min(missingColVals)])]))
      
      if(length(possibleColNames) == 1)
        return(possibleColNames)
      
      partIntColVals <-  sapply(possibleColNames, function(x) length(grep("\\d+", ctryPoly@data[[x]])))
      
      possibleColNames <- intersect(possibleColNames, names(partIntColVals[which(partIntColVals == partIntColVals[which.min(partIntColVals)])]))
      
      if(length(possibleColNames) == 1)
        return(possibleColNames)
      
      lyrNameParts <- unlist(strsplit(gsub("^\\d+_", "", lyrName), "[^[:alnum:]]"))
      
      partialMatchColNames <- unlist(sapply(lyrNameParts, function(x) grep(x, colNames, ignore.case = T, value = T)))
      
      a <- intersect(possibleColNames, partialMatchColNames)
      
      if(length(a) == 1)
        return(a)
      
      if(length(a) > 1)
        possibleColNames <- a
      
      return(possibleColNames[1])
      
    })
    
    #the number of admin levels
    nLyrs <- length(ctryPolyAdmLevels)
    
    polyColNames <- polyColNames[1:nLyrs]
    
    ctryPoly <- readCtryPolyAdmLayer(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    
    ctryNlDataDF <- as.data.frame(ctryPoly@data[, polyColNames], stringsAsFactors=F)
    
    #add the area as reported by the polygon shapefile as a convenience
    areas <- raster::area(ctryPoly)/1e6
    
    #combine the columns
    ctryNlDataDF <- cbind(ctryNlDataDF, areas)
    
    names(ctryNlDataDF) <- c(lyrNames[1:nLyrs], "area_sq_km")
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
    stop(Sys.time(), ": Missing required parameter ctryNlDataDF")
  
  if(missing(dataCol))
    stop(Sys.time(), ": Missing required parameter dataCol")
  
  if(missing(statType))
    stop(Sys.time(), ": Missing required parameter statType")
  
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  
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
#' @param admLevel admLevel to process
#' 
#' @param nlType  the type of nightlight data
#' 
#' @param nlPeriod the nlPeriod that the dataCol belongs to
#' 
#' @param statType the stat which produced the dataCol vector
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
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
deleteNlDataCol <- function (ctryCode=NULL, admLevel, nlType, nlPeriod, statType, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(statType))
    stop(Sys.time(), ": Missing required parameter statType")
  
  ctryNlDataDF <- suppressMessages(getCtryNlData(ctryCode = ctryCode,
                                admLevel = admLevel,
                                nlTypes = nlType,
                                nlPeriods = getAllNlPeriods(nlTypes = nlType), 
                                ignoreMissing = TRUE,
                                gadmVersion = gadmVersion,
                                custPolyPath = custPolyPath))
  
  #read in all column names in the dataframe
  cols <- names(ctryNlDataDF)
  
  colName <- paste("NL", nlType, nlPeriod, toupper(statType), sep = "_")
  
  #get only the named nightlight data column(s)
  nlDataColIdx <- grep(colName, cols)
  
  if(length(nlDataColIdx) == 0)
    stop(Sys.time(), ": Specified column not found")
  
  #write back the dataframe with the new column order
  ctryNlDataDF <- ctryNlDataDF[ , -c(nlDataColIdx)]
  
  saveCtryNlData(ctryNlDataDF = ctryNlDataDF, ctryCode = ctryCode, admLevel = searchAdmLevel(ctryCodes = ctryCode,admLevelNames = admLevel,gadmVersion = gadmVersion, custPolyPath = custPolyPath), gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
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
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return None
#'
#' @examples
#' 
#' \dontrun{
#' Rnightlights:::saveCtryNlData(ctryNlDataDF, ctryCode)
#' }
#'
saveCtryNlData <- function(ctryNlDataDF, ctryCode=NULL, admLevel, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryNlDataDF))
    stop(Sys.time(), ": Missing required parameter ctryNlDataDF")
  
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    stop(Sys.time(), ": Missing required parameter admLevel")
  
  if(!validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)
  
  if(!validCtryNlDataDF(ctryNlDataDF))
    stop(Sys.time(), ": Invalid country dataframe")
  
  utils::write.table(x = ctryNlDataDF, file = getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath), row.names = F, sep = ",")
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
    stop(Sys.time(), ": Missing required parameter ctryNlDataDF")
  
  if(class(ctryNlDataDF) == "data.frame" && !is.null(ctryNlDataDF) && any(grepl("area_sq_km", names(ctryNlDataDF))))
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
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return Character filename of the country data file
#'
#' @examples
#' ctryCode <- "KEN"
#' admLevel <- "KEN_adm0"
#' Rnightlights:::getCtryNlDataFname(ctryCode, admLevel)
#' #returns string of name of the ctry data file
#'
getCtryNlDataFname <- function(ctryCode=NULL, admLevel, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    stop(Sys.time(), ": Missing required parameter admLevel")
  
  if(!is.null(ctryCode) && ctryCode == "")
    ctryCode <- NULL

  #if we have both ctryCode and custPolyPath add ctryCode and admLevel to admLevel
  if(!is.null(custPolyPath) && !is.null(ctryCode)) 
    admLevel <- paste0(ctryCode, "_", admLevel)
  
  if(grepl("gadm36_[A-Z]{3,4}_\\d", admLevel))
    lyrName <- paste0("ADM", stringr::str_extract(admLevel, "\\d+$"))
  else if(grepl("[A-Z]{3,4}_adm\\d", admLevel))
    lyrName <- paste0("ADM", stringr::str_extract(admLevel, "\\d+$"))
  else
    lyrName <- admLevel
  
  polyVer <- ifelse(is.null(custPolyPath), paste0("GADM-", gadmVersion), paste0("CUST-", basename(custPolyPath)))
  
  fName <- paste0(paste("NL", "DATA", toupper(ctryCode), toupper(lyrName), polyVer, sep="_"), ".csv")
  
  #shortcut remove consecutive underscores caused by empty admLevel
  fName <- gsub("_{2,}", "_", fName)
  
  return (fName)
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
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
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
getCtryNlDataFnamePath <- function(ctryCode=NULL, admLevel, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    stop(Sys.time(), ": Missing required parameter admLevel")
  
  if(is.null(custPolyPath) && !validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)
  
  return (file.path(getNlDir(dirName = "dirNlData"), getCtryNlDataFname(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
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
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @param downloadMethod The method used to download polygons
#' 
#' @param cropMaskMethod The method used to crop and mask the satellite raster
#' 
#' @param extractMethod The method used to extract data and perform calculations
#'     on the satellite raster
#' 
#' @param source "local" or "remote" Whether to download and process the
#'     data locally or to download the pre-processed data from a remote 
#'     source/repo
#'
#' @param ... other arguments
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
getCtryNlData <- function(ctryCode=NULL, admLevel, nlTypes, nlPeriods, nlStats=pkgOptions("nlStats"), ignoreMissing=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL, downloadMethod=pkgOptions("downloadMethod"), cropMaskMethod=pkgOptions("cropMaskMethod"), extractMethod=pkgOptions("extractMethod"), source="local", ...)
{
  if(source != "local")
    stop(Sys.time(), ": Non-local sources not currently supported. \n
         Please request this feature at https://github.com/chrisvwn/Rnightlights to fasttrack it.")
  
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    admLevel <- "top"
  
  if(missing(nlTypes))
    stop(Sys.time(), ": Missing required parameter nlTypes")
  
  #if both nlPeriods and ignoreMissing are not supplied we cannot deduce
  #the nlPeriods. Error and stop
  if(missing(nlPeriods) && missing(ignoreMissing))
    stop(Sys.time(), ": Missing required parameter nlPeriods")
  
  #if nlPeriods is not provided and ignoreMissing is present and FALSE
  #process all nlPeriods
  if(missing(nlPeriods) && !missing(ignoreMissing))
    if (!ignoreMissing)
      nlPeriods <- getAllNlPeriods(nlTypes = nlTypes)
    
    #if(!allValid(nlPeriods, validNlPeriods, nlType))
    if(!allValidNlPeriods(nlTypes = nlTypes, nlPeriods = nlPeriods))
      stop(Sys.time(), ": Invalid nlPeriods detected")
    
    if(any(dupNlStats <- duplicated(nlStats)))
    {
      message(Sys.time(), ": duplicate nlStat(s) detected: ", nlStats[dupNlStats], ". Continuing with unique nlStats")
      nlStats <- unique(nlStats)
    }
    
    validStats <- validNlStats(nlStats)
    if(!all(validStats))
      stop(Sys.time(), ": Invalid nlStats detected ", as.character(nlStats[!validStats]))
    
    #if(missing(ignoreMissing))
    #  ignoreMissing = TRUE
    
    if(length(ctryCode) > 1 || length(admLevel) > 1)
      stop(Sys.time(), ": getCtryNlData can only process 1 ctryCode & 1 admLevel at a time")
    
    #if(!validCtryCodes(ctryCode))
    #  stop(Sys.time(), ": Invalid ctryCode", ctryCode)
    
    if(grepl("country", admLevel))
      admLevel <- getCtryShpLyrNames(ctryCodes = ctryCode, lyrNums = 0, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    else if(admLevel %in% c("bottom", "lowest"))
      admLevel <- getCtryShpLowestLyrNames(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    else if(admLevel %in% c("top","highest"))
      admLevel <- getCtryShpLyrNames(ctryCodes = ctryCode, lyrNums = 1, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    else if(admLevel=="all")
      admLevel <- getCtryShpAllAdmLvls(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    else
    {
      tmpAdmLevel <- searchAdmLevel(ctryCodes = ctryCode, admLevelNames = admLevel, downloadMethod = downloadMethod, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      
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
    
    if(!allValidCtryAdmLvls(ctryCode = ctryCode, admLevels = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
      stop(Sys.time(), ": Invalid admLevels detected")
    
    if(!is.null(ignoreMissing))
      if(ignoreMissing && !existsCtryNlDataFile(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
        stop(Sys.time(), ": No data exists for ", ctryCode, ". Set IgnoreMissing=FALSE to download and process")
    
    if (!missing(nlPeriods)) #if nlPeriods is provided process else return all ctry data
    {
      #check if the stats exist in the given year months will test nlYm1+stat1, nlYm2+stat1, ..., nlYm1+stat2, nlYm2+stat2
      if(is.list(nlPeriods))
        a <- lapply(1:length(nlTypes), function(i) cbind(nlTypes[i], nlPeriods[[i]]))
      else
        a <- lapply(1:length(nlTypes), function(i) cbind(nlTypes[i], nlPeriods))
      
      a <- data.frame(do.call("rbind", a), stringsAsFactors = F)
      
      if(is.list(nlStats) && length(nlStats) > 1 && all(sapply(2:length(nlStats), function(i) !is.list(nlStats[[i]]) && (grepl("=", nlStats[i]) || length(names(nlStats[i])) > 0))))
        nlStats <- list(nlStats)
      
      nlStatNames <- sapply(nlStats, function(x) x[[1]])
      
      if(length(nlStatNames) == 1)
        nlPeriodStats <- data.frame(a, X3=nlStatNames, stringsAsFactors = F)
      else
        nlPeriodStats <- data.frame(apply(X = a,
                                          MARGIN = 2,
                                          FUN = function(x) rep(x, length(nlStatNames))),
                                    X3=as.vector(sapply(nlStatNames, rep, nrow(a))),
                                    stringsAsFactors = F)
      
      #nlPeriodStats <- nlPeriodStats[order(nlPeriodStats$X1, nlPeriodStats$X2),]
      
      existnlPeriodStats <- apply(X = nlPeriodStats,
                                  MARGIN = 1,
                                  FUN = function(x) existsCtryNlData(ctryCode = ctryCode,
                                                                     admLevel = admLevel,
                                                                     nlTypes =  x[1],
                                                                     nlPeriods = x[2],
                                                                     nlStats = as.character(x[3]),
                                                                     gadmVersion = gadmVersion,
                                                                     custPolyPath = custPolyPath))
      
      missingData <- paste0(apply(X = nlPeriodStats[!existnlPeriodStats,],
                                  MARGIN = 1,
                                  FUN = function(x) paste0(x[1], ":", x[2], ":", x[3])),
                            collapse = ", ")
      
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
          
          processNlData(ctryCodes = ctryCode,
                        admLevels = admLevel,
                        nlTypes = nlTypes,
                        nlPeriods = nlPeriods,
                        nlStats = nlStats,
                        gadmVersion = gadmVersion,
                        custPolyPath = custPolyPath,
                        downloadMethod = downloadMethod,
                        cropMaskMethod = cropMaskMethod,
                        extractMethod = extractMethod)
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
      
      #if all nlPeriodStats exist  get the data
      ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
    }
    else
    {
      #else if missing nlPeriods
      #if !missing(stats) return only given stats
      #else return the whole data frame
      if(existsCtryNlDataFile(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath=custPolyPath))
        ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode = ctryCode,
                                                                           admLevel = admLevel,
                                                                           gadmVersion = gadmVersion,
                                                                           custPolyPath = custPolyPath)))
      else
      {
        message(Sys.time(), ": Data for ", ctryCode, " does not exist. Set IgnoreMissing=FALSE to download and process")
        ctryData <- NULL
      }
      
      return(ctryData)
    }
    
    #to remove any missing nlPeriods if ignoreMissing==TRUE
    if(is.null(ignoreMissing) || ignoreMissing) #shortcircuit if ignoreMissing is NULL to avoid crash
    {
      if(any(existnlPeriodStats))
        existingCols <- apply(X = nlPeriodStats[existnlPeriodStats,],
                              MARGIN =  1,
                              FUN = function(x)
                              {
                                getCtryNlDataColName(nlType = x[1], nlPeriod = x[2], nlStat = x[3])
                              })
      else
        existingCols <- NULL
    }
    else 
    {
      #ignoreMissing == FALSE so we should have the missing data
      #check again to see that processNlData was successful
      #check that each stat exists for given periods
      if (stringr::str_detect(nlTypes, "VIIRS"))
        existnlPeriodStats <- apply(X = nlPeriodStats,
                                    MARGIN = 1,
                                    FUN = function(x)
                                    {
                                      existsCtryNlData(ctryCode = ctryCode,
                                                       admLevel = admLevel,
                                                       nlTypes = x[1],
                                                       nlPeriods = x[2],
                                                       nlStats = x[3],
                                                       gadmVersion = gadmVersion,
                                                       custPolyPath = custPolyPath)})
      else if (stringr::str_detect(nlTypes, "OLS"))
        existnlPeriodStats <- apply(X = nlPeriodStats,
                                    MARGIN = 1,
                                    FUN = function(x)
                                    {
                                      existsCtryNlData(ctryCode = ctryCode,
                                                       admLevel = admLevel,
                                                       nlTypes = x[1],
                                                       nlPeriods = x[2],
                                                       nlStats = x[3],
                                                       gadmVersion = gadmVersion,
                                                       custPolyPath = custPolyPath)
                                    })
      
      #if they all exist get the list of column names
      if(all(existnlPeriodStats))
      {
        existingCols <- apply(nlPeriodStats[existnlPeriodStats,], 1, function(x) getCtryNlDataColName(nlPeriod = x[2], nlType = x[1], nlStat = x[3]))
        
        message(Sys.time(), ": All stats exist")
      }
      else #else processNlData was unsuccessful i.e. an error occurred
        stop(Sys.time(), ": An error occurred")
    }
    
    #if no nightlight columns return NULL
    if(length(existingCols) < 1)
    {
      message(Sys.time(), ": No nightlight data. Returning ctry admin data only")
      #return(NULL)
    }
    else #else we found nl data
    {
      message(Sys.time(), ": Retrieving requested data")
    }
    
    # ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion)))
    
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
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(nlStat))
    stop(Sys.time(), ": Missing required parameter stat")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if (!allValid(nlStat, validNlStats))
    stop(Sys.time(), ": Invalid/unsupported nlStat detected")
  
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
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return TRUE/FALSE
#'
#' @examples
#' ctryCode <- "KEN"
#' admLevel <- "KEN_adm0"
#' message(Sys.time(), ": Data file for ", ctryCode, 
#'     ifelse(Rnightlights:::existsCtryNlDataFile(ctryCode, admLevel), 
#'         " FOUND", " NOT FOUND"))
#'
existsCtryNlDataFile <- function(ctryCode=NULL, admLevel, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    stop(Sys.time(), ": Missing required parameter admLevel")
  
  #for polygons look for shapefile dir
  return(file.exists(getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
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
#' @param nlTypes character The nlTypes
#'
#' @param nlPeriods character The nlPeriods
#' 
#' @param nlStats character The nlStats to check for
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsCtryNlData("KEN", "KEN_adm0", "VIIRS.M","201401", "sum")
#'
existsCtryNlData <- function(ctryCode=NULL, admLevel, nlTypes, nlPeriods, nlStats, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    stop(Sys.time(), ": Missing required parameter admLevel")
  
  if(missing(nlPeriods))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(nlStats))
    stop(Sys.time(), ": Missing required parameter nlStats")
  
  if(missing(nlTypes))
    stop(Sys.time(), ": Missing required parameter nlTypes")
  
  if(length(ctryCode) > 1 || length(admLevel) > 1)
    stop(Sys.time(), ": Only 1 ctryCode and admLevel can be checked at a time")
  
  if(!validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) ")
  
  if(!allValidNlPeriods(nlPeriods=nlPeriods, nlTypes=nlTypes))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriods, " for nlType: ", nlTypes)
  
  if(!all(validNlStats(nlStats)))
    stop(Sys.time(), ": Invalid nlStat: ", nlStats)
  
  if (existsCtryNlDataFile(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    dta <- utils::read.csv(getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath), nrow=1, header=TRUE)
  else
    dta <- NULL
  
  hdrs <- names(dta)
  
  searchCols <- paste("NL", nlTypes, nlPeriods, toupper(nlStats), sep="_")
  
  return(unlist(lapply(searchCols, function(x) length(grep(pattern = x, x = hdrs, ignore.case = T))>0)))
}

allExistsCtryNlData <- function(ctryCodes, admLevels, nlTypes, nlPeriods, nlStats, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  all(unlist(existsCtryNlData(ctryCode = ctryCodes,
                              admLevel = admLevels,
                              nlPeriods = nlPeriods,
                              nlTypes = nlTypes,
                              nlStats = nlStats,
                              gadmVersion = gadmVersion,
                              custPolyPath = custPolyPath)))
}

######################## listCtryNlData ###################################

#' List available data
#'
#' List available data. If source is "local" it lists data cached locally.
#'     If source is remote lists available data on the remote repository.
#' 
#' @param ctryCodes  \code{character} vector of ctryCodes to filter by
#' 
#' @param admLevels A character vector of admLevels to filter by
#' 
#' @param nlTypes A character vector of nlTypes to filter by
#' 
#' @param nlPeriods A character vector of nlPeriods to filter by 
#' 
#' @param polySrcs The source of polygons e.g. GADM or CUST to filter by
#' 
#' @param polyVers The version of the polygon source to filter by
#' 
#' @param nlStats The stats to filter by
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
listCtryNlData <- function(ctryCodes=NULL, admLevels=NULL, nlTypes=NULL, nlPeriods=NULL, polySrcs=NULL, polyVers=NULL, nlStats=NULL, source="local")
{
  dataList <- NULL
  dataType <- NULL #appease CRAN note for global variables
  nlType <- NULL #appease CRAN note for global variables
  nlPeriod <- NULL #appease CRAN note for global variables
  
  #get a list of country data files present
  countries <- list.files(path = getNlDir(dirName = "dirNlData"), pattern = "^NL_DATA_.*(GADM|CUST).*\\.csv$")
  
  #for each country filename
  for (ctry in countries)
  {
    if(grepl("_GADM-", ctry))
    {
      #prefix
      prefix <- "NL_DATA_"
      
      ctryCode <- substr(ctry, 9, 11)
      
      admLevel <- substr(ctry, nchar(prefix)+nchar(ctryCode)+2, nchar(prefix)+nchar(ctryCode)+5)
      
      polyPart <- unlist(strsplit(substr(ctry, nchar(prefix)+nchar(ctryCode)+nchar(admLevel)+3, nchar(ctry)-4), "-"))
      
      polySrc <- polyPart[1]
      
      polyVer <- polyPart[2]
    } else
    {
      #prefix
      prefix <- "NL_DATA_"
      
      #TODO: optional ctryCode extraction
      possibleCtryCode <- substr(ctry, nchar(prefix)+1, nchar(prefix)+5)
      
      ctryCode <- if(grepl("^[A-Z]{3}_", possibleCtryCode)) possibleCtryCode else ""
      
      polyPos <- stringr::str_locate(ctry, "CUST")
      
      admLevel <- substr(ctry, nchar(prefix)+nchar(ctryCode)+1, polyPos[1]-2)
      
      polyPart <- unlist(strsplit(substr(ctry, polyPos[1], nchar(ctry)-4), "-"))
      
      polySrc <- polyPart[1]
      
      polyVer <- polyPart[2]
    }
    
    #read in the header row
    ctryHdr <- data.table::fread(input = file.path(getNlDir(dirName = "dirNlData"), ctry), header = F, nrows = 1)
    
    #grab only the NL cols
    nlCtryHdr <- grep("^NL", ctryHdr, value = T)
    
    #split the NL colnames into their components e.g. "NL_OLS.Y_2012_MEAN"
    #= "NL"+nlType+nlPeriod+stat
    nlCtryHdr <- reshape2::colsplit(nlCtryHdr, "_", c("V1", "V2","V3","V4"))
    
    #only add if there are stats cols
    if(nrow(nlCtryHdr) > 0)
    {
      #aggregate (paste) the colnames into a single row with stats for each unique 
      #nlType+nlPeriod converted to a single field
      nlCtryHdr <- stats::aggregate(V4 ~ V1 + V2 + V3, data=nlCtryHdr, FUN=paste, collapse=",")
      
      #add a ctryCode column
      nlCtryHdr <- cbind(rep(ctryCode, nrow(nlCtryHdr)), rep(admLevel, nrow(nlCtryHdr)), rep(polySrc, nrow(nlCtryHdr)), rep(polyVer, nrow(nlCtryHdr)), nlCtryHdr)
      
      #combine into one table
      dataList <- rbind(dataList, nlCtryHdr)
    }
  }
  
  if(is.null(dataList))
    return(NULL)
  
  #convert into a dataframe with numbered rownames
  dataList <- data.frame(dataList, row.names = 1:nrow(dataList), stringsAsFactors=F)
  
  #label the columns
  names(dataList) <- c("ctryCode", "admLevel", "polySrc", "polyVer", "dataType", "nlType", "nlPeriod", "nlStats")
  
  dataList$ctryCode <- as.character(dataList$ctryCode)
  dataList$admLevel <- as.character(dataList$admLevel)
  dataList$nlPeriod <- as.character(dataList$nlPeriod)
  dataList$polySrc <- as.character(dataList$polySrc)
  dataList$polyVer <- as.character(dataList$polyVer)
  
  #filters
  #filter by ctryCode if supplied
  if(!is.null(ctryCodes) && ctryCodes != "")
    dataList <- dataList[which(dataList[,"ctryCode"] %in% ctryCodes),]
  
  if(!is.null(admLevels) && admLevels != "")
    dataList <- dataList[which(dataList[,"admLevel"] %in% admLevels),]
  
  #filter by nlType if supplied
  if(!is.null(nlTypes) && nlTypes != "")
    dataList <- dataList[which(dataList[,"nlType"] %in% nlTypes),]
  
  #filter by nlPeriod if supplied
  if(!is.null(nlPeriods))
    dataList <- dataList[which(dataList[,"nlPeriod"] %in% nlPeriods),]
  
  #filter by polySrc if supplied
  if(!is.null(polySrcs) && polySrcs != "")
    dataList <- dataList[which(dataList[,"polySrc"] %in% polySrcs),]
  
  #filter by polyVer if supplied
  if(!is.null(polyVers) && polyVers != "")
    dataList <- dataList[which(dataList[,"polyVer"] %in% polyVers),]
  
  #filter by polyVer if supplied
  if(!is.null(nlStats))
    dataList <- dataList[unique(unlist(sapply(nlStats, function(nlStat) grep(nlStat, dataList[,"nlStats"], ignore.case = T)))),]
  
  #Reorder the columns
  dataList <- dplyr::select(dataList, dataType, ctryCode, admLevel, nlType, nlPeriod, polySrc, polyVer, dplyr::contains("stat"))
  
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
#' @param nlTypes A character vector of nlTypes to filter by
#' 
#' @param nlPeriods A character vector of nlPeriods to filter by
#' 
#' @param polySrcs The source of polygons e.g. GADM or CUST to filter by
#' 
#' @param polyVers The version of the polygon source to filter by
#' 
#' @param nlStats The stats to filter by
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
listCtryNlRasters <- function(ctryCodes=NULL, nlPeriods=NULL, nlTypes=NULL, polySrcs=NULL, polyVers=NULL, nlStats=NULL, source="local")
{
  #appease CRAN note for global variables
  rastType <- NULL
  ctryCode <- NULL
  nlType <- NULL
  nlPeriod <- NULL
  polySrc <- NULL
  
  #get a list of country data files present
  rasterList <- list.files(path = getNlDir(dirName = "dirRasterOutput"), pattern = "^NL_.*(GADM|CUST).*\\.tif$")
  
  if(length(rasterList) == 0)
    return(NULL)
  
  tifName <- substring(rasterList, regexpr("_(CUST|GADM).*\\.tif$", rasterList))
  
  tifName <- gsub("_|\\.tif", "", tifName)
  
  rasterList <- gsub("_(CUST|GADM).*\\.tif$", "", rasterList)
  
  rasterList <- strsplit(gsub(".tif", "", rasterList), "_")
  
  rasterList <- t(unlist(sapply(rasterList, rbind)))
  
  #convert into a dataframe with numbered rownames
  rasterList <- as.data.frame(rasterList)
  
  rasterList <- cbind(rasterList, tifName)
  
  #label the columns
  names(rasterList) <- c("rastType", "ctryCode", "nlType", "nlPeriod", "polySrc")
  
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
  rasterList <- dplyr::select(rasterList, rastType, ctryCode, nlType, nlPeriod, polySrc)
  
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
#' @param nlTypes A character vector of nlTypes to filter by
#' 
#' @param nlPeriods A character vector of nlPeriods to filter by
#' 
#' @param tileName Character vector tileNames to filter by
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
listNlTiles <- function(nlTypes=NULL, nlPeriods=NULL, tileName=NULL, source="local")
{
  #appease CRAN note for global variables
  dataType <- NULL
  nlType <- NULL
  nlPeriod <- NULL
  tileName <- NULL
  
  if(source=="remote")
  {
    message(Sys.time(), ": Not yet implemented. Please post a comment on the github page to fasttrack this feature.")
    
    return(NULL)
  }
  
  #get a list of country data files present
  rasterList <- list.files(path = getNlDir(dirName = "dirNlTiles"), pattern = "^NL_TILE_.*\\..*\\.tif$")
  
  if(length(rasterList) == 0)
    return(NULL)
  
  rasterList <- strsplit(gsub("TILE_|.tif", "", rasterList), "_")
  
  rasterList <- t(unlist(sapply(rasterList, rbind)))
  
  #convert into a dataframe with numbered rownames
  rasterList <- as.data.frame(rasterList)
  
  #label the columns
  names(rasterList) <- c("dataType", "nlType", "nlPeriod", "tileName")
  
  #filters
  #filter by nlType if supplied
  if(!is.null(nlTypes))
    rasterList <- rasterList[which(rasterList[,"nlType"] %in% nlTypes),]
  
  #filter by nlPeriod if supplied
  if(!is.null(nlPeriods))
    rasterList <- rasterList[which(rasterList[,"nlPeriod"] %in% nlPeriods),]
  
  #filter by tileName if supplied
  if(!is.null(tileName))
    rasterList <- rasterList[which(rasterList[,"tileName"] %in% tileName),]
  
  #Reorder the columns
  rasterList <- dplyr::select(rasterList, dataType, nlType, nlPeriod, tileName)
  
  #only return list if we have records esp. after filtering else return NULL
  if(nrow(rasterList) > 0)
    return(rasterList)
  else
    return(NULL)
}