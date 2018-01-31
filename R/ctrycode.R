######################## ctryNameToCode ###################################

#' Convert country names to ISO3 codes
#'
#' Convert country names to ISO3 codes. Searches the rworldmap map data.
#'     With no parameters returns a list of ctryNames and their corresponding
#'     codes as given by rworldMap
#'
#' @param ctryNames Character vector common names of countries to convert
#'
#' @return Character ISO3 ctryCode if found else NA
#'
#' @examples
#' ctryNameToCode("kenya") #returns "KEN"
#'
#' ctryNameToCode("ken") #returns "KEN"
#'   
#' ctryNameToCode("jamaica") #returns JAM
#'
#' @export
ctryNameToCode <- function(ctryNames)
{
  ADMIN <- NULL #to avoid global variable note in CRAN

  if(missing(ctryNames))
  {
    ctryList <- rworldmap::getMap()@data[,c("ADMIN", "ISO3")]

    ctryList$ISO3 <- as.character(ctryList$ISO3)
    ctryList$ADMIN <- as.character(ctryList$ADMIN)
    
    ctryList <- dplyr::arrange(ctryList, ADMIN)
    
    return(ctryList)
  }
  
  if (class(ctryNames) != "character" || is.null(ctryNames) || is.na(ctryNames) || ctryNames =="")
    stop("Invalid ctryName: ", ctryNames)

  hasNonAlpha <- sapply(ctryNames, function(ctryName) stringr::str_detect(ctryName, "[^[:alpha:]| ]"))
  
  if(any(hasNonAlpha))
    stop("Invalid ctryNames detected: ", paste0(ctryNames[hasNonAlpha], sep=","))
  
  ctryList <- rworldmap::getMap()@data[,c("ISO3", "ADMIN")]
  
  idx <- unlist(sapply(ctryNames, function(ctryName)
    {
      idxRes <- which(tolower(ctryList$ADMIN) == tolower(ctryName))
      
      #mark not found names with -1
      if(identical(idxRes, integer(0)))
        idxRes <- -1
      
      return(idxRes)
    })
  )

  foundIdx <- which(idx != -1)
  notFoundIdx <- which(idx == -1)

  #init result to the idx  
  result <- idx
  
  #for the idxs we found retrieve the name
  if(length(foundIdx)>0)
    result[foundIdx] <- as.character(ctryList[idx[foundIdx], "ISO3"])
  
  #for the idxs not found try using rwmGetISO3 which does a more 
  #in-depth search for country names
  if(length(notFoundIdx) > 0)
    result[notFoundIdx] <- unlist(sapply(ctryNames[notFoundIdx], function(cName)rworldmap::rwmGetISO3(cName)))
  
  return(result)
}

######################## ctryCodeToName ###################################

#' Convert a country ISO3 code to the full name
#'
#' Convert a country ISO3 code to the full name. Exposes the rworldmap function 
#'     isoToName(ctryCode). #rworldmap::isoToName can resolve 2-letter ctryCodes 
#'     but we only want 3-letter ISO3 codes.  With no parameters returns a list
#'     of ctryCodes and their corresponding names as given by rworldMap::getMap@data
#'
#' @param ctryCodes The country Codes to search for
#'
#' @return Character The full country name if the ctryCode is found. If
#'     \code{ctryCode} is not supplied then return a list of all country
#'     codes and their corresponding names
#'
#' @examples
#' ctryCodeToName("KEN") #returns Kenya
#' 
#' ctryCodeToName("ARE") #returns United Arab Emirates
#' 
#' ctryCodeToName("USA") #returns United States of America
#' 
#' ctryCodeToName("JAM") #returns Jamaica
#'
#' @export
ctryCodeToName <- function(ctryCodes)
{
  ISO3 <- NULL #to avoid global variable note in CRAN
  
  if(missing(ctryCodes))
  {
    ctryList <- rworldmap::getMap()@data[,c("ISO3", "ADMIN")]

    ctryList$ISO3 <- as.character(ctryList$ISO3)
    ctryList$ADMIN <- as.character(ctryList$ADMIN)
    
    ctryList <- dplyr::arrange(ctryList, ISO3)
    
    return(ctryList)
  }
  
  if (class(ctryCodes) != "character" || is.null(ctryCodes) || is.na(ctryCodes) || ctryCodes =="")
    stop("Invalid ctryCode: ", ctryCodes)
  
  ctryList <- rworldmap::getMap()@data[,c("ISO3", "ADMIN")]

  idx <- sapply(ctryCodes, function(ctryCode)
  {
      idxRes <- which(toupper(ctryList$ISO3) == toupper(ctryCode))
               
      if(identical(idxRes, integer(0)))
        idxRes <- -1
      
      idxRes
  })
    
  foundIdx <- which(idx != -1)
  notFoundIdx <- which(idx == -1)
  
  result <- idx
  
  result[foundIdx] <- as.character(ctryList[idx[foundIdx], "ADMIN"])
  
  result[notFoundIdx] <- NA
  
  return(result)
}

######################## validCtryCodes ###################################

#' Check if country codes are valid
#'
#' Check if country codes are valid
#'
#' @param ctryCodes the ISO3 country codes to validate
#'
#' @return named logical vector TRUE/FALSE
#'
#' @examples
#' validCtryCodes(c("KEN", "UGA")) #returns TRUE TRUE
#'
#' validCtryCodes("UAE") #returns FALSE. "United Arab Emirates" ISO3 code = "ARE"
#'
#'@export
validCtryCodes <- function(ctryCodes)
{
  if(missing(ctryCodes))
    stop("Missing required parameter ctryCode")
  
  #if the format is invalid return FALSE no need to return an error
  if (!is.character(ctryCodes) || is.null(ctryCodes) || is.na(ctryCodes) || ctryCodes =="")
    return(FALSE)
  
  return(toupper(ctryCodes) %in% toupper(getAllNlCtryCodes()))
}

######################## allValidCtryCodes ###################################

#' Check if all ctryCodes are valid
#'
#' Check if all ctryCodes are valid
#'
#' @param ctryCodes the ISO3 country codes to validate
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::allValidCtryCodes(c("BDI", "ETH", "KEN", "RWA", "TZA", "UGA")) #returns TRUE
#'
#' #returns FALSE. "United Arab Emirates" ISO3 code = "ARE"
#' Rnightlights:::allValidCtryCodes(c("UGA", "UAE"))
#'
allValidCtryCodes <- function(ctryCodes)
{
  return(all(validCtryCodes(ctryCodes)))
}

######################## getAllNlCtryCodes ###################################

#' Get all known valid ISO3 country codes
#'
#' Get a list of all known valid ISO3 country codes.
#'
#' @param omit The ctryCodes to exclude from processing based on observed
#'     behaviour. The option can take the following values:
#'     \itemize{
#'         \item{\code{missing}} ctryCodes that are in \code{rworldmap} but not on \code{GADM}
#'         \item{\code{long}} ctryCodes that take very long to process using 'rast'
#'             options. May improve using more processors or using 'gdal' 
#'             options.
#'         \item{\code{error}} ctryCodes whose polygons have caused processing to crash
#'             in tests. Not extensively tested and may work fine on other 
#'             systems.
#'         \item{\code{all}} Omit a combination of all the above options
#'         \item{\code{none}} Do not omit any ctryCodes
#'     }
#'
#' @return character vector of country codes
#'
#'@export
getAllNlCtryCodes <- function(omit="none")
{
  #omit is a vector and can contain "long", "missing" or "error"
  #if omit is "none" do not exclude any countries
  #if omit is "all", empty or NA set to default: omit=c("long", "missing", "error")
  
  omit <- tolower(omit)
  
  if(omit != "none" && (omit == "all" || !(omit %in% c("long", "missing", "error")) || is.na(omit)))
    omit <- c("long", "missing", "error")
  
  tooLongProcessing <- ""
  missingPolygon <- ""
  errorProcessing <- ""
  
  if ("long" %in% omit)
    tooLongProcessing <- c("RUS", "BRA", "USA", "ATA", "FRA")
  
  if ("missing" %in% omit)
    missingPolygon <- c("CYN",  "KOS", "Ashm", "Gaza", "IOA", "KAS")
  
  if ("error" %in% omit)
    errorProcessing <- c("ATF", "GNQ", "KIR", "NZL", "CAN", "MUS")
  
  #consolidate the list of countries to omit
  omitCountries <- unlist(c(tooLongProcessing, missingPolygon, errorProcessing))
  
  #rworldmap has more country codes in countryRegions$ISO3 than in the map itself
  #select ctryCodes from the map data itself
  map <- rworldmap::getMap()

  #get the list of country codes from the rworldmap
  ctryCodes <- as.character(map@data$ISO3)
  
  #remove all omitCountries from the list
  ctryCodes <- subset(ctryCodes, !(ctryCodes %in% omitCountries))
  
  #sort the country codes in ascending alphabetical order
  ctryCodes <- ctryCodes[order(ctryCodes)]
  
  return (as.character(ctryCodes))
}
