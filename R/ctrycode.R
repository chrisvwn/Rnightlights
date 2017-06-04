######################## ctryNameToCode ###################################

#' Convert a country name to its ISO3 code
#'
#' Convert a country name to its ISO3 code. Exposes the rworldmap function
#'     rwmGetISO3(ctryName). See the examples. With no parameters returns a 
#'     list of ctryNames and their corresponding codes as given by rworldMap
#'
#' @param ctryName Chracter string common name of a country
#'
#' @return Character ISO3 ctryCode if found else NA
#'
#' @examples
#' \dontrun{ctryNameToCode("kenya")}
#'   #returns "KEN"
#'
#' ctryNameToCode("ken")
#'   #returns "KEN"
#'
#' @export
ctryNameToCode <- function(ctryName)
{
  if(missing(ctryName))
  {
    ctryList <- rworldmap::getMap()@data[,c("NAME", "ISO3")]
    
    ctryList <- dplyr::arrange(ctryList, NAME)
    
    return(ctryList)
  }
  
  if (class(ctryName) != "character" || is.null(ctryName) || is.na(ctryName) || ctryName =="" || length(grep("[^[:alpha:]| ]", ctryName) > 0))
    stop("Invalid ctryName: ", ctryName)
  
  return (rworldmap::rwmGetISO3(ctryName))
}

######################## ctryCodeToName ###################################

#' Convert a country ISO3 code to the full name
#'
#' Convert a country ISO3 code to the full name. Exposes the rworldmap function 
#'     isoToName(ctryCode). #rworldmap::isoToName can resolve 2-letter ctryCodes 
#'     but we only want 3-letter ISO3 codes.  With no parameters returns a list
#'     of ctryCodes and their corresponding names as given by rworldMap::getMap@data
#'
#' @param ctryCode The country Code to search for
#'
#' @return Character The full country name if the ctryCode is found. If
#'     \code{ctryCode} is not supplied then return a list of all country
#'     codes and their corresponding names
#'
#' @examples
#' ctryCodeToName("KEN")
#'
#' @export
ctryCodeToName <- function(ctryCode)
{
  if(missing(ctryCode))
  {
    ctryList <- rworldmap::getMap()@data[,c("ISO3", "NAME")]
    
    ctryList <- dplyr::arrange(ctryList, ISO3)
    
    return(ctryList)
  }
  
  if (class(ctryCode) != "character" || is.null(ctryCode) || is.na(ctryCode) || ctryCode =="" || length(grep("[^[:alpha:]]", ctryCode) > 0))
    stop("Invalid ctryCode: ", ctryCode)
  
  #rworldmap::isoToName can resolve 2-letter ctryCodes but we only want 3-letter ISO3 codes
  if(nchar(ctryCode) != 3)
    stop("Only 3-letter ISO3 codes allowed")
  
  return(rworldmap::isoToName(ctryCode))
}

######################## validCtryCode ###################################

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for
#' "VIIRS" nightlight type
#'
#' @param ctryCode the ISO3 country code to validate
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{validCtryCode("KEN")}
#'  #returns TRUE
#'
#' \dontrun{validCtryCode("UAE")}
#'  #returns FALSE. "United Arab Emirates" ISO3 code = "ARE"
#'
validCtryCode <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  #if the format is invalid return FALSE no need to return an error
  if (class(ctryCode) != "character" || is.null(ctryCode) || is.na(ctryCode) || ctryCode =="" || length(grep("[^[:alpha:]]", ctryCode) > 0))
    return(FALSE)
  
  if(is.na(suppressWarnings(ctryCodeToName(ctryCode))))
    return(FALSE)
  else
    return(TRUE)
}

######################## getAllNlCtryCodes ###################################

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for
#' "VIIRS" nightlight type
#'
#' @param omit The ctryCodes to exclude from processing. Else keywords missing=ctryCodes
#'     that are in rworldmap but not on gadm long=ctryCodes that take very long to process
#'     
#'
#' @return character vector of country codes
#'
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
  
  #some polygons have problems. use cleangeo package to rectify
  map <- cleangeo::clgeo_Clean(map)
  
  #get the list of country codes from the rworldmap
  ctryCodes <- as.character(map@data$ISO3)
  
  #remove all omitCountries from the list
  ctryCodes <- subset(ctryCodes, !(ctryCodes %in% omitCountries))
  
  #sort the country codes in ascending alphabetical order
  ctryCodes <- ctryCodes[order(ctryCodes)]
  
  return (ctryCodes)
}
