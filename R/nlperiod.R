######################## validNlYearNum ###################################

#' Check if a year is valid for a given nightlight type
#'
#' Check if a year is valid for a given nightlight type
#'
#' @param yearNum the year of interest
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{validNlYearNum("2014","VIIRS")}
#'
validNlYearNum <- function(yearNum, nlType)
{
  if (missing(yearNum))
    stop("Missing parameter yearNum")
  
  if (missing(nlType) || (nlType != "OLS" && nlType != "VIIRS"))
    stop("Missing or invalid required parameter nlType")
  
  yearNum <- as.character(yearNum)
  
  if (class(yearNum) != "character" || yearNum =="" || length(grep("[^[:digit:]]", yearNum) > 0))
    return(FALSE)
  
  nlY <- as.numeric(yearNum)
  
  if (nlType == "OLS")
  {
    if (nlY >= 1992 && nlY <= 2013)
      return(TRUE)
    else
      return(FALSE)
  }
  else
    if (nlType=="VIIRS")
    {
      if (nlY >= 2012 && nlY <= lubridate::year(lubridate::now()))
        return(TRUE)
      else
        return(FALSE)
    }
  else
    return (FALSE)
}

######################## validNlMonthNum ###################################

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for
#' "VIIRS" nightlight type
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{validNlMonthNum("01","VIIRS")}
#'  #returns TRUE
#'
#' \dontrun{validNlMonthNum("13","VIIRS")}
#'  #returns FALSE
#'
#' \dontrun{validNlMonthNum("01","OLS")}
#'  #returns FALSE
#'
validNlMonthNum <- function(monthNum, nlType="VIIRS")
{
  if (missing(monthNum))
    stop("Missing required parameter monthNum")
  
  monthNum <- as.character(monthNum)
  nlType <- as.character(nlType)
  
  if (nlType!="VIIRS")
    stop("nlMonth only valid for nlType=\"VIIRS\"")
  
  if (class(monthNum) != "character" || monthNum =="" || length(grep("[^[:digit:]]", monthNum) > 0))
    return(FALSE)
  
  nlM <- as.numeric(monthNum)
  
  if (nlM >= 1 && nlM <= 12)
    return(TRUE)
  else
    return(FALSE)
}

######################## validNlPeriodVIIRS ###################################

#' Check if a VIIRS nlYearMonth is valid
#'
#' Check if a VIIRS nlYearMonth is valid Note this function is only valid for the "VIIRS" nightlight type
#'
#' @param nlYearMonth the yearmonth in "YYYYMM" format e.g. "201210", "201401"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{validNlPeriodVIIRS("201512")}
#'  #returns TRUE
#'
#' \dontrun{validNlPeriodVIIRS("201513")}
#'  #returns FALSE
#'
#' \dontrun{validNlPeriodVIIRS("201201")}
#'  #returns FALSE #VIIRS starts in "201204"
#'
validNlPeriodVIIRS <- function(nlYearMonth)
{
  if (missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")
  
  nlYearMonth <- as.character(nlYearMonth)
  
  if (class(nlYearMonth) != "character" || nlYearMonth =="" || length(grep("[^[:digit:]]", nlYearMonth) > 0))
    return(FALSE)
  
  if(nchar(nlYearMonth) != 6)
    return(FALSE)
  
  nlY <- as.numeric(substr(nlYearMonth, 1, 4))
  nlM <- as.numeric(substr(nlYearMonth, 5, 6))
  
  if (as.numeric(nlY) == 2012 && as.numeric(nlM) < 4) #Special cases. first VIIRS in 201204
    return(FALSE)
  
  if (validNlYearNum(yearNum = nlY, nlType = "VIIRS") && validNlMonthNum(monthNum = nlM, nlType = "VIIRS"))
    return(TRUE)
  else
    return(FALSE)
}

######################## validNlPeriodOLS ###################################

#' Check if an OLS nlYear is valid
#'
#' Check if an OLS nlYear is valid
#'
#' @param nlYear the year in "YYY" format e.g. "2012"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{validNlPeriodOLS("2015")}
#'  #returns FALSE
#'
#' \dontrun{validNlPeriodOLS("2004")}
#'  #returns TRUE
#'
#' \dontrun{validNlPeriodOLS("201201")}
#'  #returns FALSE
#'
validNlPeriodOLS <- function(nlYear)
{
  if (missing(nlYear))
    stop("Missing required parameter nlPeriod")
  
  nlYear <- as.character(nlYear)
  
  if (class(nlYear) != "character" || nlYear =="" || length(grep("[^[:digit:]]", nlYear) > 0))
    return(FALSE)
  
  if (validNlYearNum(yearNum = nlYear, nlType = "OLS"))
    return(TRUE)
  else
    return(FALSE)
}

######################## validNlMonthNum ###################################

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for
#' "VIIRS" nightlight type
#'
#' @param nlPeriod the nlPeriod of interest
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{validNlPeriod("201204","VIIRS")}
#'  #returns TRUE
#'
#' \dontrun{validNlPeriod("201203","VIIRS")}
#'  #returns FALSE
#'
#' \dontrun{validNlPeriod("2012","OLS")}
#'  #returns TRUE
#'
validNlPeriod <- function(nlPeriod, nlType)
{
  if (missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if (missing(nlType))
    stop("Missing required parameter nlType")
  
  if(!validNlType(nlType))
    stop("Invalid nlType")
  
  nlPeriod <- as.character(nlPeriod)
  nlType <- as.character(nlType)
  
  if (nlType == "OLS")
    return(validNlPeriodOLS(nlPeriod))
  else if (nlType == "VIIRS")
    return(validNlPeriodVIIRS(nlPeriod))
  else
    return(NA) #we should never get here
}

######################## nlRange ###################################

#' Create a range of nlPeriods
#'
#' Create a range of nlPeriods. Autodetects the type of nlPeriod and if both
#'     start and end range are valid and of the same type it creates a 
#'     character vector of nlPeriods filling in the intermediate nlPeriods
#' 
#' @param startNlPeriod the nlPeriod start
#'
#' @param endNlPeriod the nlPeriod end
#'
#' @return character vector of nlPeriods
#'
#' @examples
#' nlRange("2004", "2010")
#'
#' nlRange("201204", "201412")
#'
#' @export
nlRange <- function(startNlPeriod, endNlPeriod)
{
  if(missing(startNlPeriod))
    stop("Missing required parameter startNlPeriod")
  
  if(missing(endNlPeriod))
    stop("Missing required parameter endNlPeriod")
  
  if(suppressWarnings(allValid(c(startNlPeriod, endNlPeriod), validNlPeriod, "OLS")))
    nlType <- "OLS"
  else if(suppressWarnings(allValid(c(startNlPeriod, endNlPeriod), validNlPeriod, "VIIRS")))
    nlType <- "VIIRS"
  else
    stop("Unknown nlPeriod")
  
  allNlPeriods <- getAllNlPeriods(nlType)
  
  start <- grep(startNlPeriod, allNlPeriods)
  
  end <- grep(endNlPeriod, allNlPeriods)
  
  return(allNlPeriods[start:end])
  
}
