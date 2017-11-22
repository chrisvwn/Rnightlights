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
      if (nlY >= 2014 && nlY <= lubridate::year(lubridate::now()))
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
#' @param nlYearMonth the yearmonth in "YYYYMM" format e.g. "201410", "201701"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' \dontrun{validNlPeriodVIIRS("201512")}
#'  #returns TRUE
#'
#' \dontrun{validNlPeriodVIIRS("201513")}
#'  #returns FALSE; invalid month 13
#'
#' \dontrun{validNlPeriodVIIRS("201401")}
#'  #returns FALSE #VIIRS starts in "201401"
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

######################## validNlPeriod ###################################

#' Check if an nlPeriod is valid for a given nightlight type
#'
#' Check if an nlPeriod is valid for a given nightlight type
#'
#' @param nlPeriod the nlPeriod of interest
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validNlPeriod("201401","VIIRS")
#'  #returns TRUE
#'
#' validNlPeriod("201203","VIIRS")
#'  #returns FALSE
#'
#' validNlPeriod("2012","OLS")
#'  #returns TRUE
#'
#' @export
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
#' Create a range of nlPeriods. Autodetects the type of nlPeriod and creates a 
#'     character vector of nlPeriods filling in the intermediate nlPeriods.
#'     NOTE: Both start and end range must be valid and of the same type i.e.
#'     "OLS" years or "VIIRS" yearMonths
#' 
#' @param startNlPeriod the nlPeriod start
#'
#' @param endNlPeriod the nlPeriod end
#'
#' @return character vector of nlPeriods
#'
#' @examples
#' #get OLS years between 2004 and 2010
#' nlRange("2004", "2010")
#'
#' #get VIIRS yearMonths between Jan 2014 and Dec 2014
#' nlRange("201401", "201412")
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
    stop("Invalid start/end nlPeriod")

  allNlPeriods <- getAllNlPeriods(nlType)
    
  start <- grep(startNlPeriod, allNlPeriods)
  
  end <- grep(endNlPeriod, allNlPeriods)
  
  return(allNlPeriods[start:end])
  
}

######################## getAllNlPeriods ###################################

#' Generate a list of all possible nlPeriods for a given nlType
#'
#' Generate a list of all possible nlPeriods for a given nlType
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return character vector list of nlPeriods
#'
#' @examples
#' getAllNlPeriods("OLS")
#'  #returns a vector of all years from 1994 to 2013
#'
#' getAllNlPeriods("VIIRS")
#'  #returns a vector of all yearMonths from 201401 to present
#'
#' @export
getAllNlPeriods <- function(nlType)
{
  if (missing(nlType))
    stop("Missing required parameter nlType")
  
  if (!validNlType(nlType))
    stop("Invalid nlType: ", nlType)
  
  if (nlType == "OLS")
    return (1992:2013)
  else if (nlType == "VIIRS")
  {
    yrs <- 2014:lubridate::year(lubridate::now())
    
    mths <- c(paste("0",1:9, sep= ""),10:12)
    
    currYrMth <- paste0(lubridate::year(lubridate::now()), ifelse(lubridate::month(lubridate::now())<10, paste("0", lubridate::month(lubridate::now()), sep=""), lubridate::month(lubridate::now())))
    
    nlYrMths <- unlist(lapply(yrs, FUN = function(x) paste(x,mths,sep="")))
    
    nlYrMths <- nlYrMths[nlYrMths >= "201401" & nlYrMths <= currYrMth]
    
    return (nlYrMths)
  }
  else
    return()
}