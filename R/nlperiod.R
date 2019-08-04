######################## validNlPeriod ###################################

#' Check if an nlPeriod is valid for a given nightlight type
#'
#' Check if an nlPeriod is valid for a given nightlight type. Vectorized
#'     to allow checking multiple nlPeriods of corresponding nlTypes. If
#'     a single nlType is given all nlPeriods are checked in that nlType.
#'     If multiple nlTypes are given, then a corresponding number of
#'     nlPeriods is expected e.g. if nlPeriods is a vector each entry
#'     must correspond to the nlType. If multiple nlPeriods are to be 
#'     tested per nlType then a list of vectors is expected, one for
#'     each nlType.
#'
#' @param nlPeriods the nlPeriods of interest
#'
#' @param nlTypes type of nightlight
#'
#' @return named logical list of TRUE/FALSE
#'
#' @examples
#' validNlPeriods(c("201401", "201402"),"VIIRS.M")
#'  #returns
#'  #$VIIRS.M
#'  #201401 201402 
#'  #  TRUE   TRUE
#'
#' validNlPeriods("201203","VIIRS.M")
#'  #returns FALSE
#'
#' validNlPeriods("2012","OLS.Y")
#'  #returns TRUE
#'
#' @export
validNlPeriods <- function(nlPeriods, nlTypes)
{
  if (missing(nlPeriods))
    stop(Sys.time(), ": Missing required parameter nlPeriods")
  
  if (missing(nlTypes))
    stop(Sys.time(), ": Missing required parameter nlTypes")
  
  if(!all(validNlTypes(nlTypes)))
    stop(Sys.time(), ": Missing or Invalid nlType")

  #nlPeriods <- as.character(nlPeriods)
  #nlTypes <- as.character(nlTypes)

  # if(length(nlTypes) == 1)
  #   return(stats::setNames(list(stats::setNames(nlPeriods %in% unlist(getAllNlPeriods(nlTypes)), nlPeriods)),nlTypes))

  # if(is.list(nlPeriods)length(nlPeriods) != length(nlTypes))
  #   stop(Sys.time(), ": nlPeriods and nlTypes are not same length")
  
  nlTypes <- unlist(nlTypes)
  
  # validPeriods <- stats::setNames(lapply(1:length(nlTypes), function(i){
  #   nlT <- nlTypes[i]
  #   nlPs <- unlist(nlPeriods[i])
  #   allNlPeriods <- unlist(getAllNlPeriods(nlT))
  #   
  #   valid <- stats::setNames(nlPs %in% allNlPeriods, nlPs)
  #   
  #   if(!all(valid))
  #     message(Sys.time(), "Invalid nlPeriods:: ", nlT,":",paste0(nlPs[!valid], sep=","))
  #   return(valid)
  #   }), nlTypes)

  validPeriods <- stats::setNames(nlPeriods %in% unlist(getAllNlPeriods(nlTypes)), nlPeriods)
  
  return(validPeriods)
}

######################## validNlPeriod ###################################

#' Check if all nlPeriods are valid for given nlTypes
#'
#' Check if all nlPeriods are valid for given nlTypes
#' 
#' @param nlPeriods \code{vector or list of character vectors} The nlPeriods of interest
#'
#' @param nlTypes \code{vector or list of character vectors} type of nightlight
#'
#' @return \code{logical} TRUE/FALSE
#'
#' @examples
#' Rnightlights:::allValidNlPeriods(c("201401", "201402"),"VIIRS.M")
#'  #returns TRUE
#'
#' Rnightlights:::allValidNlPeriods("201203","VIIRS.M")
#'  #returns FALSE
#'
#' Rnightlights:::allValidNlPeriods("2012","OLS.Y")
#'  #returns TRUE
#'
allValidNlPeriods <- function(nlPeriods, nlTypes)
{
  return(all(unlist(validNlPeriods(nlTypes = nlTypes, nlPeriods = nlPeriods))))
}

######################## nlRange ###################################

#' Create a range of nlPeriods
#'
#' Create a range of nlPeriods. Returns a list of character vectors of
#'     nlPeriods filling in the intermediate nlPeriods.
#'     NOTE: Both start and end range must be valid and of the same type.
#' 
#' @param startNlPeriod the nlPeriod start
#'
#' @param endNlPeriod the nlPeriod end
#' 
#' @param nlType the nlType
#'
#' @return character vector of nlPeriods
#'
#' @examples
#' #get OLS years between 2004 and 2010
#' nlRange("2004", "2010", "OLS.Y")
#'
#' #get VIIRS yearMonths between Jan 2014 and Dec 2014
#' nlRange("201401", "201412", "VIIRS.M")
#'
#' @export
nlRange <- function(startNlPeriod, endNlPeriod, nlType)
{
  if(missing(startNlPeriod))
    stop(Sys.time(), ": Missing required parameter startNlPeriod")
  
  if(missing(endNlPeriod))
    stop(Sys.time(), ": Missing required parameter endNlPeriod")
 
  if(!missing(nlType))
  {
    if(length(nlType) > 1)
      stop(Sys.time(), ": Only 1 nlType accepted")
    
    #if(!allValid(c(startNlPeriod, endNlPeriod), validNlPeriods, nlType))
    if(!allValidNlPeriods(nlTypes = nlType, nlPeriods = c(startNlPeriod, endNlPeriod)))
       stop(Sys.time(), ": Invalid nlPeriod detected for nlType ", nlType)
  }
  else
  {
    for(x in getAllNlTypes())
    {
      if(unlist(suppressMessages(validNlPeriods(nlPeriods = startNlPeriod, nlTypes = x))) && unlist(suppressMessages(validNlPeriods(nlPeriods = endNlPeriod, nlTypes = x))))
      {
        message(Sys.time(), ": NlRange autodetected nlType: ", x)
        nlType <- x
      }
    }
    
    if(is.null(nlType))
      stop(Sys.time(), ": Invalid start/end nlPeriod")
  }

  allNlPeriods <- unname(unlist(getAllNlPeriods(nlType)))
    
  start <- grep(startNlPeriod, allNlPeriods)
  
  end <- grep(endNlPeriod, allNlPeriods)
  
  return(allNlPeriods[start:end])
  
}

######################## getAllNlPeriods ###################################

#' Generate a list of all possible nlPeriods for a given nlType
#'
#' Generate a list of all possible nlPeriods for a given nlType
#'
#' @param nlTypes types of nightlights to process
#'
#' @return a named list of character vector nlPeriods
#'
#' @examples
#' getAllNlPeriods("OLS.Y")
#'  #returns a vector of all years from 1994 to 2013
#'
#' getAllNlPeriods("VIIRS.M")
#'  #returns a vector of all yearMonths from 201401 to present
#'
#' getAllNlPeriods(c("OLS.Y", "VIIRS.Y"))
#'  #returns a list with 2 named vectors, one for each annual nlType
#' @export
getAllNlPeriods <- function(nlTypes)
{
  if (missing(nlTypes))
    stop(Sys.time(), ": Missing required parameter nlTypes")
  
  if (!allValidNlTypes(nlTypes))
    stop(Sys.time(), ": Invalid nlType: ", nlTypes)
  
  sapply(nlTypes, function(nlType)
  {
    if (stringr::str_detect(nlType, "OLS"))
    {
      return (1992:2013)
    }
    else if(stringr::str_detect(nlType, "VIIRS"))
    {
      if (stringr::str_detect(nlType, "D")) #D=daily
      {
        startDate <- "2017-11-20"
        
        nlYrMthDys <- gsub("-", "", seq(as.Date(startDate), as.Date(date(), "%c"), by = "day"))
        
        return (nlYrMthDys)
      }
      else if (stringr::str_detect(nlType, "M")) #M=monthly
      {
        startDate <- "2012-04-01"
        
        nlYrMths <- substr(gsub("-", "", seq(as.Date(startDate), as.Date(date(), "%c"), by = "month")), 1, 6)
    
        return (nlYrMths)
      }
      else if (stringr::str_detect(nlType, "Y")) #Y=yearly
      {
        startDate <- "2012-04-01"
        
        nlYrs <- substr(gsub("-", "", seq(as.Date(startDate), as.Date(date(), "%c"), by = "year")), 1, 4)
        
        return (nlYrs)
      }
      else
        return()
    }
    else
      return()
  }, simplify = FALSE)
}