######################## getAllNlTypes ###################################

#' Lists supported nightlight types
#'
#' Lists supported nightlight types. The names are in the form
#'     "nlType.avgPeriod" where avgPeriod is the period over which the
#'     raw data is averaged i.e. Daily (D), Monthly (M) and Yearly (Y).
#'     Currently the main nlTypes are "OLS" and "VIIRS". OLS only has yearly
#'     averaged data while VIIRS has daily, monthly and yearly
#'     e.g. VIIRS daily = "VIIRS.D", OLS yearly = "OLS.Y"
#'
#' @return \code{character} vector of supported nlTypes
#'
#' @examples
#' getAllNlTypes()
#'
#'@export
getAllNlTypes <- function()
{
  nlTypes <- c("OLS.Y", "VIIRS.D", "VIIRS.M", "VIIRS.Y")
  
  return(nlTypes)
}

######################## validNlTypes ###################################

#' Checks if a given character string is a valid nlType
#'
#' Checks if a given character string is a valid nlType
#' 
#' @param nlTypes \code{character vector} The nlTypes
#' 
#' @return \code{logical} whether the strings are valid nlTypes
#'
#' @examples
#' validNlTypes("VIIRS.D") #returns TRUE
#' 
#' validNlTypes("VIIRS") #returns FALSE
#'
#'@export
validNlTypes <- function(nlTypes)
{
  if(missing(nlTypes))
    return(getAllNlTypes())

  return(toupper(nlTypes) %in% getAllNlTypes())
}

######################## allValidNlTypes ###################################

#' Checks if all given character strings are valid nlTypes
#'
#' Checks if all given character strings are valid nlTypes
#' 
#' @param nlTypes \code{character vector} string The nlTypes
#' 
#' @return \code{logical vector} whether the strings are valid nlTypes
#'
#' @examples
#' Rnightlights:::allValidNlTypes("VIIRS.D") #returns TRUE
#' 
#' Rnightlights:::allValidNlTypes("VIIRS") #returns FALSE
#'
allValidNlTypes <- function(nlTypes)
{
  return(all(sapply(nlTypes, function(nlType)validNlTypes(nlType))))
}