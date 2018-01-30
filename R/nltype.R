######################## getAllNlTypes ###################################

#' Lists supported nightlight types
#'
#' Lists supported nightlight types. The names are in the form
#'     "nlType.avgPeriod" where avgPeriod is the period over which the
#'     raw data is averaged i.e. daily (D), monthly (M) and yearly (Y).
#'     Currently the nlTypes are "OLS" and "VIIRS". OLS only has yearly
#'     averaged data while VIIRS has daily, monthly and yearly
#'     e.g. VIIRS daily = "VIIRS.D"
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
#' @param nlType character string The nlType
#' 
#' @return \code{logical} whether the string is a valid nlType
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

#'@export
allValidNlTypes <- function(nlTypes)
{
  return(all(validNlTypes(nlTypes)))
}