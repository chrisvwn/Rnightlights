validNlType <- function(nlType)
{
  nlTypes <- c("OLS", "VIIRS")
  
  if(missing(nlType))
    return(nlTypes)
  
  return(toupper(nlType) %in% nlTypes)
}