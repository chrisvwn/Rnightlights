#test internet availability used by multiple tests
internetAvailable <- function()
{
  curl::has_internet()
}

siteIsAvailable <- function(site)
{
  temp <- curl::curl_download(site, "temp.html")
  
  if(exists("temp"))
  {
    file.remove(temp)
    
    return(TRUE)
  }
  
  return(FALSE)
}
