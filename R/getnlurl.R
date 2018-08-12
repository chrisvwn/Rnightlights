######################## getNlUrlOLS ###################################

#' Function to return the url of the OLS tile to download
#'
#' Function to return the url of the OLS tile to download given the year
#'
#' @param nlPeriod The nlPeriod of the tile for which to return the tile download URL
#'
#' @return character string Url of the OLS tile file
#'
#' @examples
#' \dontrun{
#' tileUrl <- Rnightlights:::getNlUrlOLS("1999")
#' }
#'
getNlUrlOLS <- function(nlPeriod)
{
  nlPeriod <- as.character(nlPeriod)
  
  #Function to return the url of the file to download given the year, month, and nlTile index
  #nlTile is a global list
  
  ntLtsBaseUrl <- "https://www.ngdc.noaa.gov"
  
  #the page that lists all available nightlight files
  ntLtsPageHtml <- "https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html"
  
  #the local name of the file once downloaded
  ntLtsPageLocalName <- file.path(getNlDir("dirNlTemp"), "ntltspageols.html")
  
  #if the file does not exist or is older than a day download it afresh
  #not working. download.file does not seem to update mtime
  if (!file.exists(ntLtsPageLocalName) || (lubridate::date(lubridate::now()) - lubridate::date(file.mtime(ntLtsPageLocalName)) > lubridate::as.difftime(lubridate::period("1 day"))))
  {
    utils::download.file(url = ntLtsPageHtml, destfile = ntLtsPageLocalName, method = "auto", extra = "-N")
  }
  #else
  #  message(paste0(ntLtsPageHtml, " already downloaded"))
  
  #read in the html page
  ntLtsPage <- xml2::read_html(ntLtsPageLocalName)
  
  ntLtsPage <- rvest::html_nodes(ntLtsPage, "table tr td a")
  
  #search for a line containing the patterns that make the files unique
  #sample url: https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/F101992.v4.tar
  #create the pattern
  ntLtsPageRgxp <- paste0("F.*.", nlPeriod,".*.tar")
  
  #search for the pattern in the page
  ntLtsPageHtml <- ntLtsPage[grep(pattern = ntLtsPageRgxp, x=ntLtsPage)]
  
  #split the output on quotes since this url is of the form ...<a href="URL"download> ...
  #the url is in the second position
  ntLtsPageUrl <- rvest::html_attr(ntLtsPageHtml,name = "href")
  
  ntLtsPageUrl <- gsub("\n", "", ntLtsPageUrl)
  ntLtsPageUrl <- gsub("\r", "", ntLtsPageUrl)
  
  ntLtsPageUrl <- unlist(lapply(ntLtsPageUrl, FUN=function(x) paste0(ntLtsBaseUrl, x)))
  
  #****NOTE: temp for testing using local download****
  #
  #fname <- stringr::str_extract(ntLtsPageUrl, "SVDNB.*.tgz")
  #ntLtsPageUrl <- paste0("http://localhost/", fname)
  #
  #****DELETE WHEN DONE****
  
  return (ntLtsPageUrl)
}

######################## getNlUrlVIIRS ###################################

#' Function to return the url of the VIIRS tile to download
#'
#' Function to return the url of the VIIRS tile to download given the year, month, and nlTile index
#'
#' @param nlPeriod character string the nlPeriod
#'
#' @param tileNum The integer index of the tile to download as given by \code{getNlTiles}
#' 
#' @param nlType character the nlType
#'
#' @return Character string Url of the VIIRS tile file
#'
#' @examples
#' \dontrun{
#' tileUrl <- Rnightlights:::getNlUrlVIIRS("20171231", "1", "VIIRS.D")
#' 
#' tileUrl <- Rnightlights:::getNlUrlVIIRS("201401", "1", "VIIRS.M")
#' 
#' tileUrl <- Rnightlights:::getNlUrlVIIRS("2015", "1", "VIIRS.Y")
#' }
#'
getNlUrlVIIRS <- function(nlPeriod, tileNum, nlType)
{
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(!allValidNlPeriods(nlPeriod, nlType))
    stop(Sys.time(), ": Invalid nlPeriod")
  
  #in case nlTiles exists globally from elsewhere
  if (!exists("nlTiles") || nrow(nlTiles) != 6)
    nlTiles <- getNlTiles(nlType)
  
  inYear <- as.character(substr(nlPeriod, 1, 4))
  
  inMonth <- as.character(substr(nlPeriod, 5, 6))
  
  inDay <- as.character(substr(nlPeriod, 7, 8))
  
  #nlTile is a global list
  
  #the page that lists all available nightlight files NOTE: URL CHANGE from "https://www.ngdc.noaa.gov/eog/viirs/download_mon_mos_iframe.html"
  #ntLtsPageHtml <- "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html"
  
  ntLtsIndexUrlVIIRS <- pkgOptions(paste0("ntLtsIndexUrl", nlType))

  #the local name of the file once downloaded
  ntLtsPageLocalName <- file.path(getNlDir("dirNlTemp"),paste0("ntltspage", nlType, ".html"))
  
  #if the file does not exist or is older than a week download it afresh
  if (!file.exists(ntLtsPageLocalName) || (lubridate::date(lubridate::now()) - lubridate::date(file.mtime(ntLtsPageLocalName)) > lubridate::as.difftime(lubridate::period("1 day"))) || file.size(ntLtsPageLocalName) == 0)
  {
    utils::download.file(url = ntLtsIndexUrlVIIRS, destfile = ntLtsPageLocalName, method = "auto", extra = "  -N --timestamping --no-use-server-timestamps")
  }
  #else
  #  message(paste0(ntLtsPageHtml, " already downloaded"))
  
  #read in the html page
  ntLtsPage <- readr::read_lines(ntLtsPageLocalName)
  
  #search for a line containing the patterns that make the files unique i.e.
  #1. SVDNB_npp_20141001 - year+month+01
  #2. vcmcfg - for file with intensity as opposed to cloud-free counts (vcmslcfg)
  #sample url VIIRS Daily: https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/mosaics//20180114/SVDNB_npp_d20180114.d.00N060E.rade9.tif
  #sample url VIIRS Monthly: https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201210/vcmcfg/SVDNB_npp_20121001-20121031_75N180W_vcmcfg_v10_c201602051401.tgz
  
  #create the pattern
  if(stringr::str_detect(nlType, "D"))
    ntLtsPageRgxp <- paste0("SVDNB_npp_d", nlPeriod, "\\.d\\.", nlTiles[tileNum,"name"], "\\.(rade9\\.tif)")
  else if(stringr::str_detect(nlType, "M"))
    ntLtsPageRgxp <- paste0("SVDNB_npp_", nlPeriod, "01.*", nlTiles[tileNum,"name"], ".*vcmcfg")
  else if(stringr::str_detect(nlType, "Y"))
    ntLtsPageRgxp <- paste0("SVDNB_npp_", nlPeriod, "0101-", nlPeriod, "1231_", nlTiles[tileNum,"name"], ".*tgz")
  
  
  #search for the pattern in the page
  ntLtsPageHtml <- ntLtsPage[grep(pattern = ntLtsPageRgxp, x=ntLtsPage)]
  
  #split the output on quotes since this url is of the form ...<a href="URL"download> ...
  #the url is in the second position
  ntLtsPageUrl <- unlist(strsplit(ntLtsPageHtml, '"'))[2]

  return (ntLtsPageUrl)
}