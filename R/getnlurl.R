######################## getNlUrlOLS ###################################

#' Function to return the url of the OLS tile to download
#'
#' Function to return the url of the OLS tile to download given the year
#'
#' @param nlType The nlType for which to return the tile download URL
#' 
#' @param nlPeriod The nlPeriod of the tile for which to return the tile download URL
#'
#' @param configName character the type of raster being processed
#'
#' @return character string Url of the OLS tile file
#'
#' @examples
#' \dontrun{
#' tileUrl <- Rnightlights:::getNlUrlOLS("1999")
#' }
#'
getNlUrlOLS <-
  function(nlType, nlPeriod,
           configName = pkgOptions(paste0("configName_", nlType)))
  {
    nlPeriod <- as.character(nlPeriod)
    
    if(!validNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
      stop(Sys.time(), ": Invalid nlPeriod supplied")
    
    if(!validNlConfigName(configName = configName, nlType = nlType))
      stop(Sys.time(), ": Invalid nlPeriod supplied")
    
    #configName <- toupper(configName)

    ntLtsUrls <- getAllNlUrlOLS()

    #search for a line containing the patterns that make the files unique
    #sample url: https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/F101992.v4.tar
    #create the pattern
    #configNames are found within the particular download files i.e.
    #F1019992.v4.tar should contain cf_cvg, avg_vis & stable_lights
    #
    # ntLtsRgxp <-
    #   if (toupper(configName) %in% toupper(c("cf_cvg", "avg_vis", "stable_lights")))
    #   {
    #     paste0("F\\d{2}", nlPeriod, "\\.v4\\.tar")
    #   } else if (configName %in% toupper(c("pct_lights", "avg_lights_x_pct")))
    #   {
    #     paste0("F\\d{2}", nlPeriod, ".*\\.avg_lights_x_pct.tgz")
    #   }
    
    ntLtsRgxp <- getNlFileRegex(nlTypes = nlType, configNames = configName, regexNum = "1",
                                nlPeriod = nlPeriod)
    
    # ntLtsRgxp <- unique(getAllNlConfigNames(nlTypes = nlType, configNames = configName)$fileRegex1)
    # 
    # ntLtsRgxp <- gsub(pattern = "{nlPeriod}", replacement = nlPeriod, x = ntLtsRgxp, fixed = TRUE)
    
    ntLtsUrls <- ntLtsUrls[[nlType]][[tolower(configName)]]
    
    #search for the pattern in the page
    ntLtsPageUrl <-
      grep(pattern = ntLtsRgxp, x = unlist(ntLtsUrls), value = TRUE)
    
    return (ntLtsPageUrl)
  }

######################## getAllNlUrlOLS ###################################

#' Function to return the url of the OLS tile to download
#'
#' Function to return the url of the OLS tile to download given the year
#'
#' @param nlTypes The nlTypes for which to return the tile download URL
#' 
#' @param configNames character filter by configNames if specified
#' 
#' @param extensions character filter by extensions if specified
#'
#' @return character string Url of the OLS tile file
#'
#' @examples
#' \dontrun{
#'   getAllNlUrlOLS()
#' }
#'
getAllNlUrlOLS <-
  function(nlTypes, configNames="", extensions)
  {
    if(missing(x = nlTypes))
      nlTypes <- grep(pattern = "OLS", x = getAllNlTypes(), value = TRUE)
    
    stats::setNames(lapply(nlTypes, function(nlType)
    {
      configNames <- getAllNlConfigNames(nlTypes = nlType)$configName
      
      #the page that lists all available nightlight files
      ntLtsPageHtml <- pkgOptions("ntLtsIndexUrlOLS.Y")
      
      #the local name of the file once downloaded
      ntLtsPageLocalName <-
        file.path(getNlDir("dirNlTemp"), "ntltspageols.html")
      
      ntLtsIndexUrlInfo <- httr::HEAD(ntLtsPageHtml)
      
      urlLastMTime <- as.POSIXct(x = ntLtsIndexUrlInfo$headers$`last-modified`, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC")
      
      fileLastMTime <- lubridate::as_datetime(x = file.mtime(ntLtsPageLocalName), tz = "UTC")
      
      #if the file does not exist or is older than the url modified time download it
      if (!file.exists(ntLtsPageLocalName) || file.size(ntLtsPageLocalName) == 0 ||
          fileLastMTime < urlLastMTime)
      {
        utils::download.file(
          url = ntLtsPageHtml,
          destfile = ntLtsPageLocalName,
          method = "auto",
          extra = "-N"
        )
      }else
      {
        message(paste0(ntLtsPageHtml, " already downloaded"))
      }
      
      #read in the html page
      ntLtsPage <- xml2::read_html(ntLtsPageLocalName)
      
      ntLtsPage <- rvest::html_nodes(ntLtsPage, "table tr td a")
      
      #search for a line containing the patterns that make the files unique
      #sample url: https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/F101992.v4.tar
      #create the pattern
      # ntLtsPageRgxp <-
      #   if (configName %in% toupper(c("cf_cvg", "avg_vis", "stable_lights")))
      #   {
      #     paste0("F\\d{2}", nlPeriod, "\\.v4\\.tar")
      #   } else if (configName %in% toupper(c("pct_lights", "avg_lights_x_pct")))
      #   {
      #     paste0("F\\d{2}", nlPeriod, ".*\\.avg_lights_x_pct.tgz")
      #   }
      
      ntLtsPageRgxp <- "(\\.tar|\\.tgz)"

      #pull the href nodes into a vector
      ntLtsPageUrls <- rvest::html_attr(ntLtsPage, name = "href")      

      #split the output on quotes since this url is of the form ...<a href="URL"download> ...
      #the url is in the second position
      ntLtsPageUrls <- unlist(strsplit(ntLtsPageUrls, '"'))
      
      ntLtsPageUrls <-
        grep(pattern = "http.*\\.(tar|tgz)", x = ntLtsPageUrls, value = TRUE)
      
      ntLtsPageUrls <- gsub("\n", "", ntLtsPageUrls)
      ntLtsPageUrls <- gsub("\r", "", ntLtsPageUrls)
      
      ntLtsPageUrls <- stats::setNames(lapply(configNames, function(configName)
      {
        ntLtsPageRgxp <-
          if (toupper(configName) %in% toupper(c("stable_lights", "raw", "")))
          {
            paste0("F\\d{2}.*\\.v4\\.tar")
          } else if (toupper(configName) %in% toupper(c("pct_lights", "avg_lights_x_pct")))
          {
            paste0("F\\d{2}.*\\.avg_lights_x_pct.tgz")
          }
        
        sort(grep(pattern = ntLtsPageRgxp, x = ntLtsPageUrls, value = TRUE))
      }), configNames)

      return (ntLtsPageUrls)
    }), nlTypes)
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
#' @param nlType character the nlType to consider
#'
#' @param configName character the configName to consider
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
getNlUrlVIIRS <-
  function(nlPeriod,
           tileNum,
           nlType,
           configName = pkgOptions(paste0("configName_", nlType)))
  {
    if (missing(nlPeriod))
      stop(Sys.time(), ": Missing required parameter nlPeriod")
    
    if (missing(tileNum))
      stop(Sys.time(), ": Missing required parameter tileNum")
    
    if (missing(nlType))
      stop(Sys.time(), ": Missing required parameter nlType")
    
    if (!allValidNlPeriods(nlPeriod, nlType))
      stop(Sys.time(), ": Invalid nlPeriod")
    
    tileNum <- try(as.integer(tileNum), TRUE)
    
    if(inherits(tileNum, "try-error"))
      stop(Sys.time(), ": tileNum must be an integer from 1-6")
    
    #in case nlTiles exists globally from elsewhere
    if (!exists("nlTiles") || nrow(nlTiles) != 6)
      nlTiles <- getNlTiles(nlType = nlType)
    
    # inYear <- as.character(x = substr(x = nlPeriod, start = 1, stop = 4))
    # 
    # inMonth <- as.character(x = substr(x = nlPeriod, start = 5, stop = 6))
    # 
    # inDay <- as.character(x = substr(x = nlPeriod, start = 7, stop = 8))
    
    ntLtsUrls <- getAllNlUrlVIIRS(nlTypes = nlType)
    #
    # #create the pattern to distinguish the url on the page
    # if (stringr::str_detect(string = nlType, pattern = "D"))
    #   ntLtsPageRgxp <-
    #   paste0("SVDNB_npp_d",
    #          nlPeriod,
    #          "\\.d\\.",
    #          nlTiles[tileNum, "name"],
    #          "\\.rade9\\.tif")
    # else if (stringr::str_detect(string = nlType, pattern = "M"))
    #   #VIIRS.M has 2 diff tgz files based on configName vcmcfg and vcmslcfg
    #   ntLtsPageRgxp <-
    #   paste0("SVDNB_npp_",
    #          nlPeriod,
    #          "01.*",
    #          nlTiles[tileNum, "name"],
    #          ".*",
    #          tolower(configName))
    # else if (stringr::str_detect(string = nlType, pattern = "Y"))
    #   ntLtsPageRgxp <-
    #   paste0("SVDNB_npp_",
    #          nlPeriod,
    #          "0101-",
    #          nlPeriod,
    #          "1231_",
    #          nlTiles[tileNum, "name"],
    #          ".*tgz")
    
    ntLtsRgxp <- getNlFileRegex(nlTypes = nlType, configNames = configName, regexNum = "1",
                                nlPeriod = nlPeriod, tileName = tileIdx2Name(tileNum = tileNum, nlType = nlType))
    
    ntLtsUrls <- unlist(ntLtsUrls[[nlType]][[tolower(configName)]])
    
    #search for the pattern in the page
    ntLtsUrls <-
      grep(pattern = ntLtsRgxp, x = ntLtsUrls, value = TRUE)
    
    return (ntLtsUrls)
  }

######################## getAllNlUrlVIIRS ###################################

#' Function to return the url of the VIIRS tile to download
#'
#' Function to return the url of the VIIRS tile to download given the year, month, and nlTile index
#'
#' @param nlTypes character the nlTypes to consider
#'
#' @param configNames character the configNames to consider
#'
#' @return Character string Url of the VIIRS tile file
#'
#' @examples
#' \dontrun{
#' tileUrl <- Rnightlights:::getAllNlUrlVIIRS("20171231", "1", "VIIRS.D")
#'
#' tileUrl <- Rnightlights:::getAllNlUrlVIIRS("201401", "1", "VIIRS.M")
#'
#' tileUrl <- Rnightlights:::getAllNlUrlVIIRS("2015", "1", "VIIRS.Y")
#' }
#'
getAllNlUrlVIIRS <-
  function(nlTypes, configNames)
  {
    if(missing(nlTypes) || is.null(nlTypes))
      nlTypes <- grep("VIIRS", getAllNlTypes(), value = T)
    
    if(missing(configNames))
      configNames <- ""
    
    stats::setNames(lapply(nlTypes, function(nlType)
    {
      ntLtsIndexUrlVIIRS <- pkgOptions(paste0("ntLtsIndexUrl", nlType))
      
      #the local name of the file once downloaded
      ntLtsPageLocalName <-
        file.path(getNlDir("dirNlTemp"), paste0("ntltspage", nlType, ".html"))
      
      ntLtsIndexUrlInfo <- httr::HEAD(ntLtsIndexUrlVIIRS)
      
      urlLastMTime <- as.POSIXct(x = ntLtsIndexUrlInfo$headers$`last-modified`, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC")
      
      fileLastMTime <- lubridate::as_datetime(x = file.mtime(ntLtsPageLocalName), tz = "UTC")
      
      #if the file does not exist or is older than the url modified time download it
      if (!file.exists(ntLtsPageLocalName) || file.size(ntLtsPageLocalName) == 0 ||
          fileLastMTime < urlLastMTime)
      {
        utils::download.file(
          url = ntLtsIndexUrlVIIRS,
          destfile = ntLtsPageLocalName,
          method = "auto",
          extra = " -N --timestamping --no-use-server-timestamps"
        )
      }
      #else
      #  message(paste0(ntLtsPageHtml, " already downloaded"))
      
      #read in the html page
      ntLtsPage <- readr::read_lines(ntLtsPageLocalName)
      
      #create the pattern to distinguish the url on the page
      if (stringr::str_detect(nlType, "D"))
        ntLtsPageRgxp <-
        paste0("SVDNB_npp_d",
               "\\d{8}",
               "\\.d\\.",
               "\\d{2}N\\d{3}[W|E]",
               "\\.rade9.*\\.tif")
      else if (stringr::str_detect(nlType, "M"))
        #VIIRS.M has 2 diff tgz files based on configName vcmcfg and vcmslcfg
        ntLtsPageRgxp <-
        paste0("SVDNB_npp_",
               "\\d{6}",
               "01.*",
               "\\d{2}N\\d{3}[W|E]",
               ".*")
      else if (stringr::str_detect(nlType, "Y"))
        ntLtsPageRgxp <-
        paste0("SVDNB_npp_",
               "\\d{4}",
               "0101-",
               "\\d{4}",
               "1231_",
               "\\d{2}N\\d{3}[W|E]",
               ".*tgz")
      
      #search for the pattern in the page
      ntLtsPageHtml <-
        grep(pattern = ntLtsPageRgxp, x = ntLtsPage, value = TRUE)
      
      #split the output on quotes since this url is of the form ...<a href="URL"download> ...
      #the url is in the second position
      ntLtsPageUrls <- unlist(strsplit(ntLtsPageHtml, '"'))
      
      ntLtsPageUrls <- ntLtsPageUrls[seq(2, length(ntLtsPageUrls), by = 3)]
      
      #hard-code since we only have configNames in the downloads for VIIRS.M
      configNames <- getAllNlConfigNames(nlTypes = nlType)$configName
      
      ntLtsPageUrls <- stats::setNames(lapply(configNames, function(configName)
      {
        # ntLtsPageRgxp <-
        #   if(nlType == "VIIRS.D")
        #   {
        #     "\\d{8}"
        #   } else if(nlType == "VIIRS.M")
        #   {
        #     #for VIIRS.M we can search using the configName as is
        #     configName
        #   } else if (nlType == "VIIRS.Y")
        #   {
        #     if(configName == "vcmcfg")
        #      "\\d{4}(?=0101)"
        #     else
        #       NA
        #   }
        
        ntLtsPageRgxp <- unique(getAllNlConfigNames(nlTypes = nlType, configNames = configName)$fileRegex1)
        
        ntLtsPageRgxp <- getNlFileRegex(nlTypes = nlType, configNames = configName, regexNum = "1")
        
        sort(grep(pattern = ntLtsPageRgxp, x = ntLtsPageUrls, value = TRUE, perl = TRUE))
      }), configNames)
      
      return (ntLtsPageUrls)
    }), nlTypes)
  }

getAllNlUrls <-
  function(nlTypes, configNames)
  {
    urlsOLS <- getAllNlUrlOLS()
    urlsVIIRS <- getAllNlUrlVIIRS()
    
    allUrls <- append(urlsOLS, urlsVIIRS)
    
  }

getAvailableNlPeriods <-
  function(nlTypes,
           configNames = "")
  {
    availUrls <- getAllNlUrls(nlTypes = nlTypes, configNames = configNames)
    
    stats::setNames(
      lapply(names(availUrls),
           function(nlType)
           {
             configNames <- getAllNlConfigNames(nlTypes = nlType)$configName
             
             stats::setNames(
               lapply(configNames, function(configName)
             {
               regexp <- if(nlType == "OLS.Y")
               {
                 "(?<=F\\d{2})\\d{4}"
               }else if(nlType == "VIIRS.D")
               {
                 "\\d{8}"
               }else if(nlType == "VIIRS.M")
               {
                 "\\d{6}"
               }else if(nlType == "VIIRS.Y")
               {
                 "\\d{4}(?=0101)"
               }
               
               urls <- availUrls[[nlType]][[configName]]

               unique(stringr::str_extract(string = urls, pattern = regexp))
           }), configNames)
         }),
         
         names(availUrls)
        )
  }
