######################## getNlGasFlaresRdsFnamePath ###################################

#' Get the path to the saved world gasflare RDS
#'
#' Get the path to the saved world gasflare RDS
#' 
#' @return \code{character} the path to the world gasflare RDS
#'
#' @examples
#' \dontrun{
#'   getNlGasFlaresRdsFnamePath(ctryCode="NGA")
#' #returns path to the world RDS
#' }
#'
getNlGasFlaresRdsFnamePath <- function()
{
  return(file.path(getNlDir("dirNlGasFlares"), "gasflaresmosaic.rds"))
}

######################## getNlCtryGasFlaresRdsFnamePath ###################################

#' Get the path to the saved country gasflare RDS
#'
#' Get the path to the saved country gasflare RDS
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @return \code{character} the path to the world gasflare RDS
#'
#' @examples
#' \dontrun{
#'   getNlCtryGasFlaresRdsFnamePath(ctryCode="NGA")
#' #returns path to the world RDS
#' }
#'
getNlCtryGasFlaresRdsFnamePath <- function(ctryCode)
{
  return(file.path(getNlDir("dirNlGasFlares"), paste0("gasflares_", ctryCode, ".rds")))
}

######################## getNlGasFlaresRdsFnamePath ###################################

#' Get the path to the saved world gasflare RDS
#'
#' Get the path to the saved world gasflare RDS
#' 
#' @return None
#'
#' @examples
#' \dontrun{
#'   getNlGasFlaresRdsFnamePath(ctryCode="NGA")
#' #returns path to the world RDS
#' }
#'
createNlGasFlares <- function()
{
  message(Sys.time(), ": removeGasFlares = TRUE. Downloading gas flare country polygons. This will take a while")
  
  gasFlareRdsFnamePath <- getNlGasFlaresRdsFnamePath()
  
  if(!file.exists(gasFlareRdsFnamePath))
  {
    #the page that lists all available nightlight files
    gasFlarePageHtml <- "https://www.ngdc.noaa.gov/eog/interest/gas_flares_countries_shapefiles.html"
    
    #the local name of the file once downloaded
    gasFlarePageLocalName <- file.path(getNlDir("dirNlTemp"), basename(gasFlarePageHtml))
    
    #if the file does not exist or is older than a day download it afresh
    #not working. download.file does not seem to update mtime
    if (!file.exists(gasFlarePageLocalName) || (lubridate::date(lubridate::now()) - lubridate::date(file.mtime(gasFlarePageLocalName)) > lubridate::as.difftime(lubridate::period("1 day"))))
    {
      utils::download.file(url = gasFlarePageHtml, destfile = gasFlarePageLocalName, method = "auto", extra = "-N")
    }
    #else
    #  message(paste0(ntLtsPageHtml, " already downloaded"))
    
    #read in the html page
    gasFlarePage <- xml2::read_html(gasFlarePageLocalName)
    
    gasFlarePage <- rvest::html_nodes(gasFlarePage, "table tr td a")
    
    gasFlareRgxp <- paste0("Flares_.*\\.tgz")
    
    gasFlareNodes <- gasFlarePage[grep(pattern = gasFlareRgxp, x=gasFlarePage)]
    
    gasFlareUrls <- rvest::html_attr(gasFlareNodes,name = "href")
    
    for(gasFlareUrl in gasFlareUrls)
    {
      message(Sys.time(), ": Downloading gas flare polygon ", basename(gasFlareUrl))
      
      gfTgzLclFnamePath <- file.path(getNlDir("dirNlGasFlares"), basename(gasFlareUrl))
      
      if(!file.exists(gfTgzLclFnamePath))
        res <- try(utils::download.file(url = gasFlareUrl, destfile = gfTgzLclFnamePath, method = "auto"), TRUE)
      else
      {
        message(Sys.time(), ": Exists")
        
        res <- 0
      }
      
      if(res == 0)
      {
        message(Sys.time(), ": Extracting ", basename(gasFlareUrl))
        
        gfShpDirLclFnamePath <- tools::file_path_sans_ext(gfTgzLclFnamePath)
        
        if(!dir.exists(gfShpDirLclFnamePath))
        {
          dir.create(gfShpDirLclFnamePath)
        
          utils::untar(tarfile = gfTgzLclFnamePath, exdir = gfShpDirLclFnamePath)
        } else
        {
          message(Sys.time(), ": ", gfShpDirLclFnamePath, " already exists")
        }
      }
      
    }
    
    if(res == 0)
    {
      #all shapefiles downloaded
      message(Sys.time(), ": Creating global gas flare shapefile")
      
      gfPoly <- list()
      
      message(Sys.time(), ": Reading in the downloaded gas flare polygons")
      
      for(gasFlareUrl in gasFlareUrls)
      {
        gfTgzLclFnamePath <- file.path(getNlDir("dirNlGasFlares"), basename(gasFlareUrl))
        
        gfShpDirLclFnamePath <- tools::file_path_sans_ext(x = gfTgzLclFnamePath)
        
        gfPoly <- append(gfPoly, rgdal::readOGR(dsn = gfShpDirLclFnamePath))
      }
      
      message(Sys.time(), ": mosaicing")
      
      gfPolyMosaic <- do.call(raster::bind, gfPoly)
      
      rm(gfPoly)
      
      wgs84 <- getCRS()
      
      raster::projection(gfPolyMosaic) <- sp::CRS(wgs84)
      
      #clean the geometries
      message(Sys.time(), ": cleaning the mosaiced polygon")
      
      gfPolyMosaic <- cleangeo::clgeo_Clean(gfPolyMosaic)
      
      message(Sys.time(), ": Saving mosaiced polygon to RDS")
      
      saveRDS(object = gfPolyMosaic, file = gasFlareRdsFnamePath)
      
      rm(gfPolyMosaic)
      
      gc()
    }
  } else
  {
    message("Gas flare RDS already exists")
    return(TRUE)
  }
}

######################## existsNlGasFlaresRds ###################################

#' Check if the world gasflare RDS exists
#'
#' Check if the world gasflare RDS exists
#' 
#' @return \code{logical} if the gasflare rds exists
#'
#' @examples
#' \dontrun{
#'   existsNlGasFlaresRds()
#' #returns TRUE/FALSE
#' }
#'
existsNlGasFlaresRds <- function()
{
  gasFlareRdsFnamePath <- getNlGasFlaresRdsFnamePath()
  
  return(file.exists(gasFlareRdsFnamePath))
}

######################## getNlGasFlaresRds ###################################

#' Returns the world gasflares polygon if it exists
#'
#' Returns the world gasflares polygon if it exists
#' 
#' @return \code{spPolygons} the country gasflare polygon
#'
#' @examples
#' \dontrun{
#'   getNlGasFlaresRds()
#' #returns mosaiced gasflare polygon
#' }
#'
getNlGasFlaresRds <- function()
{
  if(!existsNlGasFlaresRds())
    createNlGasFlares()
  
  readRDS(getNlGasFlaresRdsFnamePath())
}

######################## hasNlCtryGasFlares ###################################

#' Checks if a country has gasflares
#'
#' Checks if a country has gasflares
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return \code{logical} whether the country has gasflares
#'
#' @examples
#' \dontrun{
#'   hasNlCtryGasFlares(ctryCode="NGA")
#' #returns TRUE
#' }
#'
#' @export
hasNlCtryGasFlares <- function(ctryCode,
                               gadmVersion=pkgOptions("gadmVersion"),
                               gadmPolyType=pkgOptions("gadmPolyType"),
                               custPolyPath=NULL)
{
  ctryPolyAdm0 <- readCtryPolyAdmLayer(ctryCode = ctryCode,
                                       admLevel = unlist(getCtryShpLyrNames(ctryCodes = ctryCode,
                                                                            lyrNums = 0,
                                                                            gadmVersion = gadmVersion,
                                                                            gadmPolyType = gadmPolyType,
                                                                            custPolyPath = custPolyPath)),
                                       gadmVersion = gadmVersion,
                                       custPolyPath = custPolyPath)
  gasFlarePoly <- getNlGasFlaresRds()
  
  return(rgeos::gIntersects(ctryPolyAdm0, gasFlarePoly))
}

######################## hasNlCtryGasFlares ###################################

#' Checks if a country has gasflares
#'
#' Checks if a country has gasflares
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return \code{logical} whether the country has gasflares
#'
#' @examples
#' \dontrun{
#'   hasNlCtryGasFlares(ctryCode="NGA")
#' #returns TRUE
#' }
#'
createNlCtryGasFlares <- function(ctryCode, 
                                  gadmVersion = pkgOptions("gadmVersion"),
                                  gadmPolyType = pkgOptions("gadmPolyType"),
                                  custPolyPath = NULL)
{
  if(!existsNlCtryGasFlaresRds(ctryCode))
  {  
    ctryPolyAdm0 <- readCtryPolyAdmLayer(ctryCode = ctryCode,
                                         admLevel = unlist(getCtryShpLyrNames(ctryCodes = ctryCode,
                                                                              lyrNums = 0,
                                                                              gadmVersion = gadmVersion,
                                                                              gadmPolyType = gadmPolyType,
                                                                              custPolyPath = custPolyPath)),
                                         gadmVersion = gadmVersion,
                                         custPolyPath = custPolyPath)
    gasFlarePoly <- getNlGasFlaresRds()
    
    if(hasNlCtryGasFlares(ctryCode = ctryCode,
                          gadmVersion = gadmVersion,
                          gadmPolyType = gadmPolyType,
                          custPolyPath = custPolyPath))
    {
      if(!file.exists(getNlCtryGasFlaresRdsFnamePath(ctryCode)))
      {
        message(Sys.time(), ": Creating country gas flare removal polygon. Approx time 2 mins")
        
        
        ctryPolyAdm0GFRemoved <- rgeos::gDifference(ctryPolyAdm0, gasFlarePoly)
        
        saveRDS(object = ctryPolyAdm0GFRemoved, file = getNlCtryGasFlaresRdsFnamePath(ctryCode))
        
        ctryPolyAdm0 <- ctryPolyAdm0GFRemoved
      } else
      {
        ctryPolyAdm0 <- readRDS(getNlCtryGasFlaresRdsFnamePath(ctryCode))
      }
    } else
    {
      message(Sys.time(), ": No gas flares for: ", ctryCode)
    }
  } else
  {
    message(Sys.time(), ": Already exists")
  }
}


######################## existsNlCtryGasFlaresRds ###################################

#' Checks if the country gasflares rds exists
#'
#' Checks if the country gasflares rds exists
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @return \code{logical} whether the country has gasflares
#'
#' @examples
#' \dontrun{
#'   existsNlCtryGasFlaresRds(ctryCode="NGA")
#' #returns TRUE
#' }
#'
existsNlCtryGasFlaresRds <- function(ctryCode)
{
  gfNlCtryRdsFnamePath <- getNlCtryGasFlaresRdsFnamePath(ctryCode)
  
  return(file.exists(gfNlCtryRdsFnamePath))
}

######################## getNlCtryGasFlaresPoly ###################################

#' Creates and returns a country's gasflares polygon
#'
#' Creates and returns a country's gasflares polygon
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return \code{spPolygons} the country gasflare polygon
#'
#' @examples
#' \dontrun{
#'   getNlCtryGasFlaresRds(ctryCode="NGA")
#' #returns gasflare polygon
#' }
#'
getNlCtryGasFlaresRds <- function(ctryCode = ctryCode, 
                                  gadmVersion = pkgOptions("gadmVersion"),
                                  gadmPolyType = pkgOptions("gadmPolyType"),
                                  custPolyPath = NULL)
{
  if(!existsNlCtryGasFlaresRds(ctryCode))
    createNlCtryGasFlares(ctryCode = ctryCode,
                          gadmVersion = gadmVersion,
                          gadmPolyType = gadmPolyType,
                          custPolyPath = custPolyPath)
  
  if(existsNlCtryGasFlaresRds(ctryCode))
    res <- readRDS(file = getNlCtryGasFlaresRdsFnamePath(ctryCode))
  else
    res <- NULL
  
  return(res)
}

######################## getNlCtryGasFlaresPoly ###################################

#' Returns a country's gasflares polygon if it exists
#'
#' Returns a country's gasflares polygon if it exists
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param gadmPolyType The format of polygons to download from GADM
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return \code{spPolygons} the country gasflare polygon
#'
#' @examples
#' \dontrun{
#'   getNlCtryGasFlaresPoly(ctryCode="NGA")
#' #returns gasflare polygon
#' }
#'
getNlCtryGasFlaresPoly <- function(ctryCode = ctryCode, 
                                   gadmVersion = pkgOptions("gadmVersion"),
                                   gadmPolyType = pkgOptions("gadmPolyType"),
                                   custPolyPath = NULL)
{
  return(getNlCtryGasFlaresRds(ctryCode = ctryCode,
                               gadmVersion = gadmVersion,
                               gadmPolyType = gadmPolyType,
                               custPolyPath = custPolyPath))
}