#TODO:
#+ gzip all outputrasters and extract/delete tifs as required
#+ delete the 2nd tif in the tiles (avg_rad_...).
#+ keep tiles gzipped until required. extract/delete as needed
#+ modularize everything, processNtLts especially: PARTIALLY DONE
#+ give functions better names more descriptive: PARTIALLY DONE
#+ validation of inputs, error handling: PARTIALLY DONE
#+ give temp files unique names to avoid problems in case of parallelization: DONE
#+ settings and default settings list/DF: PARTIALLY DONE
#+ optimize download of tiles: PARTIALLY DONE aria2
#+ zone files functions
#+ logging
#+ debug mode
#+ do not export internal functions?: PARTIALLY DONE
#+ remove dependency on rworldmap?
#+ aggregating by date e.g. quarterly, semi-annually, annually
#+ verify treatment of ATA i.e. single adm level countries
#+ logic of getCtryPolyAdmLevelNames esp lvlEngName assignment needs scrutiny: DONE
#+ OLS
#+ store data in RDS format instead of CSV?

#Notes: gdalwarp is not used for cropping because the crop_to_cutline option causes a shift in the cell locations which then affects the stats extracted. A gdal-based crop to extent would be highly desirable for performance reasons though so seeking other gdal-based workarounds

# if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')
#
# pacman::p_load(readr, dplyr, lubridate, rgdal, raster, sp, rgeos, rworldmap, cleangeo, foreach, doParallel, compiler, gdalUtils, data.table, ff, validate)
#
# require(readr)
# require(dplyr)
# library(data.table)
# require(ff)
# require(validate)
# 
# require(lubridate)
#
# require(rgdal)
# require(gdalUtils)
# require(raster)
#
# require(sp)
# require(rgeos)
# require(rworldmap)
# require(cleangeo)
#
# require(foreach) #Enables for-each statements to be used for parallel processing
# require(doParallel) #Allows for parallel processing using multiple cores
#
# require(compiler)

compiler::enableJIT(0)

#global constants
map <- rworldmap::getMap()
map <- cleangeo::clgeo_Clean(map)
shpTopLyrName <- "adm0"
#projection system to use
#can we use only one or does it depend on the shapefile loaded?
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#ntLtsIndexUrlVIIRS = "https://www.ngdc.noaa.gov/eog/viirs/download_monthly.html"


######################## RNIGHTLIGHTSOPTIONS ###################################

RNIGHTLIGHTSOPTIONS <- settings::options_manager(
  #Change the temp dir to use e.g. if the system temp dir does not have enough space
  tmpDir = raster::tmpDir(),
  
  ntLtsIndexUrlVIIRS = "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html",
  
  stats = c("sum", "mean", "var"),

  dirNlDataPath = ".Rnightlights",
  
  #Set raster directory path
  dirRasterOLS = "tiles",
  
  #Set directory path
  dirRasterVIIRS = "tiles",
  
  dirRasterOutput = "outputrasters",
  
  dirRasterWeb = "outputrasters_web",
  
  dirZonals = "zonals",
  
  dirPolygon = "polygons",
  
  dirNlData = "data",
  
  #cropMaskMethod" Method used to crop and mask tiles to country polygons. options: "gdal" or "rast" gdal is usually faster but requires gdal to be installed on the system
  cropMaskMethod = "gdal",
  
  extractMethod = "gdal",
  
  #gdal_cachemax Speeds up gdal_rasterize calculation of stats in function ZonalPipe with more cache (advice: max 1/3 of your total RAM) see: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  gdal_cachemax = 2000,
  
  #downloadMethod used options: wget, aria
  downloadMethod = "aria",
  
  omitCountries = "missing",
  
  deleteTiles = FALSE,
  
  .allowed = list(
    cropMaskMethod = settings::inlist("gdal","rast"),
    extractMethod = settings::inlist("gdal", "rast"),
    downloadMethod = settings::inlist("wget", "aria"),
    omitCountries = settings::inlist("error", "missing", "long", "all", "none")
  )
)

getNlDir <- function(dirName)
{
  if(missing(dirName))
    stop("Missing required parameter dirName")
    
  if(!is.character(dirName) || is.null(dirName) || is.na(dirName) || dirName == "")
    stop("Invalid dirName: ", dirName)
  
  dataPath <- getDataPath()
  
  file.path(dataPath, pkg_options("dirNlDataPath"), pkg_options(dirName))
}

######################## pkg_options ###################################

#' Set or get options for the Rnightlights package
#' 
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{dirRasterOLS}}{(\code{character}) The directory in which to store the downloaded OLS raster tiles }
#'  \item{\code{dirRasterVIIRS}}{(\code{character}) The directory in which to store the downloaded VIIRS raster tiles }
#'  \item{\code{dirRasterOutput}}{(\code{character}) The directory in which to store the clipped country rasters }
#'  \item{\code{dirRasterWeb}}{(\code{character}) The directory in which to store the rasters resampled for web display }
#'  \item{\code{dirZonals}}{(\code{character}) The directory in which to store the zonal statistics country polygon }
#'  \item{\code{dirPolygon}}{(\code{character}) The directory to store the downloaded country administration level polygons }
#'  \item{\code{dirNlData}}{(\code{character}) The directory to store the extracted data files in }
#'  \item{\code{cropMaskMethod}}{(\code{character}) The method to use to clip the nightlight raster tiles to the country boundaries }
#'  \item{\code{extractMethod}}{(\code{character}) The method to use to extract data from the rasters }
#'  \item{\code{downloadMethod}}{(\code{character}) The download method to use }
#'  \item{\code{omitCountries}}{(\code{character}) The countries to exclude in processing }
#' }
#' @return if an option name is supplied as a parameter this returns the value, else a list of all options is returned.
#' 
#' @examples
#' pkg_options("cropMaskMethod")
#' 
#' pkg_options()
#' 
#' pkg_options("cropMaskMethod"="gdal")
#' 
#' @export
pkg_options <- function(...)
{
  settings::stop_if_reserved(...)
  
  RNIGHTLIGHTSOPTIONS(...)
}

######################## pkg_reset ###################################

#' Reset global options for the Rnightlights package
#'
#' @export
pkg_reset <- function()
{
  settings::reset(RNIGHTLIGHTSOPTIONS)
}

######################## getNlTiles ###################################

#' Create mapping of VIIRS nightlight tiles
#'
#' Creates a data.frame mapping VIIRS nightlight tile names to their vertice coordinates. This is used to
#'     identify nightlight tiles as well as to build a spatial polygons dataframe used to plot the tiles. OLS
#'     only has one tile for the whole world and thus does not need mapping.
#'
#' @return A data.frame of names of tiles and lon-lat coordinate of top-left corner of each
#'
#' @examples
#' getNlTiles()
#'
getNlTiles <- function()
{
  #6 nightlight tiles named by top-left geo coordinate numbered from left-right & top-bottom
  #creates columns as strings. createSpPolysDF converts relevant columns to numeric
  nlTiles <- as.data.frame(
    cbind(id=c(1,2,3,4,5,6), 
          name=c("75N180W", "75N060W", "75N060E", "00N180W", "00N060W", "00N060E"),
          minx=c(-180, -60, 60, -180, -60, 60), maxx=c(-60, 60, 180, -60, 60, 180),
          miny=c(0, 0, 0, -75, -75, -75), maxy=c(75, 75, 75, 0, 0, 0)
          ), 
    stringsAsFactors=FALSE)

  return (nlTiles)
}

######################## existsNlTiles ###################################

#' Checks if the nlTiles data.frame variable exists
#'
#' Checks if the nlTiles data.frame variable exists in the environment and that it is not null or empty. This is used by other functions that depend on the nlTiles data.frame to check if it exists. If it doesn't they may create the data.frame.
#'
#' @return TRUE/FALSE
#'
#' @examples
#' if(!existsNlTiles())
#'   nlTiles <- getNlTiles()
#'
existsNlTiles <- function()
{
  if (exists("nlTiles") && class(nlTiles) == "data.frame" && !is.null(nlTiles) && nrow(nlTiles) > 0)
    return (TRUE)
  else
    return (FALSE)
}

######################## existsTilesSpPolysDFs ###################################

#' Checks if the variable \code{"tilesSpPolysDFs"} exists
#'
#' Checks if the \code{"tilesSpPolysDFs"} (tile Spatial Polygons DataFrame) variable exists in the environment   and that it is not null. This is used by other functions that depend on the \code{"tilesSpPolysDFs"} data.frame to check if it exists. If it doesn't they can create the data.frame.
#'
#' @return TRUE/FALSE
#'
#' @examples
#' if(!existsTilesSpPolysDFs())
#'   tilesSpPolysDFs <- createNlTilesSpPolysDF()
#'
existsTilesSpPolysDFs <- function()
{
  if (exists("tilesSpPolysDFs") && class(tilesSpPolysDFs) == "SpatialPolygonsDataFrame" && !is.null(tilesSpPolysDFs))
    return(TRUE)
  else
    return(FALSE)
}

######################## createNlTilesSpPolysDF ###################################

#' Creates a tile Spatial Polygons DataFrame from the \code{"nlTiles"} dataframe
#'
#' Creates a Spatial Polygons DataFrame from the \code{"nlTiles"} dataframe of VIIRS tiles
#'
#' @return TRUE/FALSE
#'
#' @examples
#'   tilesSpPolysDFs <- createNlTilesSpPolysDF()
#'
createNlTilesSpPolysDF <- function()
{
  if (!existsNlTiles())
  {
    nlTiles <- getNlTiles()
  }

  #convert nlTiles min/max columns to numeric
  for (cIdx in grep("id|min|max", names(nlTiles))) nlTiles[,cIdx] <- as.numeric(as.character(nlTiles[, cIdx]))

  #create the empty obj to hold the data frame of tile PolygonsDataFrams
  tilesSpPolysDFs <- NULL

  #for each row in nlTiles
  for (i in 1:nrow(nlTiles))
  {
    #grab the row containing the tile
    t <- nlTiles[i,]

    #convert the tile x,y extents to a matrix
    #format is 2 cols x & y
    tMat <- as.matrix(cbind(rbind(t$minx, t$maxx, t$maxx, t$minx), rbind(t$maxy, t$maxy, t$miny, t$miny)))

    #create a Polygon object from the tile extents matrix
    tPoly <- list(sp::Polygon(tMat))

    #create a Polygons object with a list of 1 polygon
    tPolys <- sp::Polygons(srl = tPoly, ID = i)

    #create a SpatialPolygons object with a list of 1 list of Polygons
    tilesSpPolys <- sp::SpatialPolygons(Srl = list(tPolys))

    #we assign the CRS at this point (note other objects cannot be assigned CRS)
    raster::projection(tilesSpPolys) <- sp::CRS(wgs84)

    #convert the SpatialPolygons object into a SpatialPolygonsDataFrame
    #tilesSpPolysDF <- as(tilesSpPolys, "SpatialPolygonsDataFrame")
    
    #z used for plotCtryWithTiles to color the tiles
    tilesSpPolysDF <- sp::SpatialPolygonsDataFrame(tilesSpPolys, data.frame(z=factor(i), name=nlTiles[i,"name"], row.names=i))

    #append the SPDF into a dataframe of SPDFs
    if (is.null(tilesSpPolysDFs))
      tilesSpPolysDFs <- tilesSpPolysDF
    else
      tilesSpPolysDFs <- sp::rbind.SpatialPolygonsDataFrame(tilesSpPolysDFs, tilesSpPolysDF)
  }
  return (tilesSpPolysDFs)
}

######################## plotCtryWithTiles ###################################

#' Plot a country polygon against a background of the VIIRS tiles and world map
#'
#' Plot a country polygon as defined in the \pkg{rworldmap} package along with the VIIRS 
#'     nightlight tiles for a visual inspection of the tiles required for download in order 
#'     to process a country's nightlight data.
#'     
#'     It utilizes \code{rworldmap::rwmgetISO3} to resolve country codes as well as names
#'
#' @param idx character string or integer either the index of the country polygon in 
#'     \code{rworldmap::getMap()} or the 3-letter ISO3 country code e.g. "KEN" or a common 
#'     name of the country e.g. "Kenya" as found valid by \code{rworldmap::rwmgetISO3}
#'
#' @return None
#'
#' @examples
#' plotCtryWithTiles("KEN")
#'
#' plotCtryWithTiles(85)
#' 
#' plotCtryWithTiles("85")
#'
#' @export
plotCtryWithTiles <- function(idx)
{
  if(missing(idx))
    stop("You must supply a country code or index")
  
  if(!is.character(idx) && !is.numeric(idx))
    stop("The parameter you supplied needs to be type numeric or character")
  
  #if the map variable does not exist
  if(!exists("map"))
  {
    #get the map from the rworldmap package
    map <- rworldmap::getMap()

    #some rworldmap polygons have problems. Rectify them to allow plotting without errors
    map <- cleangeo::clgeo_Clean(map)
  }

  #if the tiles spatial polygons dataframe does not exist create it
  if(!existsTilesSpPolysDFs())
    tilesSpPolysDFs <- createNlTilesSpPolysDF()

  #if idx is numeric we assume it is the index of the country polygon in the map
  if (is.numeric(idx))
  {
    #idx cannot be less than zero or greater than the number of polygons in the map
    if(idx < 0 || idx > length(map@polygons))
    {
      #invalid index
      return("Index out of range")
    }
  }
  else if (is.character(idx)) #try if it is a character ISO3 code
  {
    if (suppressWarnings(!is.na(as.numeric(idx))))
    {
      idx <- as.numeric(idx)
    }
    else
    {
      #valid index so get the corresponding ISO3 country code
      ctryISO3 <- rworldmap::rwmGetISO3(idx)
  
      #print(ctryISO3)
  
      #if ctryISO3 is empty then the country was not found
      if (is.na(ctryISO3) || ctryISO3 == "")
        return("Country code/name not found")
  
      #otherwise we have a valid country ISO3 code. get its index
      idx <- which(as.character(map@data$ISO3) == ctryISO3)
    }
  }
  else
  {
    return("invalid type")
  }

  #At this point we have a valid index number

  #get the polygon that matches the index
  ctryPolys <- map@polygons[[idx]]
  
  #get the name of the polygon
  ctryPolyTitle <- paste0("VIIRS Nightlight Tiles Required for:\n", map@data$ADMIN[[idx]], " (", map@data$ISO3[[idx]], ")")

  #create a SpatialPolygons object with the list of Polygons
  ctrySpPolys <- sp::SpatialPolygons(Srl = list(ctryPolys))

  #set the coordinate reference system
  raster::projection(ctrySpPolys) <- sp::CRS(wgs84)

  #convert the spatial polygons to an SPsDF
  ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")

  #set the 4 margins to 2 inches from the border to avoid boundary errors
  #graphics::par(mar=rep(2,4))

  #plot the tiles first
  #sp::plot(tilesSpPolysDFs, main=ctryPolyName)
  

  #plot the country on the same plot and fill with blue
  #sp::plot(ctrySpPolysDF, col="blue", add=TRUE)
  
  #get the extents of the SpatialPolygonsDataFrame. Used to draw a bounding box around the plotted country. Especially helpful for very small countries
  e <- raster::extent(ctrySpPolysDF)
  
  #draw back the boundaries by 10 units
  e@xmin <- e@xmin - 10
  e@xmax <- e@xmax + 10
  e@ymin <- e@ymin - 10
  e@ymax <- e@ymax + 10
  
  #convert the Extents object to a SpatialLines object for spplot to use
  extents <- as(e, 'SpatialLines')
  
  #get a list of the intersecting tiles. Used to highlight tiles which intersect with plotted country
  tilesIntersected <- tileName2Idx(getTilesCtryIntersect(map@data$ISO3[[idx]]))
  
  #create a list which serves as a subtitle showing the mapping of tile index to tile name
  tileIdxNames <- paste(tilesSpPolysDFs@data$z, tilesSpPolysDFs@data$name, sep = "=")
  
  #plot the map
  sp::spplot(tilesSpPolysDFs, #the tiles SPDF
             zcol = "z", #the col in the tiles SPDF which determines their color
             col.regions = as.vector(ifelse(1:nrow(tilesSpPolysDFs) %in% tilesIntersected, "lightblue", "transparent")), #colors of the tiles. intersected tiles are lightblue, otherwise transparent
             colorkey = FALSE,
             sp.layout = list(list(map, col='grey', fill='transparent', first=FALSE), #plot the world map from rworldmap
                              list(ctrySpPolysDF, col='black', fill='blue', first=FALSE), #plot the selected country
                              list('sp.lines', extents, col='green', lwd=2), #plot the bounding box
                              list('sp.text', sp::coordinates(tilesSpPolysDFs), 1:nrow(tilesSpPolysDFs), col='black', cex=2) #label the tiles with their index numbers
                              ),
             main = ctryPolyTitle, #the main title
             sub = tileIdxNames #the sub title 
             )
  
  #ggplot(tilesSpPolysDFs, aes(x=long,y=lat))+geom_polygon(col="black", fill="white", alpha=0.5)#+geom_polygon(data=ctrySpPolysDF, alpha=0.5)
  #ggplot(ctrySpPolysDF, aes(x=long,y=lat, group=group))+geom_polygon(col="black", fill="white",alpha=0.5)

  #a <- spplot(tilesSpPolysDFs, main=map@polygons[[idx]]@ID)
  #b <- spplot(ctrySpPolysDF)

  #a+as.layer(b)
}

######################## mapAllCtryPolyToTiles ###################################

#' Create a mapping of all countries and the tiles they intersect
#'
#' This is simply another name for mapCtryPolyToTiles with ctryCodes="all"
#'
#' @param omitCountries A character vector or list of countries to leave out when processing. Default is #'     \code{"none"}
#'
#' @return None
#'
#' @examples
#' mapAllCtryPolyToTiles() no countries omitted
#'
#' mapAllCtryPolyToTiles(omitCountries="none") no countries omitted
#'
#' mapAllCtryPolyToTiles(omitCountries=c("error", "long")) will not omit countries that take long to process
#'
mapAllCtryPolyToTiles <- function(omitCountries=pkg_options("omitCountries"))
{
  mapCtryPolyToTiles(ctryCodes="all", omitCountries)
}

######################## mapCtryPolyToTiles ###################################

#' Create a mapping of all countries and the tiles they intersect
#'
#' Create a dataframe mapping each country in the rworldmap to the VIIRS tiles which they intersect with
#'     and thus need to be retrieved to process their nightlight imagery. Since some functions use this
#'     dataframe for long-term processing, omitCountries can eliminate countries that should be excluded
#'     from the list hence from processing. Countries can be added in the omitCountries function.
#'     Default is "none".
#'
#' @param ctryCodes A character vector or list of countries to map. Default is \code{"all"}
#' @param omitCountries A character vector or list of countries to leave out. Default is \code{"none"}
#'
#' @return ctryCodeTiles A data frame of countries and the tiles they intersect with as give by getNlTiles()
#'
#' @examples
#' mapCtryPolyToTiles() map all countries
#'
#' mapCtryPolyToTiles(ctryCodes="all", omitCountries="none") map all countries, no countries omitted
#'
#' mapCtryPolyToTiles(omitCountries=c("error", "missing")) will not omit countries that do not have polygons
#' on GADM
#'
mapCtryPolyToTiles <- function(ctryCodes="all", omitCountries=pkg_option("omitCountries"))
{
  
  #if ctryCodes is "all" otherwise consider ctryCodes to be a list of countries
  if (length(ctryCodes) == 1 && tolower(ctryCodes) == "all")
  {
    #get list of all country codes
    ctryCodes <- getAllNlCtryCodes(omitCountries)
  }

  #if the rworldmap::getMap() hasn't been loaded, load it
  if (!exists("map"))
  {
    map <- rworldmap::getMap()

    map <- cleangeo::clgeo_Clean(sp = map)
  }

  #get the indices of the country polygons from the rworldmap
  ctryCodeIdx <- which(map@data$ISO3 %in% ctryCodes)

  ctryCodeTiles <- NULL

  #for each ctryCode index
  for (i in ctryCodeIdx)
  {
    #get the matching polygon
    ctryPolys <- map@polygons[[i]]

    #create a SpatialPolygons object with a list of 1 list of Polygons
    ctrySpPolys <- sp::SpatialPolygons(Srl = list(ctryPolys))

    #set the CRS
    raster::projection(ctrySpPolys) <- sp::CRS(wgs84)

    #convert the SpatialPolygons to a SpatialPolygonsDataFrame
    ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")

    #find the tiles the SPDF intersects with and add to the list of tiles
    ctryCodeTiles <- rbind(ctryCodeTiles, list(tilesPolygonIntersect(ctrySpPolys)))
  }

  #combine the ctryCodes and intersecting tile columns into a dataframe
  ctryCodeTiles <- as.data.frame(cbind(code = as.character(ctryCodes), tiles = ctryCodeTiles))

  #name the columns
  names(ctryCodeTiles) <- c("code", "tiles")

  #convert the code column to character since it is picked as factor
  ctryCodeTiles$code <- as.character(ctryCodeTiles$code)

  #return the data frame
  return(ctryCodeTiles)
}

######################## getTilesCtryIntersect ###################################

#' Get a list of tiles that a country polygon intersects with
#'
#' Create a dataframe mapping each country in the rworldmap to the VIIRS tiles which they intersect with
#'     and thus need to be retrieved to process their nightlight imagery. Since some functions use this
#'     dataframe for long-term processing, omitCountries can eliminate countries that should be excluded
#'     from the list hence from processing. Countries can be added in the omitCountries function.
#'     Default is "none".
#'
#' @param ctryCode The country's ISO3 code
#'
#' @return None
#'
#' @examples
#' mapCtryPolyToTiles() map all countries
#'
#' mapCtryPolyToTiles(ctryCodes="all", omitCountries="none") map all countries, no countries omitted
#'
#' mapCtryPolyToTiles(omitCountries=c("error", "missing")) will not omit countries that do not have polygons
#' on GADM
#'
getTilesCtryIntersect <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing equired parameter ctryCode")
  
  ctryCode <- as.character(ctryCode)
  
  if(!validCtryCode(ctryCode))
    stop("Invalid/Unknown ctryCode: ", ctryCode)
  
  ctryISO3 <- rworldmap::rwmGetISO3(ctryCode)

  #print(ctryISO3)

  if (is.na(ctryISO3) || ctryISO3 == "")
    return("Unknown country")

  idx <- which(map@data$ISO3 == ctryISO3)

  ctryCodeTiles <- NULL

  ctryPolys <- map@polygons[[idx]]

  #create a SpatialPolygons object with a list of 1 list of Polygons
  ctrySpPolys <- sp::SpatialPolygons(Srl = list(ctryPolys))

  raster::projection(ctrySpPolys) <- sp::CRS(wgs84)

  ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")

  ctryCodeTiles <- tilesPolygonIntersect(ctrySpPolys)

  #Plot for debug
  #plot(tilesSpPolysDFs, add=TRUE)
  #plot(ctrySpPolysDF, add=TRUE)

  return (ctryCodeTiles)
}

######################## validNlTileNameVIIRS ###################################

#' Check if a tile index name is valid for a given nightlight type
#'
#' Check if a tile number is valid for a given nightlight type. Note tile num is only valid for
#' "VIIRS" nightlight type
#'
#' @param nlTileNum the index of the tile
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validNlTileNameVIIRS("00N060W","VIIRS")
#'  returns TRUE
#'
#' validNlTileNameVIIRS("9","VIIRS")
#'  returns FALSE
#'
#' validNlMonthName("","OLS")
#'  returns FALSE
#'
validNlTileNameVIIRS <- function(tileName)
{
  if(missing(tileName))
    stop("Missing required parameter tileName")
  
  if(!is.character(tileName) || is.null(tileName) || is.na(tileName) || tileName == "")
    stop("Invalid tileName: ", tileName)
  
  if(length(tileName2Idx(tileName)) != 0)
    return(TRUE)
  else
    return(FALSE)
}

######################## tileName2Idx ###################################

#' Get the index of a tile given its name
#'
#' Get the index of a VIIRS tile as given by getNlTiles() given its name
#'
#' @param tile name as given by getNlTiles()
#'
#' @return Integer index of the tile
#'
#' @examples
#' tileIdx <- tileName2Idx("00N060W")
#'
tileName2Idx <- function(tileName)
{
  if (missing(tileName))
    stop("Missing required parameter tileName")

  if(!is.character(tileName) || is.null(tileName) || is.na(tileName) || tileName == "")
    stop("Invalid tileName: ", tileName)

  if (!existsNlTiles())
    nlTiles <- getNlTiles()

  return (which(nlTiles$name %in% tileName))
}

######################## tileIdx2Name ###################################

#' Get the name of a tile given its index
#'
#' Get the name of a VIIRS tile as given by getNlTiles() given its index
#'
#' @param tile index as given by getNlTiles()
#'
#' @return Character name of the tile
#'
#' @examples
#' tileIdx <- tileName2Idx("00N060W")
#'
tileIdx2Name <- function(tileNum)
{
  if(missing(tileNum))
    stop("Missing required parameter tileNum")
  
  if(!validNlTileNumVIIRS(tileNum))
    stop("Invalid tileNum: ", tileNum)
  
  if (!existsNlTiles())
    nlTiles <- getNlTiles()

  #return (nlTiles[tileNum, "name"])
  return(nlTiles[tileNum, "name"])
}

######################## tilesPolygonIntersect ###################################

#' Get the list of VIIRS tiles that a polygon intersects with
#'
#' Get the list a VIIRS tiles that a polygon intersects with
#'
#' @param a SpatialPolygon or SpatialPolygons
#'
#' @return Character vector of the intersecting tiles as given by getNlTiles()
#'
#' @examples
#' ctryShapefile <- dnldCtryPoly("KEN")
#' ctryPoly <- rgdal::readOGR(getPolyFnamePath("KEN"), getCtryShpLyrName("KEN",0))
#' tileList <- tilesPolygonIntersect(ctryPoly)
#'
tilesPolygonIntersect <- function(shpPolygon)
{
  if(missing(shpPolygon))
    stop("Missing required parameter shpPolygon")
  
  #given a polygon this function returns a list of the names of the viirs tiles
  #that it intersects with
  #Input: a Spatial Polygon e.g. from a loaded shapefile
  #Output: a character vector of tile names as given in the nlTiles dataframe

  if (!existsTilesSpPolysDFs())
  {
    tilesSpPolysDFs <- createNlTilesSpPolysDF()
  }

  if (!existsNlTiles())
    nlTiles <- getNlTiles()

  raster::projection(shpPolygon) <- sp::CRS(wgs84)

  #init list to hold tile indices
  tileIdx <- NULL

  #loop through the 6 tile rows in our SpatialPolygonsDataFrame
  for (i in 1:nrow(tilesSpPolysDFs))
  {
    #check whether the polygon intersects with the current tile
    tileIdx[i] <- rgeos::gIntersects(tilesSpPolysDFs[i,], shpPolygon)
  }

  #return a list of tiles that intersected with the SpatialPolygon
  return (nlTiles[tileIdx, "name"])
}

######################## getNtLts : TO DELETE ###################################

#' Get the list of VIIRS tiles that a polygon intersects with
#'
#' Get the list of VIIRS tiles that a polygon intersects with
#'
#' @param a SpatialPolygon or SpatialPolygons
#'
#' @return Character vector of the intersecting tiles as given by getNlTiles()
#'
#' @examples
#' ctryShapefile <- dnldCtryPoly("KEN")
#' ctryPoly <- rgdal::readOGR(getPolyFnamePath("KEN"), getCtryShpLyrName("KEN",0))
#' tileList <- tilesPolygonIntersect(ctryPoly)
#'
getNtLts <- function(inputYear)
{
  #dmsp/ols data from 1992-2013
  #snpp/viirs data from 2014 - present

  if (inputYear < 1992 || inputYear > year(now()))
    return ("Invalid year")

  if (inputYear < 2014)
    print("Downloading from DMSP/OLS")
  else
    print("Downloading from SNPP/VIIRS")
}

######################## getNtLtsUrlVIIRS ###################################

#' Function to return the url of the VIIRS tile to download
#'
#' Function to return the url of the VIIRS tile to download given the year, month, and nlTile index
#'
#' @param inYear
#'
#' @param inMonth character month in MM format e.g. Jan="01", Feb="02"
#'
#' @param tileNum The integer index of the tile to download as given by getNlTiles()
#'
#' @return Character string Url of the VIIRS tile file
#'
#' @examples
#' tileUrl <- getNtLtsUrlVIIRS("2012", "04", "1")
#'
getNtLtsUrlVIIRS <- function(inYear, inMonth, tileNum)
{
  if(missing(inYear))
    stop("Missing required parameter inYear")
  
  if(missing(inMonth))
    stop("Missing required parameter inMonth")
  
  if(missing(tileNum))
    stop("Missing required parameter tileNum")
  
  if (!existsNlTiles())
    nlTiles <- getNlTiles()

  inYear <- as.character(inYear)

  nMonth <- as.character(inMonth)

  #nlTile is a global list

  #the page that lists all available nightlight files NOTE: URL CHANGE from "https://www.ngdc.noaa.gov/eog/viirs/download_mon_mos_iframe.html"
  #ntLtsPageHtml <- "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html"

  ntLtsIndexUrlVIIRS <- pkg_options("nltLtsIndexUrlVIIRS")
  
  #the local name of the file once downloaded
  ntLtsPageLocalName <- "ntltspageviirs.html"

  #if the file does not exist or is older than a week download it afresh
  #not working. download.file does not seem to update mtime
  if (!file.exists(ntLtsPageLocalName) || (lubridate::date(lubridate::now()) - lubridate::date(file.mtime(ntLtsPageLocalName)) > lubridate::as.difftime(lubridate::period("1 day"))) || file.size(ntLtsPageLocalName) == 0)
  {
    utils::download.file(url = ntLtsIndexUrlVIIRS, destfile = ntLtsPageLocalName, method = "wget", extra = "  -N --timestamping --no-use-server-timestamps")
  }
  #else
  #  print(paste0(ntLtsPageHtml, " already downloaded"))

  #read in the html page
  ntLtsPage <- readr::read_lines(ntLtsPageLocalName)

  #search for a line containing the patterns that make the files unique i.e.
  #1. SVDNB_npp_20121001 - year+month+01
  #2. vcmcfg - for file with intensity as opposed to cloud-free counts (vcmslcfg)
  #sample url: https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201210/vcmcfg/SVDNB_npp_20121001-20121031_75N180W_vcmcfg_v10_c201602051401.tgz

  #create the pattern
  ntLtsPageRgxp <- paste0("SVDNB_npp_", inYear, inMonth, "01.*", nlTiles[tileNum,"name"], ".*vcmcfg")

  #search for the pattern in the page
  ntLtsPageHtml <- ntLtsPage[grep(pattern = ntLtsPageRgxp, x=ntLtsPage)]

  #split the output on quotes since this url is of the form ...<a href="URL"download> ...
  #the url is in the second position
  ntLtsPageUrl <- unlist(strsplit(ntLtsPageHtml, '"'))[2]

  #****NOTE: temp for testing using local download****
  #
  #fname <- stringr::str_extract(ntLtsPageUrl, "SVDNB.*.tgz")
  #ntLtsPageUrl <- paste0("http://localhost/", fname)
  #
  #****DELETE WHEN DONE****

  return (ntLtsPageUrl)
}

######################## validNlYearNum ###################################

#' Check if a year is valid for a given nightlight type
#'
#' Check if a year is valid for a given nightlight type
#'
#' @param year
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validNlYearNum("2014","VIIRS")
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
    if (nlY > 1994 && nlY < 2013)
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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
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

######################## validNlYearMonthVIIRS ###################################

#' Check if a VIIRS yearmonth is valid
#'
#' Check if a VIIRS yearmonth is valid Note this function is only valid for the "VIIRS" nightlight type
#'
#' @param nlYearMonth the yearmonth in "YYYYMM" format e.g. "201210", "201401"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validNlYearMonthVIIRS("201512")
#'  returns TRUE
#'
#' validNlYearMonthVIIRS("201513")
#'  returns FALSE
#'
#' validNlYearMonthVIIRS("201201")
#'  returns FALSE #VIIRS starts in "201204"
#'
validNlYearMonthVIIRS <- function(nlYearMonth)
{
  if (missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")
  
  nlYearMonth <- as.character(nlYearMonth)
  
  if (class(nlYearMonth) != "character" || nlYearMonth =="" || length(grep("[^[:digit:]]", nlYearMonth) > 0))
    return(FALSE)
  
  nlY <- as.numeric(substr(nlYearMonth, 1, 4))
  nlM <- as.numeric(substr(nlYearMonth, 5, 6))
  
  if (validNlYearNum(yearNum = nlY, nlType = "VIIRS") && validNlMonthNum(monthNum = nlM, nlType = "VIIRS"))
    return(TRUE)
  else
    return(FALSE)
}

######################## validNlTileNumVIIRS ###################################

#' Check if a tile index number is valid for a given nightlight type
#'
#' Check if a tile number is valid for a given nightlight type. Note tile num is only valid for
#' "VIIRS" nightlight type
#'
#' @param nlTileNum the index of the tile
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validNlTileNumVIIRS("1")
#'  returns TRUE
#'
#' validNlTileNumVIIRS("9")
#'  returns FALSE
#'
validNlTileNumVIIRS <- function(nlTileNum)
{
  nlTileNum <- as.character(nlTileNum)

  if (missing(nlTileNum))
    stop("Missing parameter nlTileNum")

  if (class(nlTileNum) != "character" || nlTileNum =="" || length(grep("[^[:digit:]]", nlTileNum) > 0))
    return(FALSE)

  if(!exists("nlTiles"))
    nlTiles <- getNlTiles()

  nlT <- as.numeric(nlTileNum)

  if (nlT >= 1 && nlT <= length(nlTiles))
    return(TRUE)
  else
    return(FALSE)
}

######################## getNtLtsZipLclNameVIIRS ###################################

#' Constructs the filename used to save/access the downloaded VIIRS tile .tgz file
#'
#' Constructs the filename used to save/access the downloaded VIIRS tile .tgz file
#'
#' @param nlYear the year in which the tile was created
#'
#' @param nlMonth the month in which the tile was created
#'
#' @param tileNum the index of the tile as given in nlTileIndex
#'
#' @param dir the directory storing the tiles. Defaults to the global variable dirRasterVIIRS
#'
#' @return a character string filename of the compressed .tgz VIIRS tile
#'
#' @examples
#' getNtLtsZipLclNameVIIRS("2014", "01", "1")
#'  returns "./tiles/viirs_2014_01_75N180W.tgz"
#'
getNtLtsZipLclNameVIIRS <- function(nlYear, nlMonth, tileNum, dir=getNlDir("dirRasterVIIRS"))
{
  nlType <- "VIIRS"
  
  if(missing(nlYear))
    stop("Missing required parameter nlYear")
  
  if(missing(nlMonth))
    stop("Missing required parameter nlMonth")
  
  if(missing(tileNum))
    stop("Missing required parameter tileNum")

  if(!validNlYearNum(nlYear, nlType))
    stop("Invalid nlYear: ", nlYear)
  
  if(!validNlMonthNum(nlMonth, nlType))
    stop("Invalid nlMonth: ", nlMonth)
  
  if(!validNlTileNumVIIRS(tileNum))
    stop("Invalid tileNum: ", tileNum)

  #TODO: create function to return the filename
  #TODO: rename this function since it returns a path not a filename
  return (paste0(dir, "/viirs_", nlYear, "_", nlMonth, "_", tileIdx2Name(tileNum), ".tgz"))
}

######################## getNtLtsTifLclNameVIIRS ###################################

#' Constructs the filename used to save/access the decompressed VIIRS .tif file
#'
#' Constructs the filename used to save/access the decompressed VIIRS .tif file
#'
#' @param nlYear the year in which the tile was created
#'
#' @param nlMonth the month in which the tile was created
#'
#' @param tileNum the index of the tile as given in nlTileIndex
#'
#' @param dir the directory storing the tiles. Defaults to the global variable dirRasterVIIRS
#'
#' @return a character vector filename of the .tif VIIRS tile
#'
#' @examples
#' #using default dirRasterVIIRS
#' getNtLtsTifLclNameVIIRS("2014", "01", "1")
#'  returns "./tiles/viirs_2014_01_75N180W.tif"
#'
#'
getNtLtsTifLclNameVIIRS <- function(nlYear, nlMonth, tileNum, dir=getNlDir("dirRasterVIIRS"))
{
  nlType <- "VIIRS"
  
  if(missing(nlYear))
    stop("Missing required parameter nlYear")
  
  if(missing(nlMonth))
    stop("Missing required parameter nlMonth")
  
  if(missing(tileNum))
    stop("Missing required parameter tileNum")
  
  if(!validNlYearNum(nlYear, nlType))
    stop("Invalid nlYear: ", nlYear)
  
  if(!validNlMonthNum(nlMonth, nlType))
    stop("Invalid nlMonth: ", nlMonth)
  
  if(!validNlTileNumVIIRS(tileNum))
    stop("Invalid tileNum: ", tileNum)

  if (missing(dir) && !dir.exists(dir))
  {
    message("Invalid directory ", dir ,". Using default directory \"", getwd(), "/tiles\"")

    #TODO: this is not correct! Not the right place to create the directory!
    if (!dir.exists(dir))
    {
      message("creating raster tiles directory")

      dir.create("tiles")
    }
  }

  #TODO: create function to return the filename
  #TODO: rename this function since it returns a path not a filename
  return (paste0(dir, "/viirs_", nlYear, "_", nlMonth, "_", tileIdx2Name(tileNum), ".tif"))
}

######################## getNtLtsVIIRS ###################################

#' Download VIIRS nightlight tile
#'
#' Download VIIRS nightlight tile
#'
#' @param nlYear the year in "YYYY" format e.g. "2012"
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#'
#' @param tileNum the index of the tile as given by getNlTiles()
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' if(getNtLtsVIIRS("2012", "05", "1"))
#'   print("download successful")
#'
getNtLtsVIIRS <- function(nlYear, nlMonth, tileNum, downloadMethod=pkg_options("downloadMethod"))
{
  nlType <- "VIIRS"

  if(missing(nlYear))
    stop("Missing required parameter nlYear")
  
  if(missing(nlMonth))
    stop("Missing required parameter nlMonth")
  
  if(missing(tileNum))
    stop("Missing required parameter tileNum")
  
  if(!validNlYearNum(nlYear, nlType))
    stop("Invalid nlYear: ", nlYear)
  
  if(!validNlMonthNum(nlMonth, nlType))
    stop("Invalid nlMonth: ", nlMonth)
  
  if(!validNlTileNumVIIRS(tileNum))
    stop("Invalid tileNum: ", tileNum)
  
  rsltDnld <- NA

  ntLtsZipLocalNameVIIRS <- getNtLtsZipLclNameVIIRS(nlYear, nlMonth, tileNum)
  ntLtsTifLocalNameVIIRS <- getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)

  #if (!file.exists(ntLtsZipLocalNameVIIRS) && !file.exists(ntLtsTifLocalNameVIIRS))
  if (!file.exists(ntLtsTifLocalNameVIIRS))
  {
    ntLtsFileUrl <- getNtLtsUrlVIIRS(nlYear, nlMonth, tileNum)

    if (!(downloadMethod %in% c("wget", "aria")))
      downloadMethod <- "wget"

    if (downloadMethod == "wget")
      rsltDnld <- download.file(ntLtsFileUrl, ntLtsZipLocalNameVIIRS, mode = "wb", method = "wget", extra = "-c")
    else if (downloadMethod == "aria")
      rsltDnld <- system(paste0("aria2c -c -x2 ", ntLtsFileUrl, " -o ", ntLtsZipLocalNameVIIRS))
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    print("File exists, set Overwrite = TRUE to overwrite")

    rsltDnld <- 0
  }

  if (rsltDnld == 0)
  {
    message("Extracting ", ntLtsZipLocalNameVIIRS, " ", base::date())

    if (!file.exists(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)))
    {
      message("Getting list of files in ", ntLtsZipLocalNameVIIRS, " ", base::date())

      tgzFileList <- utils::untar(ntLtsZipLocalNameVIIRS, list = TRUE)
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")

      if (is.null(tgzFileList))
      {
        message("Error extracting file list. ")

        return (-1)
      }

      tgzAvgRadFilename <- tgzFileList[grep("svdnb.*.avg_rade9.tif$",tgzFileList, ignore.case = T)]

      message("Decompressing ", tgzAvgRadFilename, " ", base::date())

      if(!file.exists(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)))
      {
        utils::untar(ntLtsZipLocalNameVIIRS, files = tgzAvgRadFilename, exdir = getNlDir("dirRasterVIIRS"))

        file.rename(file.path(getNlDir("dirRasterVIIRS"), tgzAvgRadFilename), getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum))

        file.remove(ntLtsZipLocalNameVIIRS)
      }
    }
    else
    {
      message("TIF file found")
    }
  }

  return (rsltDnld == 0)
}

######################## masqVIIRS ###################################

#' extract data from one polygon in a multipolygon
#'
#' extract data from a polygon in a multipolygon. Modified from \url{https://commercedataservice.github.io/tutorial_viirs_part1/}
#'
#' @param shp the country Polygon layer as SpatialPolygon
#'
#' @param rast the clipped country raster
#'
#' @param i the index of the polygon in the country polygon layer (shp)
#'
#' @return numeric vector of radiances
#'
#' @examples
#' ctryPoly <- rgdal::readOGR('path/to/polygon.shp')
#' 
#' ctryRaster <- raster::raster('path/to/raster.tif')
#' 
#' #get the sum of nightlight pixels in the first polygon in a multipolygon
#' sumPolygon1 <- sum(masqVIIRS(ctryPoly, ctryRaster, 1), na.rm=T)
#'
masqVIIRS <- function(ctryPoly, ctryRast, idx)
{
  #based on masq function from https://commercedataservice.github.io/tutorial_viirs_part1/
  #slightly modified to use faster masking method by converting the raster to vector
  #Extract one polygon based on index value i
  polygon <- ctryPoly[idx,] #extract one polygon

  extent <- raster::extent(polygon) #extract the polygon extent

  #Raster extract
  outer <- raster::crop(ctryRast, extent) #extract raster by polygon extent

  #inner <- mask(outer,polygon) #keeps values from raster extract that are within polygon

  inner <- raster::rasterize(polygon, outer, mask=TRUE) #crops to polygon edge & converts to raster

  #Convert cropped raster into a vector
  #Specify coordinates
  #coords <- expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
  #                      seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))

  #Convert raster into vector
  data <- as.vector(inner)

  #data <- data[!is.na(data)] #keep non-NA values only ... shall this affect mask values?
  #data[is.na(data)] <- 0
  data[data < 0] <- NA #any negative values are either recording problems or error values as per: ... Negative values would distort most calculations.

  return(data)
}

######################## getNtLtsUrlOls ###################################

#' Function to return the url of the OLS tile to download
#'
#' Function to return the url of the OLS tile to download given the year, month, and nlTile index
#'
#' @param inYear
#'
#' @return Character string Url of the VIIRS tile file
#'
#' @examples
#' tileUrl <- getNtLtsUrlOls("1999")
#'
getNtLtsUrlOls <- function(inYear, inTile)
{
  inYear <- as.character(inYear)

  #Function to return the url of the file to download given the year, month, and nlTile index
  #nlTile is a global list

  ntLtsBaseUrl <- "https://www.ngdc.noaa.gov/"

  #the page that lists all available nightlight files
  ntLtsPageHtml <- "https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html"

  #the local name of the file once downloaded
  ntLtsPageLocalName <- "ntltspageols.html"

  #if the file does not exist or is older than a week download it afresh
  #not working. download.file does not seem to update mtime
  if (!file.exists(ntLtsPageLocalName) || (lubridate::date(now()) - lubridate::date(file.mtime(ntLtsPageLocalName)) > lubridate::as.difftime(period("1 day"))))
  {
    download.file(url = ntLtsPageHtml, destfile = ntLtsPageLocalName, method = "wget", extra = "-N")
  }
  #else
  #  print(paste0(ntLtsPageHtml, " already downloaded"))

  #read in the html page
  ntLtsPage <- readr::read_lines(ntLtsPageLocalName)

  #search for a line containing the patterns that make the files unique i.e.
  #1. SVDNB_npp_20121001 - year+month+01
  #2. vcmcfg - for file with intensity as opposed to cloud-free counts (vcmslcfg)
  #sample url: https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201210/vcmcfg/SVDNB_npp_20121001-20121031_75N180W_vcmcfg_v10_c201602051401.tgz

  #create the pattern
  ntLtsPageRgxp <- paste0("F.*.", inYear,".*\\.tar")

  #search for the pattern in the page
  ntLtsPageHtml <- ntLtsPage[grep(pattern = ntLtsPageRgxp, x=ntLtsPage)]

  #split the output on quotes since this url is of the form ...<a href="URL"download> ...
  #the url is in the second position
  ntLtsPageUrl <- unlist(strsplit(ntLtsPageHtml, '"'))

  ntLtsPageUrl <- ntLtsPageUrl[grep("v4composite", ntLtsPageUrl)]

  ntLtsPageUrl <- unlist(lapply(ntLtsPageUrl, FUN=function(x) paste0(ntLtsBaseUrl, x)))

  #****NOTE: temp for testing using local download****
  #
  #fname <- stringr::str_extract(ntLtsPageUrl, "SVDNB.*.tgz")
  #ntLtsPageUrl <- paste0("http://localhost/", fname)
  #
  #****DELETE WHEN DONE****

  return (ntLtsPageUrl)
}

######################## getNtLtsZipLclNameOLS ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
getNtLtsZipLclNameOLS <- function(nlYear, tileNum)
{
  return (paste0(getNlDir("dirRasterOLS"), "/ols_", nlYear, "_", tileNum, ".tgz"))
}

######################## getNtLtsTifLclNameOLS ###################################

#' Constructs the filename used to save/access the decompressed OLS .tif file
#'
#' Constructs the filename used to save/access the decompressed OLS .tif file
#'
#' @param nlYear the year in which the tile was created
#'
#' @param nlMonth the month in which the tile was created
#'
#' @param tileNum the index of the tile as given in nlTileIndex
#'
#' @param dir the directory storing the tiles. Defaults to the global variable dirRasterVIIRS
#'
#' @return a character vector filename of the .tif VIIRS tile
#'
#' @examples
#' #using default dirRasterOLS
#' getNtLtsTifLclNameOLS("2014", "01", "1")
#'  returns "./tiles/viirs_2014_01_75N180W.tif"
#'
#'
getNtLtsTifLclNameOLS <- function(nlYear, tileNum)
{
  return (paste0(getNlDir("dirRasterOLS"), "/ols_", nlYear, "_", tileNum, ".tif"))
}

######################## getNtLtsOLS ###################################

#' Download OLS nightlight tile in TIF format from NOAA compressed as .tgz
#'
#' Download OLS nightlight tile in TIF format from NOAA compressed as .tgz
#'
#' @param nlYear the year in "YYYY" format e.g. "2012"
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' result <- getNtLtsVIIRS("2012", "05", "1")
#'
#' if (result)
#'   print("download successful")
#'
getNtLtsOLS <- function(nlYear, tileNum)
{
  rsltDnld <- NA

  ntLtsZipLocalName <- getNtLtsZipLclNameOLS(nlYear, tileNum)

  if (!file.exists(ntLtsZipLocalNameOLS))
  {
    ntLtsFileUrl <- getNtLtsUrlOLS(nlYear)

    rsltDnld <- download.file(ntLtsFileUrl, ntLtsZipLocalName, mode = "wb", method = "wget", extra = "-c")
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    print("File exists, set Overwrite = TRUE to overwrite")

    rsltDnld <- 0
  }

  if (rsltDnld == 0)
  {
    message("Extracting ", ntLtsZipLocalName, " ", base::date())

    if (!file.exists(getNtLtsTifLclNameOLS(nlYear, nlMonth, tileNum)))
    {
      message("Getting list of files in ", ntLtsZipLocalName, " ", base::date())

      tgzFileList <- utils::untar(ntLtsZipLocalName, list = TRUE)
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")

      if (is.null(tgzFileList))
      {
        message("Error extracting file list. ")

        return (-1)
      }

      tgzAvgRadFilename <- tgzFileList[grep("svdnb.*.avg_rade9.tif$",tgzFileList, ignore.case = T)]

      message("Decompressing ", tgzAvgRadFilename, " ", base::date())

      if(!file.exists(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)))
      {
        utils::untar(ntLtsZipLocalName, files = tgzAvgRadFilename, exdir = getNlDir("dirRasterOLS"))

        file.rename(file.path(getNlDir("dirRasterOLS"), tgzAvgRadFilename), getNtLtsTifLclNameOLS(nlYear, tileNum))
      }
    }
    else
    {
      message("TGZ file found")
    }
  }

  return (rsltDnld == 0)
}

######################## masqOLS ###################################

#' Mask
#'
#' Download VIIRS nightlight tile
#'
#' @param shp the country Polygon layer as SpatialPolygon
#'
#' @param rast the clipped country raster
#'
#' @param i the index of the polygon in the country polygon layer (shp)
#'
#' @return numeric vector of radiances
#'
#' @examples
#' ctryPoly <- readOGR(getPolyFnamePath("KEN"), getCtryShpLyrName("KEN", 1))
#' ctryRaster <- raster(getCtryRasterOutputFname("KEN", "1999"))
#' temp <- NULL
#' KenAdm1Sum <- NULL
#' for (i in 1:5)#length(ctryPoly@polygons))
#' {
#'  temp$name <- as.character(ctryPoly@data$NAME_1[i])
#'  temp$sum <- sum(masqOLS(ctryPoly, ctryRaster, i), na.rm=T)
#'
#'  KenAdm1Sum <- rbind(KenAdm1Sum)
#' }
#'
masqOLS <- function(shp, rast, i)
{
  #based on masq function from https://commercedataservice.github.io/tutorial_viirs_part1/
  #Extract one polygon based on index value i
  polygon <- shp[i,] #extract one polygon
  extent <- raster::extent(polygon) #extract the polygon extent

  #Raster extract
  outer <- raster::crop(rast, extent) #extract raster by polygon extent
  #inner <- mask(outer,polygon) #keeps values from raster extract that are within polygon
  inner <- raster::rasterize(polygon, outer, mask=TRUE) #crops to polygon edge & converts to raster

  #Convert cropped raster into a vector
  #Specify coordinates
  coords <- expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
                        seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
  #Convert raster into vector
  data <- as.vector(inner)

  ##THIS SECTION NEEDS WORK. confirm error and NA values and handling of the same
  #keep non-NA values only?
  #data <- data[!is.na(data)]

  #in DMSP-OLS 255 == NA
  data[which(data == 255)] <- NA

  #non-negative values are errors replace with 0?
  data[data < 0] <- 0

  return(data)
}

######################## ctryNameToCode ###################################

#' Convert a country name to its ISO3 code
#'
#' Convert a country name to its ISO3 code. Exposes the rworldmap function rwmGetISO3(ctryName). See the examples
#'
#' @param ctryCode
#'
#' @return Character full country name
#'
#' @examples
#' ctryNameToCode("kenya")
#'   #returns "KEN"
#'
#' ctryNameToCode("ken")
#'   #returns "KEN"
#'
#' @export
ctryNameToCode <- function(ctryName)
{
  if(missing(ctryName))
    stop("Missing parameter ctryName")
  
  if (class(ctryName) != "character" || is.null(ctryName) || is.na(ctryName) || ctryName =="" || length(grep("[^[:alpha:]]", ctryName) > 0))
    stop("Invalid ctryName: ", ctryName)
  
  return (rworldmap::rwmGetISO3(ctryName))
}

######################## ctryCodeToName ###################################

#' Convert a country ISO3 code to the full name
#'
#' Convert a country ISO3 code to the full name. Exposes the rworldmap function 
#'     isoToName(ctryCode). #rworldmap::isoToName can resolve 2-letter ctryCodes 
#'     but we only want 3-letter ISO3 codes
#'
#' @param ctryCode
#'
#' @return Character full country name
#'
#' @examples
#' ctryCodeToName("KEN")
#'
#' @export
ctryCodeToName <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing equired parameter ctryCode")

  if (class(ctryCode) != "character" || is.null(ctryCode) || is.na(ctryCode) || ctryCode =="" || length(grep("[^[:alpha:]]", ctryCode) > 0))
    stop("Invalid ctryCode: ", ctryCode)
    
  #rworldmap::isoToName can resolve 2-letter ctryCodes but we only want 3-letter ISO3 codes
  if(nchar(ctryCode) != 3)
    stop("Only 3-letter ISO3 codes allowed")
  
  return(rworldmap::isoToName(ctryCode))
}

######################## ctryPolyLyrNames : TO DELETE ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
ctryPolyLyrNames <- function (nLyrs)
{
  if(missing(nLyrs))
    stop("Missing required parameter nLyrs")
  
  if(is.null(nLyrs) || is.na(nLyrs) || nLyrs < 1 || !is.numeric(nLyrs) || is.na(as.numeric(nLyrs)))
    stop("Invalid value for nLyrs: ", nLyrs)
  
  #the repeat pattern required to create columns in the format 1,1,2,2,3,3 ...
  #for col names: admlevel1_id, admlevel1_name, ..., admleveN_id, admlevelN_name
  #and polygon data col names: ID_1, NAME_1, ..., ID_N, NAME_N
  nums <- c(paste(1:nLyrs,1:nLyrs))

  nums <- utils::unlist(strsplit(paste(nums, collapse = " "), " "))

  return(paste(c("ID_", "NAME_"), nums, sep=""))
}

######################## ctryPolyAdmColNames : TO DELETE ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
ctryPolyAdmColNames <- function (ctryPolyAdmLevels, nLyrs)
{
  
  #the repeat pattern required to create columns in the format 1,1,2,2,3,3 ...
  #for col names: admlevel1_id, admlevel1_name, ..., admleveN_id, admlevelN_name
  #and polygon data col names: ID_1, NAME_1, ..., ID_N, NAME_N
  nums <- c(paste(1:nLyrs,1:nLyrs))

  nums <- unlist(strsplit(paste(nums, collapse = " "), " "))

  return(paste(ctryPolyAdmLevels[nums, "name"], c("_id", "_name"), sep=""))
}

######################## processNLCountryOls ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
processNLCountryOls <- function(cntryCode, nlYear)
{
  message("processNLCountryOLS: ")

  ctryPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLowestLyrName(ctryCode))

  ctryExtent <- raster::extent(ctryPoly)

  raster::projection(ctryPoly) <- sp::CRS(wgs84)

  nlYear <- substr(nlYearMonth, 1, 4)

  nlMonth <- substr(nlYearMonth, 5, 6)

  if (existsCtryNlDataFile(ctryCode))
  {
    ctryNlDataDF <- utils::read.csv(getCtryNlDataFnamePath(ctryCode),header = TRUE,sep = ",")

    existingDataCols <- names(ctryNlDataDF)

    existingYears <- existingDataCols[grep("^NL_[:alphanum:]*", existingDataCols)]

    existingYears <- stringr::str_replace(existingYears, "NL_OLS_", "")

    if(nlYear %in% existingYears)
    {
      message("Data exists for ", ctryCode, " ", nlYear)

      return(-1)
    }
  } else
  {
    #get the list of admin levels in the polygon shapefile
    ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode)

    #add the area as reported by the polygon shapefile as a convenience
    areas <- raster::area(ctryPoly)

    if (length(ctryPolyAdmLevels) > 0)
    {
      #conver to lower case for consistency
      ctryPolyAdmLevels <- tolower(ctryPolyAdmLevels)

      #the number of admin levels
      nLyrs <- length(ctryPolyAdmLevels)

      #the names of the layers which will become column names
      ctryPolyAdmCols <- ctryPolyLyrNames(nLyrs)

      #pull the ID_ and NAME_ cols from layer1 to lowest layer (layer0 has country code not req'd)
      ctryNlDataDF <- ctryPoly@data[, eval(ctryPolyAdmCols)]


      #we add the country code to ensure even a renamed file is identifiable
      #repeat ctryCode for each row in the polygon. equiv of picking layer0
      ctryCodeCol <- rep(ctryCode, nrow(ctryNlDataDF))

      #combine the columns
      ctryNlDataDF <- cbind(ctryCodeCol, ctryNlDataDF, areas)

      ctryPolyColNames <- ctryPolyAdmColNames(ctryPolyAdmLevels, nLyrs)

      #add the country and area columns to the dataframe
      ctryPolyColNames <- c("country", ctryPolyColNames, "area_sq_km")

      names(ctryNlDataDF) <- ctryPolyColNames

    } else
    {
      ctryNlDataDF <- data.frame("country"=ctryCode, "area_sq_km"=areas)
    }
  }

  if(!file.exists(getCtryRasterOutputFname(ctryCode, nlYearMonth)))
  {
    message("Begin processing ", nlYearMonth, " ", base::date())

    message("Reading in the rasters " , base::date())

    tileList <- getCtryCodeTileList(ctryCode)

    ctryRastCropped <- NULL

    for (tile in tileList)
    {
      satYear <- substr(tar_fl,1,7)

      message("Extracting ", tar_fl, " ", base::date())

      message("Getting list of files in ", tar_fl, " ", base::date())
      #get a list of files in the tar archive
      tar_file_list <- utils::untar(tar_fl, list = TRUE)

      #get the nightlight data filename
      #the nightlight data filename has the format "web.avg_vis.tif.gz"
      #    tgz_file <- tar_file_list[grep(".*web\\.avg_vis\\.tif\\.gz$",tar_file_list, ignore.case = T)]
      tgz_file <- tar_file_list[grep(".*stable_lights\\.avg_vis\\.tif\\.gz$",tar_file_list, ignore.case = T)]

      #extract the nightlight data file
      utils::untar(tar_fl, tgz_file)

      #the tif has the same name as the compressed file without the .gz
      tif_file <- stringr::str_replace(tgz_file, ".gz", "")

      message("Decompressing ", tgz_file, " ", base::date())

      gunzip(tgz_file)
      #no need to delete the gz since gunzip deletes the compressed version
    }

    #for()
    {
      message("Reading in the raster " , base::date())
      rastGlobal <- raster::raster(tif_file)

      raster::projection(rastGlobal) <- sp::CRS(wgs84)

      message("Cropping the raster ", base::date())
      ctryRast <- raster::crop(rastGlobal, ctryPoly)

      message("Releasing the raster variables")
      rm(rastGlobal)

      message("Deleting the raster files ", base::date())

      unlink(tif_file)

      #merge rasters
    }

    #write merged raster
  }
  else
  {
    #read in the raster
  }

  gc()

  message("Masking the merged raster ", base::date())
  #rast_ke <- mask(rast_ke, ke_shp_ward)
  rast_ke <- raster::rasterize(ctryPoly, rast_ke, mask=TRUE) #crops to polygon edge & converts to raster

  message("Writing the merged raster to disk ", base::date())
  raster::writeRaster(x = rast_ke, filename = paste0(getNlDir("dirRasterOutput"), nlYear, ".tif"), overwrite=TRUE)

  doParallel::registerDoParallel(cores=2)

  message("Begin extracting the data from the merged raster ", base::date())
  sumAvgRad <- foreach::foreach(i=1:nrow(ctryPoly@data), .combine=rbind) %dopar%
  {
    #message("Extracting data from polygon " , i, " ", base::date())
    dat <- masq(ctryPoly, rast_ke,i)

    #message("Calculating the mean of polygon ", i, " ", base::date())

    #calculate and return the sum of the mean of all the pixels
    data.frame(mean = sum(dat, na.rm=TRUE))
  }

  #merge the calculated means for the polygon as a new column
  ctryNlDataDF <- cbind(extctryNlDract, sumAvgRad)

  #name the new column with the yearmonth of the data
  names(extract)[ncol(extract)] <- paste0("NL_OLS_", nlYear)

  message("DONE processing ", nlYear, " ", base::date())

  message("COMPLETE. Writing data to disk")
  utils::write.table(extract, getCtryNlDataFname(ctryCode), row.names= F, sep = ",")

}

######################## createCtryNlDataDF ###################################

#' Initiates the country nightlight dataframe with the country data read from the polygon
#'
#' Initiates the country nightlight dataframe with the country data read from the polygon. 
#'     This includes admin levels, level names and area
#'
#' @param ctryCode the ISO3 code of the country
#'
#' @return dataframe with the country admin level data
#'
#' @examples
#' initCtryNlData <- createCtryNlDataDF("KEN")
#'  #returns a data frame
#'
createCtryNlDataDF <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(dir.exists(getPolyFnamePath(ctryCode)) && length(dir(getPolyFnamePath(ctryCode)))> 0)
  ctryPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLowestLyrName(ctryCode))

  ctryExtent <- raster::extent(ctryPoly)

  raster::projection(ctryPoly) <- sp::CRS(wgs84)

  #get the list of admin levels in the polygon shapefile
  ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode)

  #conver to lower case for consistency
  ctryPolyAdmLevels <- tolower(ctryPolyAdmLevels)

  #add the area as reported by the polygon shapefile as a convenience
  areas <- raster::area(ctryPoly)

  if (length(ctryPolyAdmLevels) > 0)
  {
    #When a country does not have lower administrative levels

    #the number of admin levels
    nLyrs <- length(ctryPolyAdmLevels)

    #the repeat pattern required to create columns in the format 1,1,2,2,3,3 ...
    #for col names: admlevel1_id, admlevel1_name, ..., admleveN_id, admlevelN_name
    #and polygon data col names: ID_1, NAME_1, ..., ID_N, NAME_N
    #nums <- c(paste(1:nLyrs,1:nLyrs))

    #nums <- unlist(strsplit(paste(nums, collapse = " "), " "))

    #ctryPolyAdmCols <- paste(c("ID_", "NAME_"), nums, sep="")

    ctryPolyAdmCols <- paste(c("NAME_"), 1:nLyrs, sep="")

    #pull the ID_ and NAME_ cols from layer1 to lowest layer (layer0 has country code not req'd)
    ctryNlDataDF <- as.data.frame(ctryPoly@data[,eval(ctryPolyAdmCols)])

    #add the area as reported by the polygon shapefile as a convenience
    areas <- raster::area(ctryPoly)

    #we add the country code to ensure even a renamed file is identifiable
    #repeat ctryCode for each row in the polygon. equiv of picking layer0
    ctryCodeCol <- rep(ctryCode, nrow(ctryNlDataDF))

    #combine the columns
    ctryNlDataDF <- cbind(ctryCodeCol, ctryNlDataDF, areas)

    #ctryPolyColNames <- paste(ctryPolyAdmLevels[nums, "name"], c("_id", "_name"), sep="")
    ctryPolyColNames <- ctryPolyAdmLevels

    #add the country code and area columns to the dataframe
    ctryPolyColNames <- c("country", ctryPolyColNames, "area_sq_km")

    names(ctryNlDataDF) <- ctryPolyColNames
  } else
  {
    ctryNlDataDF <- data.frame("country"=ctryCode, "area_sq_km"=areas)
  }

  return(ctryNlDataDF)
}

######################## processNLCountriesVIIRS : TO DELETE ###################################

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
#' processNLCountriesVIIRS(ctryCodes, nlYearMonth)
#'
processNLCountriesVIIRS <- function(ctryCodes, nlYearMonth)
{
  if(missing(ctryCodes))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")
  
  if(!allValid(ctryCodes, validCtryCode))
    stop("Invalid ctryCode detected")
  
  if(!validNlYearMonthVIIRS(nlYearMonth))
    stop("Invalid nlYearMonth: ", nlYearMonth)
  for (nlCtryCode in nlCtryCodes)
  {
    processNLCountryVIIRS(ctryCode, nlYearMonth)
  }
}

######################## ctryShpLyrName2Num ###################################

#' Get the integer number of the layer. topmost country layer=0
#'
#' Get the integer number of the layer. Is the last character in the name and is a digit. E.g. for the 3rd layer in Kenya shapefile polygon named "KEN_adm3" the layer number is "3"
#'
#' @param layerName - the name of the polygon layer
#'
#' @return Integer layer number
#'
#' @examples
#' ctryShpLyrName2Num("KEN_adm1")
#'   #returns 1
#'
ctryShpLyrName2Num <- function(layerName)
{
  if(missing(layerName))
    stop("Missing required parameter layerName")
  
  if (class(layerName) != "character" || is.null(layerName) || is.na(layerName) || layerName =="")
    stop("Invalid layerName: ", layerName)
  
  return(as.numeric(gsub("[^[:digit:]]", "", layerName)))
}

######################## processNLCountryVIIRS ###################################

#' Processes nightlights for an individual country in a particular year month 
#'
#' Given a \code{countryCode}, \code{yearMonth} and preferred processing methods 
#'     \code{cropMaskMethod} and \code{extractMethod}, this function will first check if the 
#'     data already exists in the cache. First it will check if the data file exists and if it 
#'     does not it will create a dataframe of the country data containing only the administrative
#'     properties and move to processing. If the data file exists it will check to see if the 
#'     particular year month already exists. If it exists, it will exit with a message. If it does
#'     not exist, it will load the country data file and move on to processing.
#' 
#'     Processing consists of:
#'     \enumerate{ 
#'        \item Reading in the country polygon in ESRI Shapefile format
#'        \item Reading in the tiles that the particular country intersects with and then clipping 
#'        the tile(s) to the country boundary
#'        \item Extract the data from the clipped raster and compute various statistics at the lowest admin level in the country.
#'        \item Finally, these stats are appended to the data frame and written to the data file.
#'     }
#' 
#'     NOTE: \code{processNLCountryVIIRS()} assumes that all inputs are available and will not 
#'     attempt to download them. It should ideally be called from the function \code{processNtLts()}
#'     which does all the preparation for processing. \code{processNtLts()} which can process 
#'     multiple countries and time periods will download all the required tiles and polygons prior to
#'     calling \code{processNLCountryVIIRS()}. \code{getCtryNlData} can also be used with the option
#'     \code{ignoreMissing=FALSE} which will call \code{processNtLts} in the background.
#'
#' @param ctryCode
#'
#' @param nlYearMonth
#'
#' @param cropMaskMethod ("rast" or "gdal") Whether to use rasterize or gdal-based functions to 
#'     crop and mask the country rasters
#'     
#' @param extractMethod ("rast" or "gdal") Whether to use rasterize or gdal-based functions 
#'     to crop and mask the country rasters
#' 
#' @param stats the statistics to calculate. If not provided will calculate the stats specified 
#'     in \code{pkg_options("stats")}
#'
#' @return None
#'
#' @examples
#' processNLCountryVIIRS(c("KEN"), c("201204"))
#'
processNLCountryVIIRS <- function(ctryCode, nlYearMonth, cropMaskMethod="rast", extractMethod="rast", fnStats=stats)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validNlYearMonthVIIRS(nlYearMonth))
    stop("Invalid nlYearMonth: ", nlYearMonth)
  
  if (missing(cropMaskMethod) || class(cropMaskMethod) != "character" || is.null(cropMaskMethod) || nlYearMonth == "")
    cropMaskMethod <- "rast"
  
  message("processNLCountryVIIRS: ", ctryCode, " ", nlYearMonth)

  nlYear <- substr(nlYearMonth, 1, 4)

  nlMonth <- substr(nlYearMonth, 5, 6)

  message("Check for existing data file")

  if (existsCtryNlDataFile(ctryCode))
  {
    message("Data file found: ", getCtryNlDataFnamePath(ctryCode))

    if(all(sapply(fnStats, function(stat) existsCtryNlDataVIIRS(ctryCode, nlYearMonth, stat))))
    {
      message("All stats data exists for ", ctryCode, " ", nlYearMonth, ". Skipping")

      return(-1)
    }

    message("Load country data file")
    ctryNlDataDF <- utils::read.csv(getCtryNlDataFnamePath(ctryCode))

    message("Load country polygon lowest admin level")
    ctryPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLowestLyrName(ctryCode))

    ctryExtent <- raster::extent(ctryPoly)

    raster::projection(ctryPoly) <- sp::CRS(wgs84)

  } else
  {
    message("Data file not found. Creating ...")

    ctryNlDataDF <- createCtryNlDataDF(ctryCode)

    message("Data file not found. Creating ... DONE")

    message("Load country polygon lowest admin level")

    ctryPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLowestLyrName(ctryCode))
  }

  if(!file.exists(getCtryRasterOutputFname(ctryCode, nlYearMonth)))
  {
    message("Begin processing ", nlYearMonth, " ", base::date())

    message("Reading in the rasters " , base::date())

    tileList <- getCtryCodeTileList(ctryCode)

    ctryRastCropped <- NULL

    for (tile in tileList)
    {
      rastFilename <- getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileName2Idx(tile))

      rastTile <- raster::raster(rastFilename)

      raster::projection(rastTile) <- sp::CRS(wgs84)

      message("Cropping the rasters", base::date())

      #extTempCrop <- crop(rastTile, ctryExtent)

      tempCrop <- raster::crop(rastTile, ctryPoly)

      if(is.null(ctryRastCropped))
      {
        ctryRastCropped <- tempCrop

        #ctryExtCropped <- extTempCrop
      }
      else
      {
        ctryRastMerged <- ctryRastCropped

        ctryRastCropped <- NULL

        ctryRastCropped <- raster::merge(ctryRastMerged, tempCrop)

        rm(ctryRastMerged)

        #ctryExtMerged <- ctryExtCropped

        #ctryExtCropped <- NULL

        #ctryExtCropped <- merge(ctryExtMerged, tempCrop)

      }

      rm(tempCrop)
    }

    rm(rastTile)

    gc()

    message("Masking the merged raster ", base::date())

    if (cropMaskMethod == "rast")
    {

      #RASTERIZE
      message("Crop and mask using rasterize ", base::date())
      ctryRastCropped <- raster::rasterize(ctryPoly, ctryRastCropped, mask=TRUE) #crops to polygon edge & converts to raster

      message("Writing the merged raster to disk ", base::date())

      raster::writeRaster(x = ctryRastCropped, filename = getCtryRasterOutputFname(ctryCode,nlYearMonth), overwrite=TRUE)

      message("Crop and mask using rasterize ... Done", base::date())
    }
    else if (cropMaskMethod == "gdal")
    {
      message("Crop and mask using gdalwarp ... ", base::date())

      #GDALWARP
      rstTmp <- paste0(tempfile(), ".tif")

      message("Writing merged raster to disk for gdal")

      raster::writeRaster(ctryRastCropped, rstTmp)

      output_file_vrt <- paste0(ctryCode, "_", nlYearMonth, ".vrt")

      if (file.exists(output_file_vrt))
        file.remove(output_file_vrt)

      message("gdalwarp ",base::date())

      gdalUtils::gdalwarp(srcfile=rstTmp, dstfile=output_file_vrt, s_srs=wgs84, t_srs=wgs84, cutline=getPolyFnamePath(ctryCode), cl= getCtryShpLyrName(ctryCode,0), multi=TRUE, wm=2048, wo="NUM_THREADS=ALL_CPUS")

      message("gdal_translate ", base::date())
      gdalUtils::gdal_translate(co = "compress=LZW", src_dataset = output_file_vrt, dst_dataset = getCtryRasterOutputFname(ctryCode,nlYearMonth))

      message("Deleting the component rasters ", base::date())

      file.remove(rstTmp)
      file.remove(output_file_vrt)

      ctryRastCropped <- raster::raster(getCtryRasterOutputFname(ctryCode, nlYearMonth))
      #GDALWARP
      message("Crop and mask using gdalwarp ... DONE", base::date())
    }
  }
  else
  {
    rastFilename <- getCtryRasterOutputFname(ctryCode, nlYearMonth)

    ctryRastCropped <- raster::raster(rastFilename)

    raster::projection(ctryRastCropped) <- sp::CRS(wgs84)
  }

  message("Create web version of raster", base::date())

  #attempting to obtain QGIS display style grayscale stretch minmax of 2%-98% of values
  #message("calculating quantile 2 and 98 ", base::date())
  #system.time(qnts <- sapply(1:1000,FUN =  function(x) quantile(sampleRandom(ctryRastCropped,100), c(0.02,0.98)),simplify = T))

  #qnt2 <- mean(qnts[1,])
  #qnt98 <- mean(qnts[2,])

  #cmd <- paste0("gdal_translate -co TILED=YES -co COMPRESS=JPEG -ot Byte -scale ", qnt2, " ", qnt98," 0 255 ", getCtryRasterOutputFname(ctryCode,nlYearMonth), " ", dirRasterWeb, "/", ctryCode, "_", nlYearMonth, "_JPEG.tif")

  #message("Create web raster ", base::date())
  #system(cmd)

  message("Begin extracting the data from the merged raster ", base::date())

  if (extractMethod == "rast")
    sumAvgRad <- fnSumAvgRadRast(ctryPoly, ctryRastCropped, fnStats)
  else if (extractMethod == "gdal")
    sumAvgRad <- fnSumAvgRadGdal(ctryCode, ctryPoly, nlYearMonth, fnStats)

  for(stat in fnStats)
    ctryNlDataDF <- insertNlDataCol(ctryNlDataDF, sumAvgRad[,stat], stat, nlYearMonth, nlType = "VIIRS")

  message("DONE processing ", ctryCode, " ", nlYearMonth, " ", base::date())

  message("COMPLETE. Writing data to disk")

  #Write the country data dataframe to disk
  saveCtryNlData(ctryNlDataDF, ctryCode)

  #release the cropped raster
  rm (ctryRastCropped)

  #delete temporary raster files
  raster::removeTmpFiles(h = 0)
}

######################## insertNlDataCol ###################################

#' Insert an aggregate nightlight data column in a country nightlights dataframe
#'
#' Insert an aggregate nightlight data column in a country nightlights dataframe. The number
#'     of elements in the vector MUST match the number of rows in the country dataframe.
#'
#' @param ctryNlDataDF dataframe with the country data to save
#' 
#' @param dataCol the numeric vector to be inserted as a column
#' 
#' @param statType the stat which produced the dataCol vector
#' 
#' @param nlYearMonth the yearmonth that the dataCol belongs to
#' 
#' @param nlType the type of nightlight data i.e. "OLS" or "VIIRS"
#'
#' @return Character full path to the cropped VIIRS country raster for a country and a given year and month
#'
#' @examples
#' 
#' ctryNlDataDF <- insertNlDataCol(ctryNlDataDF, dataCol, "sum", "201209", "VIIRS")
#'
insertNlDataCol <- function (ctryNlDataDF, dataCol, statType, nlYearMonth, nlType = "VIIRS")
{
  if(missing(ctryNlDataDF))
    stop("Missing required parameter ctryNlDataDF")
  
  if(missing(dataCol))
    stop("Missing required parameter dataCol")
  
  if(missing(statType))
    stop("Missing required parameter statType")
  
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")
  
  if(missing(nlType))
    warning("Missing parameter nlType, defaulting to: ", nlType)
  
  
  #append the calculated means for the polygon as a new column
  ctryNlDataDF <- cbind(ctryNlDataDF, dataCol)
  
  #name the new column which is currently last with the yearmonth of the data
  names(ctryNlDataDF)[ncol(ctryNlDataDF)] <- getCtryNlDataColName(nlYearMonth = nlYearMonth, stat = statType, nlType = nlType)
  
  #re-arrange the columns
  #read in all column names in the dataframe afresh
  cols <- names(ctryNlDataDF)
  
  #get only the nightlight data columns
  nlDataColIdx <- grep("^NL_", cols)
  
  nlDataColNames <- cols[nlDataColIdx]
  
  #sort the column names ascending
  nlDataColNames <- nlDataColNames[order(nlDataColNames)]
  
  #combine the non-nightlight and the nightlight data columns
  newNlDataColNames <- c(cols[-nlDataColIdx], nlDataColNames)
  
  #write back the dataframe with the new column order
  ctryNlDataDF <- ctryNlDataDF[ , newNlDataColNames]
  
  return(ctryNlDataDF)
}

######################## saveCtryNlData ###################################

#' Save a data frame of a country's data to the appropriate location
#'
#' Saves the data frame created from processNlCountry* to the appropriate location. 
#'     Note: This function does not perform any validation error checking and will overwrite 
#'     existing data. Use with caution.
#'
#' @param ctryNlDataDF dataframe with the country data to save
#'
#' @return None
#'
#' @examples
#' 
#' saveCtryNlData(ctryNlDataDF, ctryCode)
#'
saveCtryNlData <- function(ctryNlDataDF, ctryCode)
{
  if(missing(ctryNlDataDF))
    stop("Missing required parameter ctryNlDataDF")
  
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validCtryNlDataDF(ctryNlDataDF))
    stop("Invalid country dataframe")
  
  utils::write.table(ctryNlDataDF, getCtryNlDataFnamePath(ctryCode), row.names= F, sep = ",")
}

######################## validCtryNlDataDF ###################################

#' Check if a country dataframe is valid
#'
#' Check if a country dataframe is valid
#'
#' @param ctryNlDataDF the country dataframe
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validCtryNlDataDF(nlCtryDataDF)
#'  returns TRUE
#'
#' validNlMonthName("","OLS")
#'  returns FALSE
#'
validCtryNlDataDF <- function(ctryNlDataDF)
{
  if(missing(ctryNlDataDF))
    stop("Missing required parameter ctryNlDataDF")
  
  if(class(ctryNlDataDF) == "data.frame" && !is.null(ctryNlDataDF) && names(ctryNlDataDF)[1] == "country")
    return(TRUE)
  else
    return(FALSE)
}

######################## getCtryRasterOutputFname ###################################

#' Get the full path to the file where the cropped VIIRS country raster is stored.
#'
#' Get the full path to the file where the cropped VIIRS country raster is stored. This file is created
#'     when processing the country before extracting the data. It can be used to re-process a country much faster
#'
#' @param ctryCode
#'
#' @param nlYearMonth the year and month
#'
#' @return Character full path to the cropped VIIRS country raster for a country and a given year and month
#'
#' @examples
#' getCtryRasterOutputFname("KEN","201412")
#'
getCtryRasterOutputFname <- function(ctryCode, nlYearMonth)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validNlYearMonthVIIRS(nlYearMonth))
    stop("Invalid nlYearMonth: ", nlYearMonth)
  
  return (file.path(getNlDir("dirRasterOutput"), paste0(ctryCode, "_", nlYearMonth,".tif")))
}

######################## getCtryPolyUrl ###################################

#' Get the GADM url from which to download country polygons
#'
#' Get the url from which to download country polygons. Polygons are downloaded from 
#'     \url{http://www.gadm.org}. This provides the url to the zipped ESRI Shapefile 
#'     which when decompressed contains a directory with the different country admin 
#'     level boundary files. A sample url returned for Afghanistan: 
#'     http://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
#'
#' @param ctryCode
#'
#' @return Character string url of the zipped ESRI shapefile for the ctryCode
#'
#' @examples
#' getCtryPolyUrl(ctryCode)
#'  #returns url for the zipped country ESRI shapefile
#'
getCtryPolyUrl <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)

  #Sample url: http://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
  basePolyUrl <- "http://biogeo.ucdavis.edu/data/gadm2.8/shp/"

  return (paste0(basePolyUrl, ctryCode, "_adm_shp.zip"))
}

######################## getCtryNlDataFname ###################################

#' Check if a month number is valid for a given nightlight type
#'
#' Get the name of the data file. This function can be altered to name the file as 
#'     required and consistently retrieve the name. Used in the function getCtryNlDataFnamePath 
#'     to concat the directory path and this filename. Currently all nlTypes are stored in one 
#'     file. Can be altered to separate VIIRS and OLS data files for example.
#'
#' @param ctryCode
#'
#' @return Character filename of the country data file
#'
#' @examples
#' ctryCode <- "KEN"
#' getCtryNlDataFname(ctryCode)
#'  #returns name of the ctry data file
#'
getCtryNlDataFname <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  return (paste0(ctryCode, "_NLData.csv"))
}

######################## getCtryNlDataFnamePath ###################################

#' Get the full path to the file containing the country data
#'
#' Get the full path to the file containing the country data
#' 
#' @param ctryCode
#'
#' @return Character string the full path to the data file of a country
#'
#' @examples
#' ctryDF <- read.csv(getCtryNlDataFnamePath("KEN"))
#'  #returns DF with nightlight data for the country
#'
getCtryNlDataFnamePath <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  return (file.path(getNlDir("dirNlData"), getCtryNlDataFname(ctryCode)))
}

######################## getCtryNlData ###################################

#' Returns VIIRS nightlight data for the given ctryCode in the given year months
#'
#' Returns VIIRS nightlight data for the given ctryCode and stats in the given 
#'     year months. Note that getCtryNldata only processes one ctryCode at a time.
#'     \code{ignoreMissing} plays a significant role here. It can take 3 values:
#'     
#'     \itemize{
#'         \item NULL (default) only return data if found for all nlYearMonths
#'            and all stats provided otherwise return NULL
#'         \item TRUE return any partial data that is found for the provided 
#'            nlYearMonths and stats. Ignore any missing data
#'         \item FALSE return all data that is found and call \code{processNtLts}
#'            to download and process any missing nlYearMonths and stats
#'     }
#'    
#'    Farther, if \code{nlYearMonths} is missing, it is assigned values based on
#'    the value of ignoreMissing. If ignoreMissing is FALSE, nlYearMonths is 
#'    assigned all existing nlYearMonths to date. This is the equivalent of 
#'    retrieving all nightlight data for the given country and stats. If 
#'    ignoreMissing is TRUE or NULL then the existing data is returned.
#'
#' @param ctryCode the ISO3 code of the country. Only 1 country can be processed at a time
#'
#' @param nlYearMonths a vector of yearMonths
#' 
#' @param stats a vector of stats. if not supplied defaults to all stats as listed in pkg_options("stats")
#' 
#' @param nlType the nightlight type i.e. "OLS" or "VIIRS" (default)
#' 
#' @param ignoreMissing controls how the function behaves if any data is not
#'     found in the data file
#'     
#'     \itemize{
#'         \item NULL (default) only return data if found for all nlYearMonths
#'            and all stats provided otherwise return NULL
#'         \item TRUE return any partial data that is found for the provided 
#'            nlYearMonths and stats. Ignore any missing data
#'         \item FALSE return all data that is found and call \code{processNtLts}
#'            to download and process any missing nlYearMonths and stats
#'     }
#'
#' @return dataframe of data for one country in one or multiple yearmonths
#'
#' @examples
#' #missing stats implies all stats
#' 
#' getCtryNlData("KEN", ignoreMissing=NULL)
#'  #returns all existing data i.e. all nlYearMonths and all stats for KEN
#'
#' getCtryNlData("KEN", ignoreMissing=TRUE)
#'  #same as ignoreMissing=NULL. Returns all existing data i.e. all nlYearMonths
#'  #and all stats for KEN
#'  
#' getCtryNlData("KEN", ignoreMissing=FALSE)
#'  #for any missing data between 201204 to present download and process the
#'  #data then return all data
#'  
#' getCtryNlData("KEN", nlYearMonths=c("existingNlYearMonth", "missingNlYearMonth"),
#'     stats=c("existingStat", "missingStat"), ignoreMissing=NULL)
#'  #Returns NULL
#'  #(ignoreMissing==NULL returns all data if exists or if any is missing returns NULL)
#'
#' getCtryNlData("KEN", nlYearMonths=c("existingNlYearMonth", "missingNlYearMonth),
#'     stats=c("existingStat", "missingStat"), ignoreMissing=TRUE)
#'  #Returns existingStat for existingNlYearMonth
#'  #(ignoreMissing==TRUE returns only existing data)
#'  
#' getCtryNlData("KEN", nlYearMonths=c("existingNlYearMonth", "missingNlYearMonth),
#' stats=c("existingStat", "missingStat"), ignoreMissing=FALSE)
#'  #Runs processNtLts for missingStat in missingNlYearMonth and returns
#'  #existingStat and missingStat for both existingNlYearMonth and missingNlYearMonth
#'  #(ignoreMissing==FALSE must return all data: forces processing of any missing)
#'  
#' @export
getCtryNlData <- function(ctryCode, nlYearMonths, stats=pkg_options("stats"), nlType, ignoreMissing=NULL, source="local")
{
  if(missing(ctryCode))
    stop("Missing required ctryCode")

  if(missing(nlYearMonths) && (!missing(ignoreMissing) && !ignoreMissing))
    nlYearMonths <- getAllNlYears()
  
  #if(missing(ignoreMissing))
  #  ignoreMissing = TRUE
  
  if(length(ctryCode) > 1)
    stop("getCtryNlData can only process 1 ctryCode at a time")
  
  if(!is.null(ignoreMissing))
    if(ignoreMissing && !existsCtryNlDataFile(ctryCode))
      stop("No data exists for ", ctryCode, ". Set IgnoreMissing= to download and process")
  
  if (!missing(nlYearMonths)) #if nlYearMonths is provided process else return all ctry data
  {
    #check if the stats exist in the given year months will test nlYm1+stat1, nlYm2+stat1, ..., nlYm1+stat2, nlYm2+stat2
    nlYmStats <- expand.grid(nlYearMonths, stats)
    
    existnlYMStats <- apply(nlYmStats, 1, function(x) existsCtryNlDataVIIRS(ctryCode, x[1], x[2]))
    
    missingData <- paste0(apply(nlYmStats[!existnlYMStats,], 1, function(x)paste0(x[1], ":", x[2])), collapse = ", ")
    
    if (!all(existnlYMStats))
    {
      if (is.null(ignoreMissing)) #default
      {
        message("No data found for ", ctryCode, " in ", missingData, ". Returning NULL. Note: Set ignoreMissing=TRUE to return only data found or ignoreMissing=FALSE to download and extract missing data")
        return (NULL)
      }
      else if(!ignoreMissing)
      {
        message("Processing missing data: ", ctryCode, " in ", missingData, ". This may take a while. Note: Set 'ignoreMissing=TRUE' to return only data found or 'ignoreMissing=NULL' to return NULL if not all the data is found")
        
        processNtLts(ctryCode, nlYearMonths, stats = stats)
      }
      else if (ignoreMissing)
      {
        message("Ignoring missing data for ", ctryCode, " in ", missingData, ". Returning existing data only.")
      }
      else
      {
        message("Invalid value for 'ignoreMissing'. Exiting. Note: Set ignoreMissing=TRUE to return only data found or ignoreMissing=FALSE to download and extract missing data")
        return(NULL)
      }
    }
  }
  else
  {
    #else if missing nlYearMonths
    #if !missing(stats) return only given stats
    #else return the whole data frame
    if(existsCtryNlDataFile(ctryCode))
      ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode)))
    else
    {
      message("Data for ", ctryCode, " does not exist. Set IgnoreMissing=FALSE to download and process")
      ctryData <- NULL
    }

    return(ctryData)
  }

  #to remove any missing nlYearMonths if ignoreMissing==TRUE
  if(ignoreMissing)
  {
    if(any(existnlYMStats))
      existingCols <- apply(nlYmStats[existnlYMStats,], 1, function(x) getCtryNlDataColName(x[1], x[2]))
    else
      existingCols <- NULL
  }
  else 
  {
    #ignoreMissing == FALSE so we should have the missing data
    #check again to see that processNtLts was successful
    existnlYMStats <- apply(nlYmStats, 1, function(x) existsCtryNlDataVIIRS(ctryCode, x[1], x[2]))
    
    if(all(existnlYMStats))
      existingCols <- apply(nlYmStats[existnlYMStats,], 1, function(x) getCtryNlDataColName(x[1], x[2]))
    else
      stop("An error occurred")
  }
  
  if(length(existingCols) < 1)
  {
   message("No nightlight data. Returning ctry admin data only")
   #return(NULL)
  }
  else
  {
    message("Retrieving requested data")
  }

  ctryData <- as.data.frame(data.table::fread(getCtryNlDataFnamePath(ctryCode)))

  #get the names of the columns in the data file
  cols <- names(ctryData)

  #retain columns with country admin levels
  cols <- cols[grep("^[^NL_]", cols)]
  
  nlCols <- existingCols

  cols <- c(cols, nlCols)

  #add the column with the relevant yearmonth
  ctryData <- ctryData[,cols]

  return(ctryData)
}

######################## getCtryNlDataColName ###################################

#' Construct the name of a nightlight data column given the nightlight type and yearMonth
#'
#' Construct the name of a nightlight data column given the nightlight type and yearMonth. Used in
#'     creating and retrieving data columns from the nightlight data file
#'
#' @param nlYearMonth character vector. the yearMonth (concat year + month) in the format 
#'     "YYYYMM" e.g. "201203"
#'
#' @param stat
#' 
#' @param nlType character vector. the type of nightlight i.e. "OLS" or VIIRS. Default=VIIRS
#'
#' @return character string
#'
#' @examples
#' ctryCode <- "KEN"
#' dt <- read.csv(getCtryNlDataFnamePath(ctryCode))
#' dt <- dt[,getCtryNlDataColName("201612", nlType="VIIRS")]
#'
getCtryNlDataColName <- function(nlYearMonth, stat, nlType="VIIRS")
{
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")
  
  if(missing(stat))
    stop("Missing required parameter stat")

  if(!validNlYearMonthVIIRS(nlYearMonth))
    stop("Invalid nlYearMonth: ", nlYearMonth)
    
  if (!allValid(stat, validStat))
    stop("Invalid/unsupported stat detected")
  
  colName <- "NL_"
  
  if (nlType=="VIIRS")
    colName <- paste0(colName, "VIIRS_")
  else if(nlType == "OLS")
    colName <- paste0(colName, "OLS_")
  
  # colName <- paste0(colName, nlYearMonth, "_")
  # 
  # colName <- paste0(colName, toupper(stat))

  colName <- paste0(colName, sapply(nlYearMonth, function(x) paste0(x, "_", toupper(stat))))
  
  colName <- sort(colName)
  
  return(colName)
}

######################## validStat ###################################

#' Check if a statistic given is valid
#'
#' Check if a statistic given is valid
#'
#' @param stat the statistic to check
#'
#' @return TRUE/FALSE
#'
#' @examples
#'
#' validStat("sum")
#'  returns TRUE
#'
validStat <- function(stat)
{
  if(missing(stat))
    stop("Missing required parameter stat")
  
  if(!is.character(stat) || is.null(stat) || is.na(stat) || stat == "")
    stop("Invalid stat: ", stat)
  
  matchedFun <- tryCatch(
    {
      matched <- match.fun(stat)
    }, error = function(err)
    {
      message(paste0("Invalid stat: ", stat))
      matched <- NULL
      return(matched)
    }
  )

  #if (!tolower(stat) %in% c("sum", "mean", "median", "min", "max", "var", "sd"))
  if(is.null(matchedFun))
    return(FALSE)
  else
    return(TRUE)
}

######################## allValid ###################################

#' Check if a vector/list of values given is valid as per the given validation function
#'
#' Check if a vector/list of values given is valid as per the given validation function. 
#'     The function will also print a warning showing the values that are invalid. One can 
#'     stop the warning being printed by wrapping the function in the \code{suppressWarnings} 
#'     function.
#'
#' @param testData the list/vector of values to validate
#' 
#' @param testFun the validation function to test each value of testData against
#'
#' @return TRUE/FALSE
#'
#' @examples
#'
#' ctryCodes <- c("KE", "UGA", "RWA", "TZ")
#' allValid(ctryCodes, validCtryCode)
#'  returns TRUE
#'
allValid <- function(testData, testFun)
{
  valid <- sapply(testData, function(x) eval(parse(text="testFun(x)")))
  
  invalidData <- testData[!valid]
  
  if(length(invalidData) > 0)
    warning("Invalid data: ", paste0(invalidData, collapse = ", "))
  
  return(all(valid))
}
######################## existsCtryNlDataFile ###################################

#' Check if a country's data file exists
#'
#' Check if a country's data file exists
#'
#' @param ctryCode the ISO3 country code
#'
#'
#' @return TRUE/FALSE
#'
#' @examples
#' ctryCode <- "KEN"
#' if(existsCtryNlDataFile(ctryCode))
#'  message("Data file for ", ctryCode, " found")
#'
existsCtryNlDataFile <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid/Unknown ctryCode: ", ctryCode)
  
  #for polygons look for shapefile dir
  return(file.exists(getCtryNlDataFnamePath(ctryCode)))
}

######################## existsPolyFnamePath ###################################

#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' @param ctryCode
#'
#' @return TRUE/FALSE
#'
#' @examples
#' existsPolyFnamePath("KEN")
#'  TRUE/FALSE
#'
existsPolyFnamePath <- function(ctryCode)
{
  #for polygons look for shapefile dir
  return(dir.exists(getPolyFnamePath(ctryCode)))
}

######################## existsPolyFnameZip ###################################

#' Check if the compressed country polygon has been downloaded and stored in the polygon folder
#'
#' Check if the compressed country polygon has been downloaded and stored in the polygon folder
#'
#' @param ctryCode
#'
#' @return TRUE/FALSE
#'
#' @examples
#' existsPolyFnameZip("KEN")
#'  #returns TRUE/FALSE
#'
existsPolyFnameZip <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  return(file.exists(getPolyFnameZip(ctryCode)))
}

######################## getCtryShpLyrName ###################################

#' Get the standard name of a polygon layer for a country
#'
#' Get the standard name of a polygon layer for a country. Used to refer to a polygon layer by name.
#'     i.e. for CTRYCODE & lyrNum="0": lyrName="CTRYCODE_adm0", lyrNum="1": lyrName="KEN_adm1". Note this #'     is different from the country official administration level name.
#'
#' @param ctryCode the ISO3 code for the country
#'
#' @param lyrNum the order of the layer starting from 0 = country level, 1 = first admin level
#'
#' @return Character layer name
#'
#' @examples
#' lyrName <- getCtryShpLyrName("KEN","0")) #top layer name
#'   #returns "KEN_adm0"
#'   
getCtryShpLyrName <- function(ctryCode, lyrNum)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)

  return(paste0(ctryCode, "_adm", lyrNum))
}

######################## getCtryShpLowestLyrName ###################################

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for
#'     "VIIRS" nightlight type
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#'
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
getCtryShpLowestLyrName <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)

  layers <- rgdal::ogrListLayers(path.expand(getPolyFnamePath(ctryCode)))

  admLayers <- layers[grep("adm", layers)]

  admLayerNums <- gsub("[^[:digit:]]", "", admLayers)

  lowestAdmLyrName <- admLayers[order(as.numeric(admLayerNums),decreasing = T)][1]

  return(lowestAdmLyrName)
}

######################## getCtryPolyAdmLevelNames ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
getCtryPolyAdmLevelNames <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ISO3 ctryCode: ", ctryCode)
  
  lowestLayer <- getCtryShpLowestLyrName(ctryCode)

  numLayers <- ctryShpLyrName2Num(lowestLayer)

  admLevels <- NULL

  if (numLayers > 0)
    for (lyrNum in 1:numLayers)
    {
      lyrPoly <- rgdal::readOGR(path.expand(getPolyFnamePath(ctryCode)), getCtryShpLyrName(ctryCode, lyrNum))

      lvlTypeName <- paste0("TYPE_",lyrNum)

      lvlName <- as.character(unlist(lyrPoly@data[2, eval(lvlTypeName)]))

      lvlTypeEngName <- paste0("ENGTYPE_",lyrNum)

      lvlEngName <- as.character(unlist(lyrPoly@data[2,eval(lvlTypeEngName)]))

      if ((!is.na(lvlName) && !is.na(lvlEngName)) && lvlName != lvlEngName)
        lvlName <- paste0(lvlEngName, "_(", lvlName, ")")

      if (is.na(lvlName))
        lvlName <- "Unknown"

      admLevels <- c(admLevels, as.character(lvlName))
    }

  #admLevels <- as.data.frame(cbind(1:numLayers, admLevels))

  #names(admLevels) <- c("id", "name")

  return (admLevels)
}

######################## validCtryCode ###################################

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for
#' "VIIRS" nightlight type
#'
#' @param ctryCode the ISO3 country code to validate
#'
#' @return TRUE/FALSE
#'
#' @examples
#' validNlCtryCode("KEN")
#'  #returns TRUE
#'
#' validNlCtryCode("UAE")
#'  #returns FALSE. "United Arab Emirates" ISO3 code = "ARE"
#'
validCtryCode <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  #if the format is invalid return FALSE no need to return an error
  if (class(ctryCode) != "character" || is.null(ctryCode) || is.na(ctryCode) || ctryCode =="" || length(grep("[^[:alpha:]]", ctryCode) > 0))
    return(FALSE)
  
  if(is.na(suppressWarnings(ctryCodeToName(ctryCode))))
    return(FALSE)
  else
    return(TRUE)
}

######################## dnldCtryPoly ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
dnldCtryPoly <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)

  fullPolyUrl <- getCtryPolyUrl(ctryCode)

  result <- NULL

  #if the path doesn't exist
  if (!existsPolyFnamePath(ctryCode))
  {
    if (!existsPolyFnameZip(ctryCode))
    {
      if(download.file(url = getCtryPolyUrl(ctryCode), destfile = getPolyFnameZip(ctryCode), method = "wget", mode = "wb", extra = "-c") == 0)
      {
        result <- utils::unzip(getPolyFnameZip(ctryCode), exdir = getPolyFnamePath(ctryCode))
        file.remove(getPolyFnameZip(ctryCode))
      }
    }else
    {
      result <- utils::unzip(getPolyFnameZip(ctryCode), exdir = getPolyFnamePath(ctryCode))
      file.remove(getPolyFnameZip(ctryCode))
    }
  }
  else
  {
    message("Polygon ", ctryCode, " already exists")
  }

  return (!is.null(result))
}

######################## getAllNlYears ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
getAllNlYears <- function(nlType = "VIIRS")
{
  if (class(nlType) != "character" || is.null(nlType) || is.na(nlType) || nlType =="" || length(grep("[^[:alpha:]]", nlType) > 0))
    stop("Invalid nlType: ", nlType)
  
  if (nlType == "OLS")
    return (1992:2013)
  else if (nlType == "VIIRS")
  {
    yrs <- 2012:lubridate::year(lubridate::now())
    mths <- c(paste("0",1:9, sep= ""),10:12)

    currYrMth <- paste0(lubridate::year(lubridate::now()), ifelse(lubridate::month(lubridate::now())<10, paste("0", lubridate::month(lubridate::now()), sep=""), lubridate::month(lubridate::now())))

    nlYrMths <- unlist(lapply(yrs, FUN = function(x) paste(x,mths,sep="")))

    nlYrMths <- nlYrMths[nlYrMths >= "201204" & nlYrMths <= currYrMth]

    return (nlYrMths)
  }
  else
    return()
}

######################## getAllNlCtryCodes ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
getAllNlCtryCodes <- function(omit="none")
{
  #omit is a vector and can contain "long", "missing" or "error"
  #if omit is "none" do not exclude any countries
  #if omit is "all", empty or NA set to default: omit=c("long", "missing", "error")

  omit <- tolower(omit)

  if(omit != "none" && (omit == "all" || !(omit %in% c("long", "missing", "error")) || is.na(omit)))
    omit <- c("long", "missing", "error")

  tooLongProcessing <- ""
  missingPolygon <- ""
  errorProcessing <- ""

  if ("long" %in% omit)
    tooLongProcessing <- c("RUS", "BRA", "USA", "ATA", "FRA")

  if ("missing" %in% omit)
    missingPolygon <- c("CYN",  "KOS", "Ashm", "Gaza", "IOA", "KAS")

  if ("error" %in% omit)
    errorProcessing <- c("ATF", "GNQ", "KIR", "NZL", "CAN", "MUS")

  #consolidate the list of countries to omit
  omitCountries <- unlist(c(tooLongProcessing, missingPolygon, errorProcessing))

  #rworldmap has more country codes in countryRegions$ISO3 than in the map itself
  #select ctryCodes from the map data itself
  map <- rworldmap::getMap()

  #some polygons have problems. use cleangeo package to rectify
  map <- cleangeo::clgeo_Clean(map)

  #get the list of country codes from the rworldmap
  ctryCodes <- as.character(map@data$ISO3)

  #remove all omitCountries from the list
  ctryCodes <- subset(ctryCodes, !(ctryCodes %in% omitCountries))

  #sort the country codes in ascending alphabetical order
  ctryCodes <- ctryCodes[order(ctryCodes)]

  return (ctryCodes)
}

######################## getNlType ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
getNlType <- function(nlYear)
{
  if(missing(nlYear))
    stop("Missing required parameter nlYear")
  
  if(is.null(nlYear) || is.na(nlYear) || (!is.numeric(nlYear) && is.na(as.numeric(nlYear))))
    stop("Invalid value for nlYear: ", nlYear)
  
  nlYear <- as.numeric(nlYear)
  
  if (nlYear < 1992 || nlYear > lubridate::year(lubridate::now()))
    return(NA)

  if (nlYear > 1992 && nlYear < 2012)
    return("OLS")
  else
    return("VIIRS")
}

######################## getPolyFname ###################################

#' Returns the directory name of the unzipped shapefile downloaded from GADM.ORG without the path
#'
#' Returns the directory name of the unzipped shapefile downloaded from \url{www.GADM.ORG} without the path
#'
#' @param ctryCode character the ISO3 code of the country
#'
#' @return character path to zip
#'
#' @examples
#' getPolyFnameZip("KEN")
#'  returns "path/to/"
#'
getPolyFname <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(ctryCode, "_adm_shp")

  return (polyFname)
}

######################## getPolyFnamePath ###################################

#' Get the path of the unzipped polygon directory downloaded from GADM.ORG
#'
#' Get the path of the unzipped polygon directory downloaded from GADM.ORG. Note the polygons are in ESRI
#'     Shapefile format thus when unzipped create a directory with the name <ctrycode>_adm_shp e.g.
#'     KEN_adm_shp. The directory will contain a number of files including the .shp file. 
#'     \code{rgdal::readOGR} can read a shapefile polygon when given the directory path. It will
#'     determine which files to read.
#'
#' @param ctryCode character the ISO3 code of the country
#'
#' @return character path to polygon directory
#'
#' @examples
#' getPolyFnameZip("KEN")
#'  returns "path/to/"
#'
getPolyFnamePath <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  #check for the shapefile directory created with
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFnamePath <- file.path(getNlDir("dirPolygon"), getPolyFname(ctryCode))

  return (polyFnamePath)
}

######################## getPolyFnameZip ###################################

#' Get the filename of the polygon zip file as downloaded from \url{http://www.GADM.ORG}
#'
#' Get the filename of the polygon zip file as downloaded from \url{http://www.GADM.ORG}
#'
#' @param ctryCode character the ISO3 code of the country
#'
#' @return character path to zip
#'
#' @examples
#' getPolyFnameZip("KEN")
#'  returns "path/to/"
#'
getPolyFnameZip <- function(ctryCode)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  #format of shapefiles is <ctryCode>_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(getPolyFnamePath(ctryCode),".zip")

  return (polyFname)
}

######################## getNlYearMonthTilesOLS ###################################

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
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'
getNlYearMonthTilesOLS <- function(nlYearMonth, tileList)
{
  success <- TRUE

  for (tile in tileList)
  {
    nlYear <- substr(nlYearMonth, 1, 4)

    nlMonth <- substr(nlYearMonth, 5, 6)

    nlTile <-

    #download tile
    success <- success && getNtLtsOls(nlYear, nlMonth, nlTile)
  }

  return (success)
}

######################## getNlYearMonthTilesVIIRS ###################################

#' Download the listed VIIRS tiles for a particular nlYearMonth
#'
#' Download the listed VIIRS tiles for a particular nlYearMonth
#'
#' @param nlYearMonth character the nlYearMonth to process in the format "YYYYMM"
#'
#' @param tileList integer vector or character vector of digits containing valid tile numbers
#'
#' @return TRUE/FALSE if the download was successful
#' 
#' @examples
#' #download tiles for "KEN" which are tiles 2 and 5 for the specified time periods
#' getNlYearMonthsTilesVIIRS("201201", c(2, 5))
#'
#' #same as above but getting the tileList automatically
#' getNlYearMonthsTilesVIIRS("201201", tileName2Idx(getCtryCodeTileList("KEN")))
#' 
#' returns TRUE if the download was successful
#'
getNlYearMonthTilesVIIRS <- function(nlYearMonth, tileList)
{
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")

  if(missing(tileList))
    stop("Missing required parameter tileList")
  
  if(!validNlYearMonthVIIRS(nlYearMonth))
    stop("Invalid nlYearMonth: ", nlYearMonth)
  
  if(!allValid(tileList, validNlTileNameVIIRS))
    stop("Invalid tileNum detected")
  
  success <- TRUE

  #ensure we have all required tiles
  for (tile in tileList)
  {
    nlYear <- substr(nlYearMonth, 1, 4)

    nlMonth <- substr(nlYearMonth, 5, 6)

    nlTile <- tileName2Idx(tile)

    print(paste0(nlYear, nlMonth, nlTile))

    #download tile
    success <- success && getNtLtsVIIRS(nlYear, nlMonth, nlTile)
  }

  return (success)
}

######################## getAllNlYearMonthsTiles ###################################

#' Downloads all the listed VIIRS tiles for a particular year, month
#'
#' Downloads all the listed VIIRS tiles for a particular year, month
#'
#' @param nlYearMonths character vector a list of the nlYearMonths to process in the format "YYYYMM"
#'
#' @param tileList character vector a list of tile numbers (1-6) to download
#'
#' @return TRUE/FALSE whether the download was successful
#'
#' @examples
#' #download tiles for "KEN" which are tiles 2 and 5 for the specified time periods
#' getAllNlYearMonthsTiles(c("201201", "201202", "201205"), c(2, 5))
#'
#' #same as above but getting the tileList automatically
#' getAllNlYearMonthsTiles(c("201201", "201202", "201205"), tileName2Idx(getCtryCodeTileList("KEN")))
#' #returns TRUE if ALL downloads were successful
#'
getAllNlYearMonthsTiles <- function(nlYearMonths, tileList)
{
  if(missing(nlYearMonths))
    stop("Missing required parameter nlYearMonths")
  
  if(missing(tileList))
    stop("Missing required parameter tileList")
  
  if(!allValid(nlYearMonths, validNlYearMonthVIIRS))
    stop("Invalid nlYearMonth detected")
  
  if(!allValid(tileList, validNlTileNumVIIRS))
    stop("Invalid tileNum detected")
  
  #all good we can start
  success <- TRUE
  
  for (nlYearMonth in nlYearMonths)
  {
    success <- success && getNlYearMonthTilesVIIRS(nlYearMonth, tileList)
  }
  
  return (success)
}

######################## existsCtryCodeTiles ###################################

#' Check if the variable ctryCodeTiles exists
#'
#' Check if the variable ctryCodeTiles, which is a mapping between countryCodes and the tiles it intersects with, exists as created by \code{mapCtryPolyToTiles()}.
#'
#' @return TRUE/FALSE
#'
#' @examples
#' if(!existsCtryCodeTiles())
#'   mapCtryPolyToTiles()
#'
existsCtryCodeTiles <- function()
{
  return (exists("ctryCodeTiles") && class(ctryCodeTiles)=="data.frame" && !is.null(ctryCodeTiles))
}

######################## getCtryCodeTileList ###################################

#' Returns a list of VIIRS nightlight tiles that a country or countries intersects with
#'
#' Given a list of countries, this function will provide alist of VIIRS nightlight tiles 
#'     that intersect with them. This helps in processing multiple countries by determining
#'     which nightlight tiles are required for processing by allowing the download of all 
#'     required tiles before processing. 
#'
#' @param ctryCodes character vector of country codes to process
#'
#' @param omitCountries countries to exclude from processing. This is helpful when the 
#'     number of countries to exclude is smaller than the number to process e.g. when 
#'     one wants to process all countries and exclude countries that take long to process i.e. 
#'     omitCountries = "long"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' getCtryCodeTileList(ctryCodes=c("BUR", "KEN", "RWA", "UGA", "TZA"), omitCountries="none")
#' 
#' getCtryCodeTileList(ctryCodes="all", omitCountries="long")
#'
getCtryCodeTileList <- function(ctryCodes, omitCountries="none")
{
  if(missing(ctryCodes))
    stop("Missing required parameter ctryCodes")

  if(!allValid(ctryCodes, validCtryCode))
    stop("Invalid ctryCode(s) detected")
  
  ctryTiles <- unlist(mapCtryPolyToTiles(ctryCodes, omitCountries)$tiles)

  return (ctryTiles)
}

######################## existsCtryNlDataVIIRS ###################################

#' Check if VIIRS nightlight data for a country in a given year and month already exists locally
#'
#' Check if VIIRS nightlight data for the country exists in the country nightlight data file. 
#'     First checks if the country nightlight data file exists.
#'
#' @param ctryCode character the ISO3 code of the country
#'
#' @param nlYearMonth character the year and month concatenated
#' 
#' @param stat character the stat to check for
#'
#' @return TRUE/FALSE
#'
#' @examples
#' existsCtryNlDataVIIRS("KEN", "201204")
#'
existsCtryNlDataVIIRS <- function(ctryCode, nlYearMonth, stat)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")
  
  if(missing(stat))
    stop("Missing required parameter stat")
  
  # #number of arguments
  # if(length(ctryCode) > 1)
  #   stop("Please supply only 1 ctryCode to check")
  # 
  # if(length(nlYearMonth) > 1)
  #   stop("Please supply only 1 nlYearMonth to check")
  # 
  # if(length(stat) > 1)
  #   stop("Please supply only 1 stat to check")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validNlYearMonthVIIRS(nlYearMonth))
    stop("Invalid nlYearMonth: ", nlYearMonth)
  
  if(!validStat(stat))
    stop("Invalid stat: ", stat)
    
  if (!existsCtryNlDataFile(ctryCode))
    return (FALSE)

  dt <- utils::read.csv(getCtryNlDataFnamePath(ctryCode), nrow=1, header=TRUE)

  hd <- names(dt)

  if (length(grep(getCtryNlDataColName(nlYearMonth = nlYearMonth, stat = stat, "VIIRS"), hd)) > 0)
    return(TRUE)
  else
    return(FALSE)
}

######################## processNtLts ###################################

#' Downloads nightlight tiles and country polygons and calls the function to process them
#'
#' Downloads nightlight tiles and country polygons in preparation for the appropriate functions
#'     to process them. Given a list of countries and nlYearMonths and an nlType, processNtLts 
#'     will first determine which countries and periods do not actually have data i.e. have not 
#'     been previously processed. From the list of countries and prediods that need processing,
#'     it determines which nightlight tiles require to be downloaded. At the same time, it 
#'     downloads any country polygons which have not already been downloaded.
#' 
#'     processNtLts then calls the appropriate function to process the data depending on the type 
#'     of nightlights. i.e. OLS = processNlCountryOLS and VIIRS = processNlCountryVIIRS
#' 
#'     This is the main entry-point to the package and the main user-facing function. However, it 
#'     works in batch mode and caches all data without returning any data to the user. However, it 
#'     will return TRUE/FALSE depending on whether it completed successfully. Since it saves state
#'     regularly it can be run multiply in case of failure until it finally returns TRUE. This is where 
#'     the only constraints are downloading and processing errors due to bandwidth or other 
#'     resource constraints. A good example is running a long-running batch on an AWS EC2 
#'     spot-priced machine where the server may be decommissioned at any time. See more in the examples.
#' 
#' @param ctryCodes the list of countries to be processed
#'
#' @param nlYearMonths the concatenated year+month(s) to be processed e.g. ""201205"
#'
#' @param nlType the type of nightlights to process i.e. "OLS" or "VIIRS". Default "VIIRS"
#' 
#' @param stats the statistics to calculate. If not provided will calculate the stats specified 
#'     in \code{pkg_options("stats")}
#'
#' @return None
#'
#' @examples
#'
#' #Example 1: process VIIRS nightlights for all countries and all periods available e.g. to create 
#'     a local cache or repo
#'     
#'     #Recommend running initNtLts() to improve performance. It stores some global variables 
#'     so that they do not have to be re-evaluated multiply
#'     initNtLts() 
#'     
#'     processNtLts() #process VIIRS nightlights for all countries and all periods
#'
#' #Example 2: process nightlights for all countries in 2012 only
#'     
#'     initNtLts() #for performance. See Example 1
#'
#'     nlYrMths <- getAllNlYears() #get a list of all nightlight periods to present-day
#'
#'     nlYrMths <- nlYrMths[grep("^2012", nlYrMths)] #filter only periods in 2012
#'
#'     processNtLts(nlYearMonths=nlYrMths)
#'
#' #Example 3: process VIIRS nightlights for countries KEN & RWA in 2014 Jan to 2014 May only
#'     
#'     initNtLts()
#'
#'     cCodes <- c("KEN", "RWA")
#'
#'     nlYrMths <- getAllNlYears()
#'
#'     nlYrMths <- nlYrMths[grep("^20120[1-5]", nlYrMths)]
#'
#'     processNtLts(ctryCodes=cCodes, nlYearMonths=nlYrMths)
#' 
#' #Example 4: process VIIRS nightlights for countries KEN & RWA in 2014 Oct to 2014 Dec only
#'
#'     processNtLts(ctryCodes=c("KEN", "RWA"), nlYearMonths=c("201410", "201411", "201412"))
#'     
#' #Example 5: process all nightlights, all countries, all stats in one thread
#' 
#'    processNtLts() 
#'    
#' #Example 6: process all nightlights, all countries, all stats with each
#' #   year in a separate thread. Create a separate R script for each year as follows:
#' 
#'     library(Rnightlights)
#' 
#'     initNtLts()
#' 
#'     nlYearMonths <- getAllNlYears()
#' 
#'     nlYearMonths_2012 <- nlYearMonths[grep("^2012", nlYearMonths)]
#' 
#'     processNtLts(nlYearMonths=nlYearMonths_2012)
#' 
#'     #Run the script from the command line as:
#'     
#'     #R CMD BATCH script_name_2012.R
#'     
#' @export
processNtLts <- function (ctryCodes=getAllNlCtryCodes("all"), nlYearMonths=getAllNlYears(), nlType="VIIRS", stats=pkg_options("stats"))
{
  #nlYearMonths is a character vector with each entry containing an entry of the form YYYYMM (%Y%m)
  #e.g. 201401 representing the month for which nightlights should be calculated
  #use provided list
  #if none given default to all year_months
  #TODO:
  #1. if years only given, infer months
  #2. verification & deduplication
  #3. OLS

  #if the period is not given process all available periods
  if(missing("nlYearMonths") || is.null(nlYearMonths) || length(nlYearMonths) == 0 || nlYearMonths == "")
  {
    nlYears <- getAllNlYears(nlType)
  }

  if(!allValid(ctryCodes, validCtryCode))
    stop("Invalid ctryCode detected")
  
  if(!allValid(nlYearMonths, validNlYearMonthVIIRS))
  {
    stop("Invalid nlYearMonths detected: ", nlYearMonths)
  }
  
  #if the tile mapping does not exist create it
  if (!existsNlTiles())
    nlTiles <- getNlTiles()
  
  #use supplied list of ctryCodes in ISO3 format else use default of all
  #TODO:
  #1.accept other formats and convert as necessary
  #2.verification & deduplication
  if (is.null(ctryCodes))
  {
    #get list of all country codes
    ctryCodes <- getAllNlCtryCodes(omit = "all")
  }

  ##First step: Determine which tiles are required for processing. This is determined by the
  #list of ctryCodes. Since theoretically we need the polygons of all countries to determine which
  #tiles to download we take the opportunity to download all required shapefiles
  #Practically, we can stop as soon as all 6 tiles are flagged for download.

  ##If any tiles cannot be found/downloaded then abort and try for next country
  #we probably need to flag failed downloads so we don't try to process them and report back to user

  #download all country polygons if they don't already exist
  for (ctryCode in ctryCodes)
  {
    dnldCtryPoly(ctryCode)
  }

  #for all nlYearMonths check if the tiles exist else download
  for (nlYearMonth in nlYearMonths)
  {
    message("Checking tiles required for ", nlYearMonth)

    nlType <- getNlType(substr(nlYearMonth,1,4))

    #init the list of tiles to be downloaded
    tileList <- NULL

    #determine tiles to download
    if (nlType == "VIIRS")
    {
      ctryTiles <- NULL

      tileList <- NULL

      #For each country
      for (ctryCode in unique(ctryCodes))
      {
        if (all(sapply(stats, function(stat) existsCtryNlDataVIIRS(ctryCode, nlYearMonth, stat))))
        {
          message ("All stats exists for ", ctryCode, ":", nlYearMonth)

          next
        }

        message("Stats missing. Adding tiles for ", ctryCode)

        #get the list of tiles required for the ctryCode
        ctryTiles <- getCtryCodeTileList(ctryCode)

        #combine the list of unique tiles across all ctryCodes in tileList
        tileList <- c(tileList, setdiff(ctryTiles, tileList))

        #if all the unique tiles have been listed no need to proceed checking
        if (length(tileList) == nrow(nlTiles))
        {
          message ("All tiles have been listed. No need to check other country tiles")

          break
        }
      }

      if (length(tileList) == 0)
      {
        message("No tiles needed for ", nlYearMonth, ". Process next nlYearMonth")

        next
      }
      else #if the cropped raster is not found try to download
      {
        if (!file.exists(getCtryRasterOutputFname(ctryCode, nlYearMonth)))
        {
            if(!getNlYearMonthTilesVIIRS(nlYearMonth, tileList))
            {
              print("Something went wrong with the tile downloads. Aborting ...")
      
              break
            }
        }
        else
        {
          print("Cropped raster already exists. Skipping tile download")
        }
      }
      
      #for all required countries
      for (ctryCode in unique(ctryCodes))
      {
        processNLCountryVIIRS(ctryCode, nlYearMonth, cropMaskMethod = pkg_options("cropMaskMethod"), extractMethod = pkg_options("extractMethod"), fnStats = stats)
      }

      #post-processing. Delete the downloaded tiles to release disk space
      if(pkg_options("deleteTiles"))
        for (tile in tileList)
        {
          nlYear <- substr(nlYearMonth, 1, 4)
          nlMonth <- substr(nlYearMonth, 5, 6)
  
          #del the tif file
          #file.remove(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileName2Idx(tile)))
  
          #del the zip file
          #file.remove(getNtLtsZipLclNameVIIRS(nlYear, nlMonth, tileName2Idx(tile)))
        }
    }
    else if (nlType == "OLS")
    {
      if (!getNlYearMonthTilesOLS(nlYearMonth))
      {
        print("Something went wrong with the tile downloads. Aborting ...")

        break
      }

      #for all required countries
      for (ctryCode in unique(ctryCodes))
      {
        processNLCountryOls(ctryCode, nlYearMonth)
      }

    }
  }
}

######################## initNtLts ###################################

#' Initialize some important variables and create directory structure
#'
#' Initialize some important variables and create directory structure
#'
#' @return NULL
#'
#' @examples
#'
#'  initNtLts()
#'
initNtLts <- function(omitCountries="none")
{
  #create the nlTiles and tileSpPolysDFs variables in the global environment to speed up processing
  #the two speed up tile lookup
  nlTiles <<- getNlTiles()

  tilesSpPolysDFs <<- createNlTilesSpPolysDF()
}

######################## cleanupNtLts ###################################

#' Clean up the environment after processing (Not yet implemented)
#'
#' Clean up the environment after processing (Not yet implemented)
#'
#' @return NULL
#'
#' @examples
#'  cleanup()
#'
#' @export
cleanupNtLts <- function()
{
  #remove any global vars we might have used
  suppressWarnings(rm(map, shpTopLyrName, wgs84, nlTiles, tilesSpPolysDFs))
  
  #the destructor

  #del temp files
}

######################## myZonal ###################################

#' Calculate zonal statistics. Used internally
#'
#' Calculate zonal statistics. Used internally by zonalpipe. Modified from 
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param x the country raster
#'
#' @param z the zonal country polygon layer
#'
#' @param stats a character list of statistics to calculate
#'
#' @param digits round off to how many decimals
#'
#' @param na.rm how to handle NAs
#'
#' @param ...
#'
#' @return numeric value result of the given stat function
#'
#' @examples
#' myZonal
#'
myZonal <- function (x, z, stats, digits = 0, na.rm = TRUE, ...)
{
  #http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  
  #fun <- match.fun(stat)
  
  #fun <- paste0(sapply(stats, function(stat) paste0(stat,"=sapply(.SD, ", stat, ", na.rm = TRUE)")), collapse = ", ")
  
  varx <- function(x, ...) ifelse(length(x) > 1, var(x, ...), x)
  
  statsFn <- lapply(stats, function(x) switch(x, sum="sum", mean="mean", var="varx"))
  
  fun <- paste0(sapply(stats, function(stat) paste0(stat,"=", stat, "(x, na.rm = TRUE)")), collapse = ", ")
  
  funs <- paste0("rDT[, as.list(unlist(lapply(.SD, function(x) list(", fun, ")))), by=z]")
  
  
  #result[,list(sum=sum(vals.sum, na.rm = TRUE),mean=mean(vals.mean, na.rm = TRUE)), by = z]
  
  vals <- NULL
  
  zones <- NULL
  
  blocks <- raster::blockSize(x)
  
  result <- NULL
  
  for (i in 1:blocks$n)
  {
    message("Block: ", i)
    
    message("Reading X")
    vals <- raster::getValues(x, blocks$row[i], blocks$nrows[i])
    
    vals[vals < 0] <- NA
    
    message("Reading Zones")
    zones <- round(raster::getValues(z, blocks$row[i], blocks$nrows[i]), digits = digits)
    
    rDT <- data.table::data.table(vals, z=zones)
    
    #setkey(rDT, z)
    
    message("Calculating partial stats")
    #result <- rbind(result, rDT[, lapply(.SD, fun, na.rm = TRUE), by=z])
    result <- rbind(result, eval(parse(text = funs)))
  }
  
  resultfun <- paste0(paste0(stats,"="),paste0(stats, paste0("(",names(result)[2:ncol(result)],", na.rm=TRUE)")), collapse = ", ")
  
  resultfuns <- paste0("result[, list(", resultfun, "), by=z]")
  
  result <- eval(parse(text = resultfuns))
  
  result <- setNames(result, c("z",stats))
  
  gc()
  
  return(result)
}

######################## ZonalPipe ###################################

#' Create a zonal file if it does not exist and calculate the zonal stats
#'
#' Create a zonal file if it does not exist and calculate the zonal stats by calling the 
#'     myZonal function. Modified from 
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param ctryCode
#'
#' @param ctryPoly
#'
#' @param path.in.shp
#'
#' @param path.in.r
#'
#' @param path.out.r
#'
#' @param path.out.shp
#'
#' @param zone.attribute
#'
#' @param stat
#'
#' @return TRUE/FALSE
#'
ZonalPipe <- function (ctryCode, ctryPoly, path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stats)
{
  #Source: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  #path.in.shp: Shapefile with zone (INPUT)
  #path.in.r: Raster from which the stats have to be computed (INPUT)
  #path.out.r: Path of path.in.shp converted in raster (intermediate OUTPUT)
  #path.out.shp: Path of path.in.shp with stat value (OUTPUT)
  #zone.attribute: Attribute name of path.in.shp corresponding to the zones (ID, Country...)
  #stat: function to summary path.in.r values ("mean", "sum"...)

  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")

  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(path.in.shp))
    stop("Missing required parameter path.in.shp")
  
  if(missing(path.in.r))
    stop("Missing required parameter path.in.r")
  
  if(missing(path.out.r))
    stop("Missing required parameter path.out.r")
  
  if(missing(zone.attribute))
    stop("Missing required parameter zone.attribute")
  
  if(missing(stats))
    stop("Missing required parameter stats")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!allValid(stats, validStat))
    stop("Invalid stat(s) detected")
  
  # 1/ Rasterize using GDAL

  #Initiate parameter
  r<-raster::stack(path.in.r)

  if (!file.exists(path.out.r))
  {
    message("Zonal file ", path.out.r, " doesn't exist. Creating")

    #get the extent and change to minx, miny, maxx, maxy order for use in gdal_rasterize. Explanation below
    ext<-raster::extent(r)
    ext<-paste(ext[1], ext[3], ext[2], ext[4])

    #get the resolution of the raster. will be used in gdal_rasterize for target resolution which should be the same as the source resolution. Specifying makes it run faster (?)
    res<-paste(raster::res(r)[1], raster::res(r)[2])

    lowestLyrName <- getCtryShpLowestLyrName(ctryCode)
    lowestIDCol <- paste0("ID_", gsub("[^[:digit:]]", "", lowestLyrName))

    #Gdal_rasterize
    message("Creating zonal raster")
    command<-'gdal_rasterize'
    command<-paste(command, paste0("--config GDAL_CACHEMAX ", pkg_options("gdal_cachemax"))) #Speed-up with more cache (avice: max 1/3 of your total RAM)
    command<-paste(command, "-l", lowestLyrName)
    #command<-paste(command, "-where", paste0(lowestIDCol, "=", i))
    command<-paste(command, "-a", zone.attribute) #Identifies an attribute field on the features to be used for a burn in value. The value will be burned into all output bands.
    command<-paste(command, "-te", as.character(ext)) #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed in georeferenced units. If not specified, the extent of the output file will be the extent of the vector layers.
    command<-paste(command, "-tr", res) #(GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced units. Both must be positive values.
    #command<-paste(command, "-a_nodata", 0)
    command<-paste(command, path.in.shp)
    command<-paste(command, "temprast.tif")

    system(command)

    message("Compressing zonal raster")
    gdalUtils::gdal_translate(co = "compress=LZW", src_dataset = "temprast.tif", dst_dataset = path.out.r)

    file.remove("temprast.tif")
  }

  message("Zonal file ", path.out.r, " found")

  # 2/ Zonal Stat using myZonal function
  zone<-raster::raster(path.out.r)

  message("Calculating zonal stats ...")
  Zstat<-data.frame(myZonal(r, zone, stats))

  message("Calculating zonal stats ... DONE")

  #colnames(Zstat)[2:length(Zstat)]<-paste0("B", c(1:(length(Zstat)-1)), "_",stats)

  colnames(Zstat)[2:length(Zstat)] <- stats
  
  return(Zstat)

  # 3/ Merge data in the shapefile and write it #Not required at this point
  #shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))

  #shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])

  #writeOGR(shp, path.out.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)), driver="ESRI Shapefile")
}

######################## fnSumAvgRadGdal ###################################

#' Calculate zonal statistics using GDAL. Faster than fnSumAvgRadRast for large polygons.
#'
#' Calculate zonal statistics. Alternative to fnSumAvgRadRast using GDAL. Faster for large polygons. 
#'     Modified from \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param ctryCode the ISO3 country code to be processed
#'
#' @param ctryPoly the loaded country polygon layer
#'
#' @param nlYearMonth the concatenated year+month to be processed
#'
#' @return data.frame of polygon attributes and the calculated stats
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
fnSumAvgRadGdal <- function(ctryCode, ctryPoly, nlYearMonth, fnStats=stats)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlYearMonth))
    stop("Missing required parameter nlYearMonth")

  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!validNlYearMonthVIIRS(nlYearMonth))
    stop("Invalid nlYearMonth: ", nlYearMonth)
  
  if(!allValid(fnStats, validStat))
    stop("Invalid stat(s) detected")
  
  #source: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  
  path.in.shp<- getPolyFnamePath(ctryCode)
  
  path.in.r<- getCtryRasterOutputFname(ctryCode, nlYearMonth) #or path.in.r<-list.files("/home/, pattern=".tif$")
  path.out.r<- paste0(getNlDir("dirZonals"), "/", ctryCode, "_zone.tif")

  path.out.shp<-"zone_withZstat.shp"

  zone.attribute<-paste0("ID_", gsub("[^[:digit:]]", "", getCtryShpLowestLyrName(ctryCode)))

  lowestLyrName <- getCtryShpLowestLyrName(ctryCode)

  lowestIDCol <- paste0("ID_", gsub("[^[:digit:]]", "", lowestLyrName))

  #ctryPoly <- readOGR(getPolyFnamePath(ctryCode), lowestLyrName)

  sumAvgRad <- ZonalPipe(ctryCode, ctryPoly, path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stats=fnStats)

  ctryPolyData <- ctryPoly@data

  ctryPolyData[,lowestIDCol] <- as.integer(ctryPolyData[,lowestIDCol])

  ctryPolyData <- ctryPolyData[order(ctryPolyData[,lowestIDCol]),]

  #if there is only the country adm level i.e. no lower adm levels than the country adm level then we only have 1 row each but IDs may not match as seen with ATA. treat differently
  #since we do not have IDs to merge by, we simply cbind the columns and return column 2
  
  #cols <- paste0("B", c(1:(length(fnStats))), "_",fnStats)
  
  if (lowestIDCol == "ID_0")
  {
    sumAvgRad <- cbind(ctryPolyData$ID_0, sumAvgRad[sumAvgRad$z!=0, cols])

    sumAvgRad <- sumAvgRad[,2]
  }
  else
  {
    sumAvgRad <- merge(ctryPolyData, sumAvgRad, by.x=lowestIDCol, by.y="z", all.x=T, sort=T)

    #sumAvgRad <- names(sumAvgRad) <-  c(names(sumAvgRad), fnStats)
  }

  return(sumAvgRad)
}

######################## fnSumAvgRadRast ###################################

#' Calculate statistics on a nightlight raster that fall within a polygon
#'
#' Calculate the sum of the radiance of the pixels in a nightlight raster
#'     that fall within a polygon and its subpolygons using the \code{raster} 
#'     package. Given a country polygon with subpolygons representing lower 
#'     admin levels, it will crop and mask the raster to each subpolygon and 
#'     calculate the total radiance for the polygon and return a vector of total
#'     radiances that matches the subpolygons
#'
#' @param ctryPoly The polygon of the admin level/region of interest. In general is a country polygon
#'     with sub-regions usually the lowest known admin level as given by the GADM polygons.
#'
#' @param ctryRastCropped The raster containing nightlight radiances to sum. Usually will have already be
#'     cropped to the country outline
#'
#' @return Integer Sum of radiances of all pixels in a raster that fall within a polygon region
#'
#' @examples
#' #read the Kenya polygon downloaded from GADM and load the lowest admin level (ward)
#' ctryPoly <- readOGR(getPolyFnamePath("KEN"), getCtryShpLowestLyrName("KEN"))
#'     
#' # the VIIRS nightlight raster cropped earlier to the country outline
#' ctryRastCropped <- getCtryRasterOutputFname("KEN","201401")
#' 
#' #calculate the sum of radiances for the wards in Kenya
#' sumAvgRadRast <- fnSumAvgRadRast(ctryPoly, ctryRastCropped)
#' 
#' @importFrom doParallel dopar
fnSumAvgRadRast <- function(ctryPoly, ctryRastCropped, stats)
{
  if(missing(ctryPoly))
    stop("Missing required parameter ctryPoly")
  
  if(missing(ctryRastCropped))
    stop("Missing required parameter ctryRastCropped")
  
  if ((class(ctryPoly) != "SpatialPolygons" && class(ctryPoly) != "SpatialPolygonsDataFrame" ) || is.null(ctryPoly))
    stop("Invalid ctryPoly type: ", class(ctryPoly))
  
  if (class(ctryRastCropped) != "RasterLayer" || is.null(ctryRastCropped))
    stop("Invalid ctryRastCropped type: ", class(ctryRastCropped))
  
  if(!allValid(stats, validStat))
    stop("Invalid stat(s) detected")
  
  doParallel::registerDoParallel(cores=pkg_options("numCores"))

  sumAvgRad <- foreach::foreach(i=1:nrow(ctryPoly@data), .combine=rbind) %dopar% {
    
    message("Extracting data from polygon " , i, " ", base::date())

    dat <- masqVIIRS(ctryPoly, ctryRastCropped, i)

    message("Calculating the NL stats of polygon ", i, " ", base::date())

    #calculate and return the mean of all the pixels
    #data.frame(sum = sum(dat, na.rm=TRUE))
    
    sumAvgRad <- data.frame(sapply(stats, FUN=function(stat) data.frame(stat = eval(parse(text=paste0(stat, "(dat, na.rm=TRUE)"))))))
    
    setNames(sumAvgRad, stats)
  }

  raster::removeTmpFiles(h=0)

  gc()

  return(sumAvgRad)
}

######################## getRastPercentiles ###################################

getRastPercentiles <- function(rastFilename)
{
  rastInfo <- gdalUtils::gdalinfo(rastFilename, hist=T)
  rastMinMax <- unlist(strsplit(gsub("[^[:digit:].-]", " ", rastInfo[30]), " "))
  rastMinMax <- as.numeric(rastMinMax[which(rastMinMax!="")])

  rastSeq <- seq(rastMinMax[2],rastMinMax[3],length.out = rastMinMax[1])

  rastHist <- as.numeric(unlist(strsplit(rastInfo[31]," ")))
  rastHist <- rastHist[!is.na(rastHist)]

  rastCumSum <- cumsum(rastHist)
  rastCumProbs <- rastCumSum/last(rastCumSum)

  q2 <- last(rastSeq[rastCumProbs<0.2])
  q3 <- first(rastSeq[rastCumProbs>0.2])
  hist1 <- rastHist[]

  q98 <- data.table::last(rastSeq[rastCumProbs<0.98])
  q99 <- data.table::first(rastSeq[rastCumProbs>0.98])

  return (c(q2,q98))
}

######################## writeNightlightsMap ###################################

writeNightlightsMap <- function()
{
  tplHead <- "
MAP
  IMAGETYPE  PNG
  EXTENT	-180 -90 180 90
  SIZE		800 600
  SHAPEPATH      /var/www/cgi-bin
  IMAGECOLOR     \"#ffffffff\"
  TRANSPARENT 	TRUE
  SHAPEPATH	/btrfs/shiny_nightlights/outputrasters

  PROJECTION
    \"init=epsg:4326\"
  END

  OUTPUTFORMAT
    NAME GEOTIFF
    DRIVER GDAL/GTiff
    MIMETYPE image/tiff
    IMAGEMODE RGB
    EXTENSION tif
  END

  WEB
    METADATA
      \"wms_title\" \"Nightlight Rasters\"
      \"wms_onlineresource\" \"http://localhost/cgi-bin/mapserv?map=nightlights_wms.map\"
      \"wms_description\" \"nightlights\"
      \"wms_name\" \"Nightlights\"
      \"wms_label\" \"Nightlights\"
      \"wms_srs\" \"EPSG:3857\"
      \"wms_extent\" \"-180 -90 180 90\"
      \"wms_formats\" \"GEOTIFF\"
      \"wms_enable_request\" \"*\"
    END
  END"

  tplLayer <- "
  LAYER
    NAME	<VAL_NAME>

    METADATA
      \"wms_title\"		\"<VAL_NAME>\"
      \"wms_enable_request\"	\"*\"
      \"wms_srs\"			\"EPSG:4326\"
      \"wms_extent\"		\"<VAL_EXTENT>\"
      \"wms_include_items\"	\"all\"
      \"wms_dataurl_format\"	\"text/html\"
    END

    EXTENT	<VAL_EXTENT>
    DATA	\"<VAL_NAME>.tif\"
    #    TILEINDEX	/btrfs/shiny_nightlights/outputrasters/nightlights_201204.tif
    #    TILEITEM	\"location\"
    STATUS	OFF
    TYPE	RASTER

    DUMP TRUE
    PROJECTION
      \"init=epsg:4326\"
    END

    CLASSITEM \"[pixel]\"

    CLASS
      NAME \"NODATA\"
      EXPRESSION ([pixel] = <VAL_NODATA>)

      STYLE
      OPACITY 0
      END
    END

#     CLASS
#       NAME DEC0
#       EXPRESSION ([pixel] > <VAL_NODATA> AND [pixel] < <VAL_DEC0>)
#
#       STYLE
#       OPACITY 100
#       COLOR \"#000000\"
#       END
#     END
#
#     CLASS
#       NAME DEC1
#       EXPRESSION ([pixel] >= <VAL_DEC0> AND [pixel] < <VAL_DEC1> )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#5A5A5A\"
#       END
#     END
#
#     CLASS
#       NAME DEC2
#       EXPRESSION ([pixel] >= <VAL_DEC1>  AND [pixel] < <VAL_DEC2>  )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#7B7B7B\"
#       END
#     END
#
#     CLASS
#       NAME DEC3
#       EXPRESSION ([pixel] >= <VAL_DEC2> AND [pixel] < <VAL_DEC3> )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#949494\"
#       END
#     END
#
#     CLASS
#       NAME DEC4
#       EXPRESSION ([pixel] >= <VAL_DEC3> AND [pixel] < <VAL_DEC4> )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#A8A8A8\"
#       END
#     END
#
#     CLASS
#       NAME DEC5
#       EXPRESSION ([pixel] >= <VAL_DEC4> AND [pixel] < <VAL_DEC5> )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#BABABA\"
#       END
#     END
#
#     CLASS
#       NAME DEC6
#       EXPRESSION ([pixel] >= <VAL_DEC5> AND [pixel] < <VAL_DEC6> )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#CACACA\"
#       END
#     END
#
#     CLASS
#       NAME DEC7
#       EXPRESSION ([pixel] >= <VAL_DEC6> AND [pixel] < <VAL_DEC7> )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#D9D9D9\"
#       END
#     END
#
#     CLASS
#       NAME DEC8
#       EXPRESSION ([pixel] >= <VAL_DEC7> AND [pixel] < <VAL_DEC8>)
#
#       STYLE
#       OPACITY 100
#       COLOR \"#E6E6E6\"
#       END
#     END
#
#     CLASS
#       NAME DEC9
#       EXPRESSION ([pixel] >= <VAL_DEC9> AND [pixel] < <VAL_DEC10> )
#
#       STYLE
#       OPACITY 100
#       COLOR \"#F3F3F3\"
#       END
#     END
#
#     CLASS
#       NAME DEC10
#       EXPRESSION ([pixel] > <VAL_DEC10>)
#
#       STYLE
#       OPACITY 100
#       COLOR \"#FFFFFF\"
#       END
#     END
  END # MODIS raster layer ends here"

  fList <- dir(path = dirRasterOutput, pattern = "*.tif$",full.names = T)

  layers <- NULL
  nodata <- "-1.69999999999999994e+308"

  tplMap <- tplHead
  tplLayers <- ""

  for (f in fList)
  {
    tplLyr <- tplLayer

    message(f)
    rast <- raster::raster(f)
    e <- raster::extent(rast)
    ext <- paste0(e@xmin, " ", e@ymin, " ", e@xmax, " ", e@ymax )
    qs <- myquantile(rast)
    q2 <- qs[1]
    q98 <- qs[2]

    if (is.na(q2))
      q2 <- 0

    if (is.na(q98))
      q98 <- 0

    deciles <- seq(q2, q98, length.out = 11)

    fname <- unlist(stringr::str_split(f, "/"))
    fname <- data.table::last(fname)
    lyrName <- substr(fname,1,10)

    tplLyr <- stringr::str_replace_all(tplLyr,  "<VAL_NAME>", lyrName)
    tplLyr <- stringr::str_replace_all(tplLyr,  "<VAL_NODATA>", nodata)
    tplLyr <- stringr::str_replace_all(tplLyr,  "<VAL_EXTENT>", ext)

    for (i in 1:11)
    {
      decval <- paste0("<VAL_DEC", i-1,">")
      tplLyr <- stringr::str_replace_all(tplLyr,  decval, deciles[i])
    }

    tplLayers <- paste0(tplLayers, "\n", tplLyr)
  }

  tplMap <- paste0(tplMap, "\n", tplLayers, "\nEND #END MAP")

  readr::write_file(tplMap, "test.map")
}

######################## myquantile ###################################

myquantile <- function (x)
{
  #http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  vals <- NULL

  zones <- NULL

  blocks <- raster::blockSize(x)

  result <- NULL

  for (i in 1:blocks$n)
  {
    vals <- raster::getValues(x, blocks$row[i], blocks$nrows[i])

    result <- rbind(result, quantile(vals, c(0.02,0.98),na.rm=T))
  }

  result <- colMeans(result, na.rm = T)

  gc()

  return(result)
}

######################## runApplication ###################################

#' Run a web application to explore the processed nightlight data cached locally
#'
#' Run a web application to perform some exploratory data analysis. The application written in shiny either loads demo data or the data processed and cached locally.
#'
#' @return None
#'
#' @examples
#' runApplication()
#'
#' @export
exploreData <- function()
{
  appDir <- system.file("application", package = "Rnightlights")
  if (appDir == "")
  {
    stop("Could not find application directory. Try re-installing the `Rnightlights` package.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

######################## setupDataPath ###################################

#' Interactively offers the user to set up the default root path
#'
#' Interactively offers the user to set up the default root path
#'
#' @param defaultPath Default root path to set
#' 
#' @param ... Not used.
#'
#' @return character string Returns (invisibly) the root path,
#'     or @NULL if running a non-interactive session.
#' 
#' @seealso Internally, @see "setDataPath" is used to set the root path.
#'     The \code{"base::interactive"} function is used to test whether 
#'     \code{R} is running interactively or not.
#'
#' @export
setupDataPath <- function(newDataPath="~", ...)
{
  dirName <- ".Rnightlights"
  
  defaultPath <- file.path("~")

  dataPath <- getDataPath()
  
  if (missing(newDataPath))
  {
    if (is.null(dataPath))
    {
      #if we don't find a data path and none is supplied ask the user where to create it
      #both null; ask user
      if (interactive())
      {
        prompt <- "The Rnightlights package needs to create a directory that will hold package files and data which may be large. Please choose a location with 3GB+ where this directory will be created to avoid running out of space. If none is chosen the home directory will be used. 
\nWould you like to choose a different data directory? 
\nEnter 0 to Exit"
        

          ans <- menu(choices = c(paste0("Create data path under home directory '",  
                                         path.expand("~")), 
                                  "Choose a different directory as the data path"),
                      graphics = F, title = prompt);
          
          if (ans == 1)
            dataPath <- defaultPath
          else if (ans == 2)
            dataPath <- tryCatch(
            {
              path <- tcltk::tk_choose.dir(getDataPath())
            }, 
            error=function(ex) 
            {
              path <- readline("Please enter the directory path: ")
              if (dir.exists(path))
                return (path)
              else
                return(NULL)
            })
          else if (ans == 0)
          {
            message("Exiting. dataPath not set: Re-run to set/change dataPath")
            return(invisible(getDataPath()))
          }

        if (is.null(dataPath) || is.na(dataPath))
        {
          message("Exiting. dataPath not set: Re-run to set/change dataPath")
          return(invisible(getDataPath()))
        }
      }
      else
      {
        dataPath <- defaultPath

        message("Creating data folder in default location ", path.expand(file.path(dataPath, dirName)))
      }
      
      setDataPath(dataPath)
      #invisible(dataPath)
    }
    else
    {
      #if a directory currently exists ask the user if they want to change it
      if (interactive())
      {
        prompt <- paste0("The Rnightlights package needs to create a directory that will hold package files and data which may be large. Please choose a location with 3GB+ where this directory will be created to avoid running out of space. \nCurrently the data directory is set to '", path.expand(getDataPath()), "' \nWould you like to choose a different data directory?
\nEnter 0 to Exit")
        
        
        ans <- menu(choices = c(paste0("Use current directory '",  
                                       path.expand(getDataPath()), " as the data path"), 
                                "Choose a different directory as the data path"),
                    graphics = F, title = prompt);
        
        if (ans == 1)
          return(invisible(getDataPath()))
        else if (ans == 2)
        {
          dataPath <- tryCatch(
            {
              path <- tcltk::tk_choose.dir("~/")
            }, 
            error=function(ex) 
            {
              path <- readline("Please enter the directory path: ")
              if (dir.exists(path))
                return (path)
              else
                stop(path, " not found")
            })
          
          #this is a move
          setDataPath(dataPath)
        }
        else if (ans == 0)
        {
          message("Exiting. dataPath not set: Re-run to set/change dataPath")
          return(getDataPath())
        }
        
        #is.na if dialog cancelled, is.null if readline empty
        if (is.null(dataPath) || is.na(dataPath)) 
        {
          message("Exiting. dataPath not set: Re-run to set/change dataPath")
          return(invisible(getDataPath()))
        }
      }
      else
      {
        dataPath <- getDataPath()
        if(!is.null(dataPath))
          message("Using previous install detected at ", path.expand(file.path(dataPath, dirName)))
      }
      
      setDataPath(dataPath)
    }
  }
  else
  {
    if(is.null(dataPath))
    {
      #create dir in newDataPath
      message("Creating default directory")
      setDataPath("~")
      
      message("Creating data directory")
      setDataPath(newDataPath)
    }
    else
    {
      message("Attempting data directory move")
      setDataPath(newDataPath)
    
      #invisible(newDataPath)
    }   
  }
  
  dataPath <- getDataPath()

  return(invisible(dataPath))
} # setupCacheDataPath()

######################## setDataPath ###################################
#' Sets the root path to the package data directory
#'
#' By default, this function will set the root path to \code{~/.Rnightlights/}.
#'
#' @param path The path
#'
#' @param ... Not used
#'
#' @return
#'   Returns (invisibly) the old root path.
#'   
#' @examples
#'   
#' @export
setDataPath <- function(dataPath, newDataPath)
{
  if(missing(dataPath))
    stop("Missing required parameter dataPath")
  
  if (!is.character(dataPath) || is.null(dataPath) || is.na(dataPath) || dataPath == "")
    stop("dataPath must be a valid character string")
  
  dataPath <- as.character(dataPath)
  
  dataDirName <- ".Rnightlights"
  
  homePath <- file.path("~", ".Rnightlights")
  
  existingPath <- getDataPath()
  
  #if existingPath is not null we already have an existing directory. This is potentially a move
  if (!is.null(existingPath))
  {
    #if the supplied directory is the same as the current dataPath stop. Nothing to do
    if(path.expand(dataPath) == path.expand(existingPath))
    {
      message("The directories are the same. Not changing")
      return(invisible(dataPath)) #return user version. less expensive
    }
    else #if they are different we will move
    {
      isMove <- TRUE
    }
  }
  else #is a new install
  { 
    isMove <- FALSE
  }

  #create the dataPath
  if(dir.exists(dataPath))
  {
    dirCreate <- file.path(dataPath, dataDirName)
    
    if(!dir.create(dirCreate))
      stop("Unable to create directory ", dirCreate)
    else
    {
      message("Data directory created ", path.expand(dirCreate))
      message("Rnightlights may require 3GB+. Run setupDataPath() to change the location")
    }
  }
  else
    stop("Directory ", dataPath, " not found")
  
  #If we are here we have created a new directory
  #Make sure the homePath exists and persist the dataPath
  #~/.Rnightlights Must always exist even if it does not hold the data
  if(!exists(file.path(homePath)))
    if(dir.exists(file.path(homePath)) || dir.create(file.path(homePath)))
      saveRDS(path.expand(dataPath), file.path(homePath, "datapath.rda"))

  #if this is a move
  if(isMove)
  {
    message("Moving dataPath from ", dataPath, " to ", newDataPath)
    
    #copy the .Rnightlights folder to newDataPath
    if(file.copy(file.path(dataPath, dataDirName), file.path(newDataPath), recursive = TRUE))
    {
      #persist the changed data path
      saveRDS(path.expand(newDataPath), file.path(homePath, "datapath.rda"))
      
      #if the old directory was the default dir in the home dir then do not attempt to delete old directory
      if(path.expand(dataPath) == path.expand("~"))
      {
        message("Move of datapath from ", dataPath, " to ", newDataPath, " complete.")
      }
      else #else mark the dir for deletion and prompt user to delete it
      {
        #unlink(dataPath, recursive = T, force = T)
        delText <- "This is an old Rnightlights package data directory. It is safe to delete this directory."
        readr::write_file(delText, file.path(dataPath, dataDirName, "_RNIGHTLIGHTS_SAFE_TO_DELETE"))
        
        #if the new location was an old data dir the _RNIGHTLIGHTS_SAFE_TO_DELETE file might still be present. Delete it.
        if(file.exists(file.path(newDataPath, "_RNIGHTLIGHTS_SAFE_TO_DELETE")))
          file.remove(file.path(newDataPath, "_RNIGHTLIGHTS_SAFE_TO_DELETE"))
        
        #remove the copied datapath.rda if it exists. Usually if the datapath is moving from the default location i.e. home dir
        if(newDataPath != "~")
          file.remove(file.path(dataPath, ".Rnightlights/datapath.rda"))
        
        message("Move of datapath from ", dataPath, " to ", newDataPath, " complete.")
        message("You may now delete ", file.path(dataPath, dataDirName))
      }
    }
  }

  # Add a README.txt file, if missing.
  .addREADME(to=file.path(dataPath, dataDirName))

  if(!is.null(dataPath))
    createNlDataDirs()
  
  invisible(getDataPath())
} # setdataPath()

######################## getDataPath ###################################

#' Gets the root path to the file directory"
#'
#' Gets the root path to the file directory"
#'
#' @param defaultPath The default path, if no user-specified directory
#'     has been given.
#'     
#' @param ... Not used.
#'
#' @return Returns the path as a @character string.
#'
#' @examples
#'   print(getDataPath())
#'
#' @seealso To set the directory where package data files are stored,
#'     see @see "setDataPath".
#'     
#' @export
getDataPath <- function()
{

  homePath = "~"
  dirName = ".Rnightlights"
  dataPathFile = "datapath.rda"
  
  if (dir.exists(file.path(homePath, dirName)))
  {
    if (file.exists(file.path(homePath, dirName, dataPathFile)))
    {
      dataPath <- readRDS(file = file.path(homePath, dirName, dataPathFile))
      
      if (!dir.exists(dataPath))
      {
        dataPath <- homePath
        
        RnightlightsDataPath <- homePath
        saveRDS(path.expand(dataPath), file.path(homePath, "datapath.rda"))
      }
    }
    else
      dataPath <- homePath
  }
  else
  {
    dataPath <- NULL
  }
  
  dataPath
}

removeDataPath <- function()
{
  #dataPath <- pkg_options("dataPath")
  
  menuPrompt <- paste0("You are about to remove the Rnightlights data folder in \n, dataPath. Do you want to continue?")
  
  #warning(menuPrompt)
  
  response <- menu(choices = c("Yes", "no"), graphics = F, title = menuPrompt)
  
  if(response == "1")
  {
    #file.remove(dataPath)
    message("Removed dataPath")
  }
  else if(response == "2")
    message("Not deleting")
  else if(response == "0")
    message("Aborted")
}

.addREADME <- function(to=getdataPath(), ...)
{
  # Add a README.txt to dataPath (expaining what the directory is)
  filename <- "README.txt"
  
  pathnameD <- file.path(to, filename)
  
  if (!file.exists(pathnameD))
  {
    pathnameS <- system.file("_Rnightlights", package="Rnightlights")
    
    file.copy(pathnameS, pathnameD)
  }
} # .addREADME()

createNlDataDirs <- function()
{
  #set directory paths (tiles, ctrypoly, output/cropped rasters, downloads/temp?)
  
  #create directories
  if(!dir.exists(getNlDir("dirPolygon")))
    dir.create(getNlDir("dirPolygon"))
  
  if(!dir.exists(getNlDir("dirRasterOLS")))
    dir.create(getNlDir("dirRasterOLS"))
  
  if(!dir.exists(getNlDir("dirRasterVIIRS")))
    dir.create(getNlDir("dirRasterVIIRS"))
  
  if(!dir.exists(getNlDir("dirNlData")))
    dir.create(getNlDir("dirNlData"))
  
  if(!dir.exists(getNlDir("dirRasterOutput")))
    dir.create(getNlDir("dirRasterOutput"))
  
  if(!dir.exists(getNlDir("dirRasterWeb")))
    dir.create(getNlDir("dirRasterWeb"))
  
  if(!dir.exists(getNlDir("dirZonals")))
    dir.create(getNlDir("dirZonals"))
}