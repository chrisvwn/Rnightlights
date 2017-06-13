######################## allValid ###################################

#' Check if a vector/list of values given is valid as per the given validation function
#'
#' Check if a vector/list of values given is valid as per the given 
#'     validation function. The function will also print a warning 
#'     showing the values that are invalid. One can stop the warning 
#'     being printed by wrapping the function in the 
#'     \code{suppressWarnings} function.
#'
#' @param testData The list/vector of values to validate
#' 
#' @param testFun The validation function to test each value of testData against
#' 
#' @param ... Other parameters to pass on to the testFun
#'
#' @return TRUE/FALSE
#'
#' @examples
#' 
#' \dontrun{allValid(c("KE", "UGA", "RWA", "TZ"), validCtryCode)}
#'  
#' \dontrun{allValid(c("2012", "2015"), validNlPeriod, "OLS")}
#'
allValid <- function(testData, testFun, ...)
{
  valid <- sapply(testData, function(x) eval(parse(text="testFun(x, ...)")))
  
  invalidData <- testData[!valid]
  
  if(length(invalidData) > 0)
    warning("Invalid data: ", paste0(invalidData, collapse = ", "))
  
  return(all(valid))
}

######################## dnldCtryPoly ###################################

#' Download a country's polygon shapefile from \url{http://gadm.org}
#'
#' Download a country's polygon shapefile from \url{http://gadm.org}
#' 
#' @param ctryCode The ISO3 ctryCode of the country polygon to download
#'
#' @return TRUE/FALSE Success/Failure of the download
#'
#' @examples
#' \dontrun{downloadCtryPoly("KEN")}
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
      if(utils::download.file(url = getCtryPolyUrl(ctryCode), destfile = getPolyFnameZip(ctryCode), method = "auto", mode = "wb", extra = "-c") == 0)
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
#' \dontrun{getAllNlPeriods("OLS")}
#'  #returns a vector of all years from 1994 to present
#'
#' \dontrun{getAllNlPeriods("VIIRS")}
#'  #returns a vector of all yearMonths from 201204 to present
#'
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

######################## nlInit ###################################

#' Initialize some important variables and create directory structure
#'
#' Initialize some important variables and create directory structure
#'
#' @param omitCountries character string/vector CtryCodes to exclude from processing
#' 
#' @return NULL
#'
#' @examples
#'
#'  \dontrun{nlInit()}
#'
nlInit <- function(omitCountries="none")
{

}

######################## nlCleanup ###################################

#' Clean up the environment after processing (Not yet implemented)
#'
#' Clean up the environment after processing (Not yet implemented)
#'
#' @return NULL
#'
#' @examples
#'  \dontrun{nlCleanup()}
#'
nlCleanup <- function()
{
  #remove any global vars we might have used
  #suppressWarnings(rm(map, shpTopLyrName, wgs84, nlTiles, tilesSpPolysDFs))
  
  #the destructor
  
  #del temp files
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
  rastCumProbs <- rastCumSum/data.table::last(rastCumSum)
  
  q2 <- data.table::last(rastSeq[rastCumProbs<0.2])
  q3 <- data.table::first(rastSeq[rastCumProbs>0.2])
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
  
  fList <- dir(path = pkgOptions("dirNlTiles"), pattern = "*.tif$",full.names = T)
  
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
    
    result <- rbind(result, stats::quantile(vals, c(0.02,0.98),na.rm=T))
  }
  
  result <- colMeans(result, na.rm = T)
  
  gc()
  
  return(result)
}