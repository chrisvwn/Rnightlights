######################## getCtryPolyUrl ###################################

#' Get the GADM url from which to download country polygons
#'
#' Get the url from which to download country polygons. Polygons are downloaded from 
#'     \url{http://www.gadm.org}. This provides the url to the zipped ESRI Shapefile 
#'     which when decompressed contains a directory with the different country admin 
#'     level boundary files. A sample url returned for Afghanistan: 
#'     http://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
#'
#' @param ctryCode character string The ctryCode of interest
#'
#' @return Character string url of the zipped ESRI shapefile for the ctryCode
#'
#' @examples
#' ctryCode <- "KEN"
#' Rnightlights:::getCtryPolyUrl(ctryCode)
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

######################## existsPolyFnamePath ###################################

#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' @param ctryCode The ctryCode to process
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsPolyFnamePath("KEN")
#'  #returns TRUE/FALSE
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
#' @param ctryCode The ctryCode of interest
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsPolyFnameZip("KEN")
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
#' lyrName <- getCtryShpLyrName("KEN","0") #top layer name
#'   #returns "KEN_adm0"
#'
#' #@export only due to exploreData() shiny app
#' @export
getCtryShpLyrName <- function(ctryCode, lyrNum)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  return(paste0(ctryCode, "_adm", lyrNum))
}

######################## getCtryShpLowestLyrName ###################################

#' Get the name of the lowest ctry admin level
#'
#' Get the name of the lowest ctry admin level
#' 
#' @param ctryCode the ctryCode of interest
#'
#' @return character string The name of the lowest admin level
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

#' Get the list of admin level names in a polygon shapefile
#'
#' Get the list of admin level names in a polygon shapefile
#'
#' @param ctryCode the ctryCode of the country of interest
#'
#' @return character vector of admin level names
#'
#' @examples
#' \dontrun{
#' Rnightlights:::getCtryPolyAdmLevelNames("KEN")
#' #returns vector [1] "County"       "Constituency" "Ward"
#' #if KEN shapefile exists otherwise errors
#' }
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

######################## getPolyFname ###################################

#' Returns the directory name of the unzipped shapefile downloaded from
#'     GADM.ORG without the path
#'
#' Returns the directory name of the unzipped shapefile downloaded from 
#'     \url{www.GADM.ORG} without the path
#'
#' @param ctryCode character the ISO3 code of the country
#'
#' @return character name of shapefile directory
#'
#' @examples
#' Rnightlights:::getPolyFname("KEN")
#'  #returns "KEN_adm_shp"
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
#' @return character path to polygon shapefile directory
#'
#' @examples
#' Rnightlights:::getPolyFnamePath("KEN")
#'  #returns "dataPath/polygons/KEN_adm_shp"
#'  
#' #@export only due to exploreData() shiny app
#' @export
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
#' Rnightlights:::getPolyFnameZip("KEN")
#'  #returns "path/to/"
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

######################## ctryShpLyrName2Num ###################################

#' Get the integer number of the layer.
#'
#' Get the integer number of the layer. Is the last character in the name 
#'     and is a digit. E.g. for the 3rd layer in Kenya shapefile polygon 
#'     named "KEN_adm3" the layer number is \code{3}. The lowest layer number
#'     is \code{0}
#'
#' @param layerName - the name of the polygon layer
#'
#' @return Integer layer number
#'
#' @examples
#' Rnightlights:::ctryShpLyrName2Num("KEN_adm1") #returns 1
#'
ctryShpLyrName2Num <- function(layerName)
{
  if(missing(layerName))
    stop("Missing required parameter layerName")
  
  if (class(layerName) != "character" || is.null(layerName) || is.na(layerName) || layerName =="")
    stop("Invalid layerName: ", layerName)
  
  return(as.numeric(gsub("[^[:digit:]]", "", layerName)))
}