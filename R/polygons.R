######################## getAllGadmVersions #############################

#' Return a vector of GADM versions
#'
#' Return a vector of GADM versions
#' 
#' @return character vector valid GADM versions
#'
#' @examples
#' \dontrun{
#' Rnightlights:::getAllGadmVersions()
#' }
#'
getAllGadmVersions <- function()
{
  return(c("2.8", "3.6"))
}

######################## validGadmVersions #############################

#' Check whether GADM versions are valid
#'
#' Check whether GADM versions are valid
#' 
#' @param gadmVersions The GADM versions to validate
#' 
#' @return logical vector
#'
#' @examples
#' \dontrun{
#' Rnightlights:::validGadmVersions("2.8")
#' }
#'
validGadmVersions <- function(gadmVersions)
{
  return(stats::setNames(gadmVersions %in% getAllGadmVersions(), gadmVersions))
}

getGadmLayerNames <- function(ctryCode, layerNum, gadmVersion)
{
  
}

getGadmLevelNames <- function(ctryCode, layerNum)
{
  
}


gadmLayerToAlias <- function(layerNames, gadmVersion=pkgOptions("gadmVersion"))
{
  admLevels <- sapply(layerNames, function(layerName)
  {
    ctryCode <- toupper(gsub("_","", stringr::str_extract(tolower(layerName), "_*[a-z]{3}_+")))
    levelNum <- stringr::str_extract(layerName, "\\d+$")
    paste0(ctryCode, "_adm", levelNum)
  })
  
  return(admLevels)
}

gadmAliasToLayer <- function(layerAlias, gadmVersion=pkgOptions("gadmVersion"))
{
  
}

######################## orderCustPolyLayers ###################################

#' Order polygon shapefile layers in custom polygons
#' 
#' Order polygon shapefile layers in custom polygons
#'
#' Add an index column to all layers of a polygon
#' 
#' @param ctryCode The ISO3 ctryCode of the country polygon to download
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' Rnightlights:::addCtryPolyIdx(ctryCode="KEN")
#' }
#'
orderCustPolyLayers <- function(ctryCode, custPolyPath=NULL)
{
  if(is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter custPolyPath")
  
  if(!is.null(ctryCode) && !validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ISO3 ctryCode: ", ctryCode)
  
  polyFnamePath <- getPolyFnamePath(ctryCode = ctryCode, custPolyPath = custPolyPath)
  
  lyrNames <- rgdal::ogrListLayers(polyFnamePath)
  
  ctryPolyNRows <- sapply(lyrNames, function(lyrName)
  {
    ctryPoly <- rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, custPolyPath = custPolyPath), layer = lyrName)
    
    return(nrow(ctryPoly@data))
  })

  lyrOrders <- order(ctryPolyNRows)
  
  for(lyrIdx in 1:length(lyrNames))
  {
    lyrFiles <- list.files(path = polyFnamePath, pattern = lyrNames[lyrIdx], full.names = TRUE)
    
    for(lyrFile in lyrFiles)
      file.rename(from = lyrFile, to = file.path(dirname(lyrFile), paste0(lyrOrders[lyrIdx]-1, "_", basename(lyrFile))))
  }
}

######################## addCtryPolyIdx ###################################

#' Add an index column to all layers of a polygon
#'
#' Add an index column to all layers of a polygon
#' 
#' @param ctryCode The ISO3 ctryCode of the country polygon to download
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' Rnightlights:::addCtryPolyIdx(ctryCode="KEN")
#' }
#'
addCtryPolyIdx <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  if(!is.null(custPolyPath))
  {
    for(admLevel in rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
    {
      ctryPoly <- rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), layer = admLevel)
      
      #Create an integer col in the shapefile with unique GIDs
        message(Sys.time(), ": Creating integer zone attribute col for polygon")
        
        lowestIDCol <- "GID_IDX"
        
        ctryPoly@data[,lowestIDCol] <- 1:nrow(ctryPoly@data)  # Make new attribute
      
        #project to wgs84
        ctryPoly <- sp::spTransform(ctryPoly, sp::CRS(wgs84))
          
        message(Sys.time(), ": Writing layer with new idx col")
        rgdal::writeOGR(obj = ctryPoly,
                        dsn = getPolyFnamePath(ctryCode = ctryCode,
                                               gadmVersion = gadmVersion,
                                               custPolyPath = custPolyPath),
                        layer = admLevel,
                        driver = "ESRI Shapefile",
                        overwrite_layer = T) # Save new version of shapefile
    }
  } else if(gadmVersion == "3.6")
  {
    for(admLevel in sort(rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))))
    {
      ctryPoly <- rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), layer = admLevel)
      
      lowestIDCol <- paste0("GID_", stringr::str_extract(admLevel, "\\d+$"), "_IDX")
      
      #for GADM 3.6 polygons the attribute col GID_3 contains
      #strings which cannot be used by gdal_rasterize
      #Create an integer col in the shapefile corresponding to the unique GIDs
      if(is.null(ctryPoly@data[[lowestIDCol]]))
      {
        message(Sys.time(), ": Creating integer zone attribute col for polygon")
        
        lowestIDColOrig <- gsub("_IDX", "", lowestIDCol)
        
        ctryPoly@data[,lowestIDCol] <- 1:length(sort(ctryPoly@data[,lowestIDColOrig]))  # Make new attribute
        
        message(Sys.time(), ": Writing layer with new idx col")
        rgdal::writeOGR(obj = ctryPoly,
                        dsn = getPolyFnamePath(ctryCode = ctryCode,
                                               gadmVersion = gadmVersion,
                                               custPolyPath = custPolyPath),
                        layer = admLevel,
                        driver = "ESRI Shapefile",
                        overwrite_layer = T) # Save new version of shapefile
      }
    }
  }
}

######################## dnldCtryPoly ###################################

#' Download a country's polygon shapefile from \url{http://gadm.org}
#'
#' Download a country's polygon shapefile from \url{http://gadm.org}
#' 
#' @param ctryCode The ISO3 ctryCode of the country polygon to download
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @param downloadMethod The method used to download polygons
#'
#' @return TRUE/FALSE Success/Failure of the download
#'
#' @examples
#' \dontrun{
#' Rnightlights:::dnldCtryPoly("KEN")
#' }
#'
dnldCtryPoly <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL, downloadMethod=pkgOptions("downloadMethod"))
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter. One of ctryCode/custPolyPath required")
  
  if(!is.null(ctryCode) && !validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ISO3 ctryCode: ", ctryCode)

  message(Sys.time(), ": Downloading ctry poly: ", ctryCode)
  
  fullPolyUrl <- getCtryPolyUrl(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  result <- NULL
  
  #if the path doesn't exist
  if (!existsPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  {
    #if the dir and zip dont exist download and unzip
    if (!existsPolyFnameZip(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    {
      #do not use aria. Seems GADM has issues with it. Use auto
      downloadMethod <- "auto"

      message(Sys.time(), ": Downloading ", fullPolyUrl)
      
      if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
        rsltDnld <- utils::download.file(url = fullPolyUrl,
                                         destfile = getPolyFnameZip(ctryCode = ctryCode,
                                                                    gadmVersion = gadmVersion,
                                                                    custPolyPath = custPolyPath),
                                         method = "auto",
                                         mode = "wb",
                                         extra = "-c")
      else if (downloadMethod == "aria")
        rsltDnld <- system(paste0("aria2c -c -x2 --show-console-readout=false --summary-interval=10 ", fullPolyUrl,
                                  " -d ", getNlDir("dirPolygon"),
                                  " -o ", basename(getPolyFnameZip(ctryCode = ctryCode,
                                                                   gadmVersion = gadmVersion,
                                                                   custPolyPath = custPolyPath)))) #downloads to path relative to -d if specified else local dir

      if(rsltDnld == 0)
      {
        #unzip does not like double slashes! Replace with singles if found
        
        polyFnameZip <- getPolyFnameZip(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
        polyFnameZip <- gsub("//", "/", polyFnameZip, perl=TRUE)
        polyFnameZip <- gsub("\\\\\\\\", "\\\\", polyFnameZip, perl=TRUE)
        
        polyFnamePath <- getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
        polyFnamePath <- gsub("//", "/", polyFnamePath, perl=TRUE)
        polyFnamePath <- gsub("\\\\\\\\", "\\\\", polyFnamePath, perl=TRUE)
        
        #unzip
        result <- utils::unzip(zipfile = polyFnameZip, junkpaths = TRUE, exdir = polyFnamePath)
        file.remove(polyFnameZip)
        
        if(!is.null(custPolyPath) || gadmVersion == "3.6")
          addCtryPolyIdx(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      }else
      {
        stop(Sys.time(), ": Something went wrong. Polygon download failed!")
      }
    }else
    {
      #if the dir doesn't exist but the zip does unzip the zip
      #unzip does not like double slashes! Replace with singles if found
      
      #Convert double forward slashes to single
      polyFnameZip <- getPolyFnameZip(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      polyFnameZip <- gsub("//", "/", polyFnameZip, perl=TRUE) #forward
      polyFnameZip <- gsub("\\\\\\\\", "\\\\", polyFnameZip, perl=TRUE) #back
      
      polyFnamePath <- getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      polyFnamePath <- gsub("//", "/", polyFnamePath, perl=TRUE)
      polyFnamePath <- gsub("\\\\\\\\", "\\\\", polyFnamePath, perl=TRUE)
      
      #unzip
      result <- utils::unzip(zipfile = polyFnameZip, junkpaths = TRUE, exdir = polyFnamePath)
      file.remove(polyFnameZip)
      
      if(!is.null(custPolyPath) || gadmVersion == "3.6")
        addCtryPolyIdx(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      
    }

    if(!is.null(custPolyPath))
    {
      orderCustPolyLayers(ctryCode = ctryCode, custPolyPath = custPolyPath)
    }
        
    wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    
    #saving RDS
    message(Sys.time(), ": Saving shapefile as RDS for faster access")
    message(Sys.time(), ": Getting admLevels in ", ctryCode)
    if(is.null(custPolyPath))
      allCtryLevels <- sort(unlist(grep("adm", rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)), value = T)))
    else
      allCtryLevels <- rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    
    message(Sys.time(), ": Reading in all admLevels")
    listCtryPolys <- unlist(lapply(allCtryLevels, function(lvl){ ctryPoly <- rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), layer = lvl)}))
    
    message(Sys.time(), ": Saving admLevel polygons as RDS")
    saveRDS(object = listCtryPolys, file = getPolyFnameRDS(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  }
  else
  {
    message(Sys.time(), ": Polygon dir for ", paste(ctryCode, ifelse(is.null(custPolyPath), gadmVersion, basename(custPolyPath)), sep = ":"), " already exists")
    
    if(!file.exists(getPolyFnameRDS(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
    {
      message(Sys.time(), ": Saving shapefile as RDS for faster access")
      message(Sys.time(), ": Getting admLevels in ", ctryCode)
      if(is.null(custPolyPath))
        allCtryLevels <- sort(unlist(grep("adm", rgdal::ogrListLayers(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)), value = T)))
      else
        allCtryLevels <- rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
      
      message(Sys.time(), ": Reading in all admLevels")
      listCtryPolys <- unlist(lapply(allCtryLevels, function(lvl){ ctryPoly <- rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), layer = lvl); ctryPoly <- cleangeo::clgeo_Clean(ctryPoly)}))
      
      message(Sys.time(), ": Saving admLevel polygons as RDS")
      saveRDS(listCtryPolys, getPolyFnameRDS(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    }
  }
  
  #save ctry structure as CSV in data dir
  if(!file.exists(getCtryStructFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
  {
    message(Sys.time(), ": Saving country admLevel structure to CSV")
  
    createCtryStruct(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  }

  return (!is.null(result))
}

######################## getCtryStructFname ###################################

#' Construct the name for the country struct file
#'
#' Construct the name for the country struct file
#' 
#' @param ctryCode The ISO3 ctryCode of the country
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character string The filename
#'
#' @examples
#' Rnightlights:::getCtryStructFname("KEN")
#'
getCtryStructFname <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  fName <- if(is.null(custPolyPath))
              paste0("NL_STRUCT_", ctryCode, "_GADM-", gadmVersion, ".csv")
            else
              paste0("NL_STRUCT_", ifelse(is.null(ctryCode), "", paste0(ctryCode, "_")), "CUST-", basename(custPolyPath), ".csv")
  
  return(fName)
}

######################## getCtryStructFnamePath ###################################

#' Construct the full path to the country struct file
#'
#' Construct the full path to the country struct file
#' 
#' @param ctryCode The ISO3 ctryCode of the country
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character string The file path
#'
#' @examples
#' Rnightlights:::getCtryStructFnamePath("KEN")
#'
getCtryStructFnamePath <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  return(file.path(getNlDir("dirNlData"), getCtryStructFname(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
}

######################## createCtryStruct ###################################

#' Save ctry admin structure to text file for faster access
#'
#' Save ctry admin structure to text file for faster access
#' 
#' @param ctryCode The ISO3 ctryCode of the country
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return None
#'
#' @examples
#' \dontrun{
#'   Rnightlights:::createCtryStruct("KEN")
#' }
#'
createCtryStruct <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  ctryStructFnamePath <- getCtryStructFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(!exists(ctryStructFnamePath))
  {
    ctryNlDataDF <- createCtryNlDataDF(ctryCode = ctryCode, admLevel = getCtryShpLowestLyrNames(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    
    utils::write.table(x = ctryNlDataDF, file = ctryStructFnamePath, row.names = F, sep = ",")
  }
}

######################## readCtryStruct ###################################

#' Reads the ctry admin structure from struct text file
#'
#' Reads the ctry admin structure from struct text file
#' 
#' @param ctryCode The ISO3 ctryCode of the country structure to read
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return None
#'
#' @examples
#' \dontrun{
#' Rnightlights:::createCtryStruct("KEN")
#' }
#'
readCtryStruct <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  nlCtryData <- NULL
  
  ctryStructFnamePath <- getCtryStructFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(file.exists(ctryStructFnamePath))
  {
    nlCtryStruct <- data.table::fread(input = ctryStructFnamePath)
  } else
  {
    createCtryStruct(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    
    nlCtryStruct <- data.table::fread(input = ctryStructFnamePath)
  }
  
  return(nlCtryStruct)
}


######################## getCtryPolyUrl ###################################

#' Get the GADM url from which to download country polygons
#'
#' Get the url from which to download country polygons. Polygons are downloaded from 
#'     \url{http://gadm.org}. This provides the url to the zipped ESRI Shapefile 
#'     which when decompressed contains a directory with the different country admin 
#'     level boundary files. A sample url returned for Afghanistan: 
#'     http://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
#'
#' @param ctryCode character string The ctryCode of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return Character string url of the zipped ESRI shapefile for the ctryCode
#'
#' @examples
#' ctryCode <- "KEN"
#' Rnightlights:::getCtryPolyUrl(ctryCode)
#'  #returns url for the zipped country ESRI shapefile
#'
getCtryPolyUrl <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(is.null(custPolyPath))
  {
    #Sample url: https://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
    ctryPolyUrl <- if(gadmVersion == "2.8")
      paste0("https://biogeo.ucdavis.edu/data/gadm2.8/shp/", ctryCode, "_adm_shp.zip")
    else if(gadmVersion == "3.6")
      paste0("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_", ctryCode, "_shp.zip")
    else
      stop(Sys.time(), ": Invalid gadmVersion")
  }else
  {
    #return custPolyPath as a url taking into account whether it is a relative path
    ctryPolyUrl <- paste0(ifelse(substr(custPolyPath,1,1) == "/", "file://", paste0("file:///", getwd(), "/")), custPolyPath)
  }

  return (ctryPolyUrl)
}

######################## existsPolyFnamePath ###################################

#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' @param ctryCode The ctryCode to process
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsPolyFnamePath("KEN")
#'  #returns TRUE/FALSE
#'
existsPolyFnamePath <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  #for polygons look for shapefile dir
  return(dir.exists(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
}

######################## existsPolyFnameZip ###################################

#' Check if the compressed country polygon has been downloaded and stored in the polygon folder
#'
#' Check if the compressed country polygon has been downloaded and stored in the polygon folder
#'
#' @param ctryCode The ctryCode of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsPolyFnameZip("KEN")
#'  #returns TRUE/FALSE
#'
existsPolyFnameZip <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  # if(missing(ctryCode))
  #   stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  return(file.exists(getPolyFnameZip(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
}

######################## getCtryShpLyrName ###################################

#' Get the standard names of polygon layers in a country
#'
#' Get the standard name of a polygon layer for a country. Used to refer 
#'     to a polygon layer by name i.e. for 
#'     CTRYCODE & lyrNum="0": lyrName="CTRYCODE_adm0", 
#'     lyrNum="1": lyrName="KEN_adm1".
#'     Note this is different from the official country administration
#'     level name.
#'
#' @param ctryCodes the ISO3 codes for the countries
#'
#' @param lyrNums the layer numbers starting from 0 = country level, 
#'     1 = first admin level
#'     
#' @param dnldPoly \code{logical} If the country polygon doesn't exist 
#'     should we download it?
#'
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return Character layer name
#'
#' @examples
#' \dontrun{
#' #requires KEN polygon shapefile to exist in the polygons folder
#' getCtryShpLyrNames("KEN","1")
#'   #returns "KEN_adm1"
#' }
#' 
#' #export only due to exploreData() shiny app
#' @export
getCtryShpLyrNames <- function(ctryCodes=NULL, lyrNums, dnldPoly, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  #if(missing(ctryCodes))
  #  stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(is.null(ctryCodes) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCodes) && !allValidCtryCodes(ctryCodes = ctryCodes))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(dnldPoly))
    dnldPoly <- TRUE
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  if(!existsCtryPoly(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    if(!dnldPoly)
      message(Sys.time(), ": ctryPoly doesn't exist. Set dnldPoly=TRUE to download it")
  else
    dnldCtryPoly(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  admLyrNames <- stats::setNames(lapply(ctryCodes, function(ctryCode)
  {
    layers <- rgdal::ogrListLayers(dsn = path.expand(path = getPolyFnamePath(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
    
    if(is.null(custPolyPath))
    {
      layers <- sort(layers)
      
      if(gadmVersion == "2.8")
        admLayers <- layers[grep(pattern = "adm", x = layers)]
      else if(gadmVersion == "3.6")
        admLayers <- layers[grep(pattern = "adm", x = layers)]
      
      admLayerNums <- sapply(X = lyrNums, FUN = function(lyrNum) grep(paste0(lyrNum,"$"), admLayers))
      
      admLyrName <- admLayers[as.numeric(admLayerNums)]
    }else
    {
      #layers <- gsub("^\\d+_", "", layers)
      
      #layer number to index
      admLyrName <- layers[as.numeric(lyrNums)+1]
    }
  }), ctryCodes)
  
  admLyrNames
}

######################## getCtryShpLowestLyrNames ###################################

#' Get the name of the lowest ctry admin level
#'
#' Get the name of the lowest ctry admin level
#' 
#' @param ctryCodes \code{character} ctryCodes the ctryCodes of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character string The name of the lowest admin level
#'
getCtryShpLowestLyrNames <- function(ctryCodes=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCodes) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCodes) && !allValidCtryCodes(ctryCodes = ctryCodes))
    stop(Sys.time(), ": Invalid ctryCode(s) detected")
  
  if(!dir.exists(path.expand(getPolyFnamePath(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath))))
    dnldCtryPoly(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(!dir.exists(path.expand(getPolyFnamePath(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath))))
    stop(Sys.time(), ": Unable to find/download ctry polygon")
  
  lowestAdmLyrNames <- sapply(ctryCodes,
                              function(ctryCode)
  {
    layers <- as.character(rgdal::ogrListLayers(path.expand(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))))
    
    if(is.null(custPolyPath))
      layers <- sort(layers)
    #else
    #  layers <- gsub("^\\d+_", "", layers)
    
    layers[length(layers)]
  })
  
  return(lowestAdmLyrNames)
}

######################## getCtryPolyAdmLevelNames ###################################

#' Get the list of admin level names in a polygon shapefile
#'
#' Get the list of admin level names in a polygon shapefile. It returns
#'     all official names starting from 1 to the specified 
#'     \code{lowestAdmLevel}. If not \code{lowestAdmLevel} is not
#'     specified, all admin level names are returned
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param lowestAdmLevel \code{integer} The lowest admin level number to return
#'
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
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
getCtryPolyAdmLevelNames <- function(ctryCode=NULL, lowestAdmLevel, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(custPolyPath) && is.null(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(lowestAdmLevel))
    lowestLayer <- getCtryShpLowestLyrNames(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  else
    lowestLayer <- lowestAdmLevel
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  allLayers <- rgdal::ogrListLayers(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  #gadm layers if out of order should be sorted by name. The X=integer in KEN_admX ensures correct order
  if(is.null(custPolyPath))
    allLayers <- sort(allLayers)
  #else
  #  allLayers <- gsub("^\\d+_", "", allLayers)
  
  layerNum <- which(tolower(allLayers) == tolower(lowestLayer))-1
  
  admLevels <- NULL
  
  #get the names of each layer starting from level 1
  if(is.null(custPolyPath))
  {
    for (lyrNum in 0:layerNum)
    {
      lyrPoly <- readCtryPolyAdmLayer(ctryCode = ctryCode, admLevel = as.character(getCtryShpLyrNames(ctryCode, lyrNum, gadmVersion = gadmVersion, custPolyPath = custPolyPath)), gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      
      if(lyrNum > 0)
      {
        lvlTypeName <- paste0("TYPE_",lyrNum)
        
        lvlName <- as.character(unlist(lyrPoly@data[2, eval(lvlTypeName)]))
        
        lvlTypeEngName <- paste0("ENGTYPE_",lyrNum)
        
        lvlEngName <- as.character(unlist(lyrPoly@data[2,eval(lvlTypeEngName)]))
        
        if ((!is.na(lvlName) && !is.na(lvlEngName)) && lvlName != lvlEngName)
          lvlName <- paste0(lvlEngName, "_(", lvlName, ")")
        
        if (is.na(lvlName))
          lvlName <- "Unknown"
      }else
      {
        lvlName <- "country"
      }
      
      admLevels <- c(admLevels, as.character(lvlName))
    }
  }else
  {
    for (lyrNum in 0:layerNum)
    {
      if(lyrNum > 0)
        lvlName <- allLayers[as.numeric(lyrNum)+1]
      else
        lvlName <- allLayers[1]
      
      admLevels <- c(admLevels, as.character(lvlName))
    }
  }
  
  return (admLevels)
}


######################## getCtryStructAdmLevelNames ###################################

#' Get the list of admin level names in a polygon shapefile
#'
#' Get the list of admin level names in a polygon shapefile. It returns
#'     all official names starting from 1 to the specified 
#'     \code{lowestAdmLevel}. If not \code{lowestAdmLevel} is not
#'     specified, all admin level names are returned
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param lowestAdmLevel \code{integer} The lowest admin level number to return
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
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
getCtryStructAdmLevelNames <- function(ctryCode=NULL, lowestAdmLevel, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(custPolyPath) && missing(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ISO3 ctryCode: ", ctryCode)
  
  if(missing(lowestAdmLevel))
    lowestAdmLevel <- getCtryShpLowestLyrNames(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  #why not directy assign?
  lowestLayer <- lowestAdmLevel
  
  allLayers <- rgdal::ogrListLayers(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  #gadm layers if out of order should be sorted by name. The X=integer in KEN_admX ensures correct order
  #for custom polys remove the idx prefix e.g. 1_lyrname
  if(is.null(custPolyPath))
    allLayers <- sort(allLayers)
  #else
  #  allLayers <- gsub("^\\d+_", "", allLayers)
  
  layerNum <- which(allLayers == lowestLayer)
  
  admLevels <- NULL
  
  if(length(layerNum) > 0)
  {
    #get the names of each layer starting from level 1
    if (layerNum > 0)
    {
      lvlNames <- names(readCtryStruct(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
      lvlNames <- lvlNames[-c(length(lvlNames))]
      
      for (lyrNum in 1:(layerNum)) #skip country layer
      {
        admLevels <- c(admLevels, lvlNames[lyrNum])
      }
    }else if (layerNum == 0)
    {
      if(is.null(custPolyPath))
        admLevels <- NULL
      else
        admLevels <- NULL
    } else
    {
      admLevels <- NULL
    }
  } else
  {
    #we are still creating the ctryStruct for a cust poly so use layernames
    if(length(lowestAdmLevel) == 0 || nchar(lowestAdmLevel)==0)
    {
      admLevels <- as.character(allLayers)
    } else
    {
      admLevels <- NULL
    }
  }

  return (admLevels)
}

######################## searchAdmLevel ###################################

#' Search for the admLevel by official name
#'
#' Search for the admLevel by official name. Expects the shapefile
#'     to already exist.
#' 
#' @param ctryCodes \code{character} The ctryCodes of the country of interest
#' 
#' @param admLevelNames \code{character} The names to search for
#' 
#' @param dnldPoly \code{logical} If the country polygon doesn't exist 
#'     should we download it?
#'     
#' @param downloadMethod The method used to download polygons
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character vector of admin level names
#'
#' @examples
#' \dontrun{
#' searchAdmLevel("KEN", "county")
#' #returns "KEN_adm1"
#' }
#'
#' @export
searchAdmLevel <- function(ctryCodes=NULL, admLevelNames, dnldPoly=TRUE, downloadMethod=pkgOptions("downloadMethod"), gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(custPolyPath) && is.null(ctryCodes))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCodes) && !allValidCtryCodes(ctryCodes = ctryCodes))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevelNames))
    admLevelNames <- NULL
  #  stop(Sys.time(), ": Missing required parameter admLevelName")

  #if(length(admLevelName) > 1)
  #  stop(Sys.time(), ": Only 1 admLevel allowed")
  
  if(missing(dnldPoly))
    dnldPoly <- TRUE
  
  #loop over custPolyPaths if present otherwise over ctryCodes
  #since custPolyPaths can have ctryCodes but not vice versa
  numLoops <- ifelse(length(custPolyPath) > 0, length(custPolyPath), length(unlist(ctryCodes)))
  
  if(numLoops < 1)
    return(NULL)
  
  admLevels <- sapply(1:numLoops, function(cCodeIdx)
  {
    if(!existsCtryPoly(ctryCode = ctryCodes[[cCodeIdx]], gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    {
      if(!dnldPoly)
        message(Sys.time(), ": ctryPoly doesn't exist. Set dnldPoly=TRUE to download it")
      else
        dnldCtryPoly(ctryCode = ctryCodes[[cCodeIdx]], gadmVersion = gadmVersion, custPolyPath = custPolyPath, downloadMethod = downloadMethod)
    }
    
    allAdmLevels <- getCtryStructAdmLevelNames(ctryCode = ctryCodes[[cCodeIdx]], gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
    if(is.null(admLevelNames))
      return(stats::setNames(list(allAdmLevels), ctryCodes[[cCodeIdx]]))
    
    ctryAdmLevelNames <- admLevelNames[[cCodeIdx]]
    
    sapply(ctryAdmLevelNames, function(admLevelName)
    {
      ctryShpLyrNames <- if(admLevelName=="country")
        getCtryShpLyrNames(ctryCodes = ctryCodes[[cCodeIdx]], lyrNums = 0, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      else if(admLevelName %in% c("bottom", "lowest"))
        getCtryShpLyrNames(ctryCodes = ctryCodes[[cCodeIdx]], lyrNums = length(allAdmLevels), gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      else if(admLevelName %in% c("top","highest"))
        getCtryShpLyrNames(ctryCodes = ctryCodes[[cCodeIdx]], lyrNums = 1, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      else if(admLevelName == "all")
        getCtryShpAllAdmLvls(ctryCodes = ctryCodes[[cCodeIdx]], gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      
      if(length(ctryShpLyrNames) > 0)
        return(unlist(ctryShpLyrNames))
      
      #if 0 or adm0 take the last digit as the layer num
      if(length(grep("\\d+$", admLevelName, ignore.case = T)) > 0)
        idxFound <- as.numeric(stringr::str_extract(admLevelName, "\\d+$"))
      else
        idxFound <- grep(pattern = admLevelName, x = allAdmLevels, ignore.case = TRUE)-1
      
      if(length(idxFound) == 0)
      {
          return(NA)
      }
      
      ctryShpLyrNames <- getCtryShpLyrNames(ctryCodes = ctryCodes[[cCodeIdx]], lyrNums = idxFound, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      
      return(ctryShpLyrNames)
    })
  })

  lvlNames <- if(!is.null(custPolyPath)) basename(custPolyPath) else ctryCodes
  
  stats::setNames(admLevels, lvlNames)
}

######################## validCtryAdmLvls ###################################

#' Checks if ctry admLevels are valid
#'
#' Checks if ctry admLevels are valid
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param admLevels \code{character} The admLevel(s) to search for
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return \code{logical} whether inputted admLevels are valid
#'
#' @examples
#' \dontrun{
#' validCtryAdmLvls("KEN", "adm0")
#' #returns "KEN_adm1"
#' }
#'
validCtryAdmLvls <- function(ctryCode=NULL, admLevels, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(length(ctryCode) > 1)
    stop(Sys.time(), ": Only one ctryCode can be processed at a time")
  
  ctryAdmLvls <- unlist(getCtryShpAllAdmLvls(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  validAdmLvls <- sapply(admLevels, function(x) toupper(x) %in% toupper(ctryAdmLvls))
  
  if(!all(validAdmLvls))
    message(Sys.time(), ": Invalid admLevels: ", ctryCode, ":", admLevels[!validAdmLvls], " in polygon '", getPolyFname(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), "'")
  
  return(validAdmLvls)
}

######################## allValidCtryAdmLvls ###################################

#' Checks if all ctry admLevels are valid
#'
#' Checks if all ctry admLevels are valid
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param admLevels \code{character} The admLevel(s) to search for
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return \code{logical} whether inputted admLevels are valid
#'
#' @examples
#' \dontrun{
#' validCtryAdmLvls("KEN", "adm0")
#' #returns "KEN_adm1"
#' }
#'
allValidCtryAdmLvls <- function(ctryCode=NULL, admLevels, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  return(all(unlist(sapply(1:length(ctryCode), function(cCodeIdx)validCtryAdmLvls(ctryCode = ctryCode[[cCodeIdx]], admLevels = unlist(admLevels[[cCodeIdx]]), gadmVersion = gadmVersion, custPolyPath = custPolyPath)))))
}

######################## existsCtryPoly ###################################

#' Checks if a country polygon exists
#'
#' Checks if a country polygon exists
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @examples
#' \dontrun{
#' existCtryPoly("KEN")
#' #returns TRUE/FALSE
#' }
#'
existsCtryPoly <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  return(dir.exists(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
}

######################## getCtryShpAllAdmLvls ###################################

#' Get all the admLevels in a country
#'
#' Get all the admLevels in a country
#' 
#' @param ctryCodes \code{character} The ctryCode of the country of interest
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return \code{logical} whether inputted admLevels are valid
#'
#' @examples
#' \dontrun{
#' getCtryShpAllAdmLvls("KEN")
#' #returns "KEN_adm1"
#' }
#'
getCtryShpAllAdmLvls <- function(ctryCodes=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(custPolyPath) && is.null(ctryCodes))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCodes) && !allValidCtryCodes(ctryCodes = ctryCodes))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  stats::setNames(lapply(X = ctryCodes, 
         FUN = function(ctryCode)
         {
           lvl <- ctryShpLyrName2Num(ctryCode = ctryCode, layerName = getCtryShpLowestLyrNames(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), gadmVersion = gadmVersion, custPolyPath = custPolyPath)
           
           adms <- apply(X = cbind(0:lvl,ctryCode),
                         MARGIN = 1,
                         FUN = function(lv) 
                         {
                           adm <- getCtryShpLyrNames(ctryCodes = unlist(lv)[2], lyrNums = unlist(lv)[1], gadmVersion = gadmVersion, custPolyPath = custPolyPath)
                         })
           
           as.character(unlist(adms))
         }), ctryCodes)
}

######################## getPolyFname ###################################

#' Returns the directory name of the unzipped shapefile downloaded from
#'     http://gadm.org without the path
#'
#' Returns the directory name of the unzipped shapefile downloaded from 
#'     \url{http://gadm.org} without the path
#'
#' @param ctryCode character the ISO3 code of the country
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character name of shapefile directory
#'
#' @examples
#' Rnightlights:::getPolyFname("KEN")
#'  #returns "KEN_adm_shp"
#'
getPolyFname <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter. One of ctryCode/custPolyPath required")
  
  if(!is.null(ctryCode) && !validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)

  #if any value is given for custAdmPoly override GADM
  if(!is.null(custPolyPath))
  {
    if(missing(ctryCode))
      ctryCode <- NULL
    
    fName <- basename(custPolyPath)
    
    #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
    polyFname <- paste0("SHP_", ifelse(is.null(ctryCode), "", paste0(ctryCode,"_")), "CUST-", fName)
  }else
  {
    #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
    polyFname <- paste0("SHP_", ctryCode, "_GADM-", gadmVersion)
  }

  return(polyFname)
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
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character path to polygon shapefile directory
#'
#' @examples
#' Rnightlights:::getPolyFnamePath("KEN")
#'  #returns "dataPath/polygons/KEN_adm_shp"
#'  
#' #@export only due to exploreData() shiny app
#' @export
getPolyFnamePath <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter. One of ctryCode/custPolyPath required")
  
  if(!is.null(ctryCode) && !validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)
  
  #check for the shapefile directory created with
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFnamePath <- file.path(getNlDir(dirName = "dirPolygon"), getPolyFname(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  return (polyFnamePath)
}

######################## getPolyFnameZip ###################################

#' Get the filename of the polygon zip file as downloaded from \url{http://GADM.ORG}
#'
#' Get the filename of the polygon zip file as downloaded from \url{http://GADM.ORG}
#'
#' @param ctryCode character the ISO3 code of the country
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character path to zip
#'
#' @examples
#' Rnightlights:::getPolyFnameZip("KEN")
#'  #returns "path/to/"
#'
getPolyFnameZip <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  #format of shapefiles is <ctryCode>_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath),".zip")
  
  return (polyFname)
}

######################## getPolyFnameRDS ###################################

#' Get the filename of the polygon zip file as downloaded from \url{http://GADM.ORG}
#'
#' Get the filename of the polygon zip file as downloaded from \url{http://GADM.ORG}
#'
#' @param ctryCode character the ISO3 code of the country
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return character path to zip
#'
#' @examples
#' Rnightlights:::getPolyFnameZip("KEN")
#'  #returns "path/to/"
#'
getPolyFnameRDS <- function(ctryCode=NULL, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  #format of shapefiles is <ctryCode>_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), ".RDS")
  
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
#' @param ctryCode The ctryCode to process
#' 
#' @param layerName - the name of the polygon layer
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return Integer layer number
#'
#' @examples
#' \dontrun{
#'   Rnightlights:::ctryShpLyrName2Num("KEN", "KEN_adm1") #returns 1
#' }
#'
ctryShpLyrName2Num <- function(ctryCode=NULL, layerName, gadmVersion = pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(layerName))
    stop(Sys.time(), ": Missing required parameter layerName")
  
  if (class(layerName) != "character" || is.null(layerName) || is.na(layerName) || layerName =="")
    stop(Sys.time(), ": Invalid layerName: ", layerName)
  
  allLyrNames <- rgdal::ogrListLayers(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  #if gadm layers are listed out of order sort them
  #custPolyPaths will have been named in order
  if(is.null(custPolyPath))
    allLyrNames <- sort(allLyrNames)

  return(which(allLyrNames == layerName)-1)
}


######################## readCtryPolyAdmLayer ###################################

#' Read a country admLevel polygon
#'
#' Read a country admLevel polygon. Reads the saved RDS format of the shapefile
#'     which is saved as part of \code{dnldCtryPoly} by default. Otherwise,
#'     it will try to read the shapefile. If it fails it returns null.
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param admLevel \code{character} The name to search for
#' 
#' @param polyType \code{character} Whether to read the shapefile or the RDS
#'     format.
#' @param dnldPoly \code{logical} If the country polygon doesn't exist 
#'     should we download it?
#'     
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return \code{SpatialPolygonsDataFrame} The admLevel polygon layer or NULL 
#'     if not found
#'
#' @examples
#' \dontrun{
#' readCtryPolyAdmLayer("KEN", "KEN_adm1")
#' #returns "KEN_adm1"
#' }
#'
#' @export
readCtryPolyAdmLayer <- function(ctryCode=NULL, admLevel, polyType="rds", dnldPoly=TRUE, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(is.null(ctryCode) && is.null(custPolyPath))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(!is.null(ctryCode) && !allValidCtryCodes(ctryCodes = ctryCode))
    stop(Sys.time(), ": Invalid ctryCode(s) detected ")
  
  if(missing(admLevel))
    stop(Sys.time(), ": Missing required parameter admLevelName")
  
  if(!existsCtryPoly(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  {
    if(!dnldPoly)
      message(Sys.time(), ": ctryPoly doesn't exist. Set dnldPoly=TRUE to download it")
    else
      dnldCtryPoly(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  }

  rdsPath <- getPolyFnameRDS(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(polyType=="rds")
  {
    if(!file.exists(rdsPath))
    {
      if(dnldPoly)
        dnldCtryPoly(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      else
        stop(Sys.time(), ": RDS doesn't exist. Set dnldPoly=TRUE to download/create it")
    }
    
    if(file.exists(rdsPath))
    {
      ctryPolys <- readRDS(rdsPath)
      
      layerNum <- ctryShpLyrName2Num(ctryCode = ctryCode, layerName = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      
      ctryPoly <- ctryPolys[[layerNum+1]]
    }
    else
    {
      stop(Sys.time(), ": Unable to retrieve RDS. Retry with dnldPoly=TRUE to download/create it")
    }
    
  }
  else if(polyType == "shp")
  {
    shpPath <- getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
    
    if(!dir.exists(rdsPath))
    {
      if(dnldPoly)
        dnldCtryPoly(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      else
        stop(Sys.time(), ": Shapefile doesn't exist. Set dnldPoly=TRUE to download it")
    }
    
    if(dir.exists(shpPath))
    {
      ctryPoly <- rgdal::readOGR(dsn = shpPath, layer = admLevel)
    }
    else
    {
      stop(Sys.time(), ": Shapefile not found. set dnldPoly=TRUE to download shapefile and save as RDS")
    }
  }
  
  return (ctryPoly)
}