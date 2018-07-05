getAllGadmVersions <- function()
{
  return(c("2.8", "3.6"))
}

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

addCtryPolyIdx <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"))
{
  if(gadmVersion == "3.6")
  {
    for(admLevel in rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
    {
      ctryPoly <- rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), layer = admLevel)
      
      lowestIDCol <- paste0("GID_", stringr::str_extract(admLevel, "\\d+$"), "_IDX")
      
      #for GADM 3.6 polygons the attribute col GID_3 contains
      #strings which cannot be used by gdal_rasterize
      #Create an integer col in the shapefile corresponding to the unique GIDs
      if(is.null(ctryPoly@data[[lowestIDCol]]))
      {
        message("Creating integer zone attribute col for GADM 3.6")
        
        lowestIDColOrig <- gsub("_IDX", "", lowestIDCol)
        
        ctryPoly@data[,lowestIDCol] <- 1:length(sort(ctryPoly@data[,lowestIDColOrig]))  # Make new attribute
        
        message("Writing layer with new idx col")
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
#' @return TRUE/FALSE Success/Failure of the download
#'
#' @examples
#' \dontrun{
#' Rnightlights:::dnldCtryPoly("KEN")
#' }
#'
dnldCtryPoly <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL, downloadMethod=pkgOptions("downloadMethod"))
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  message("Downloading ctry poly: ", ctryCode)
  
  fullPolyUrl <- getCtryPolyUrl(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  result <- NULL
  
  #if the path doesn't exist
  if (!existsPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  {
    #if the dir and zip dont exist download and unzip
    if (!existsPolyFnameZip(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    {
      #download
      
      #downloadMethod <- pkgOptions("downloadMethod")

      if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
        rsltDnld <- utils::download.file(url = fullPolyUrl,
                                         destfile = getPolyFnameZip(ctryCode = ctryCode,
                                                                    gadmVersion = gadmVersion,
                                                                    custPolyPath = custPolyPath),
                                         method = "auto",
                                         mode = "wb",
                                         extra = "-c")
      else if (downloadMethod == "aria")
        rsltDnld <- system(paste0("aria2c -c -x2 ", fullPolyUrl,
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
        
        if(missing(custPolyPath) && gadmVersion == "3.6")
          addCtryPolyIdx(ctryCode = ctryCode, gadmVersion = gadmVersion)
      }else
      {
        stop("Something went wrong. Polygon download failed!")
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
      
      if(missing(custPolyPath) && gadmVersion == "3.6")
        addCtryPolyIdx(ctryCode, gadmVersion)
    }
    
    #saving RDS
    message("Saving shapefile as RDS for faster access")
    message("Getting admLevels in ", ctryCode)
    if(is.null(custPolyPath))
      allCtryLevels <- unlist(grep("adm", rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)), value = T))
    else
      allCtryLevels <- rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    
    message("Reading in all admLevels")
    listCtryPolys <- unlist(lapply(allCtryLevels, function(lvl) rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), layer = lvl)))
    
    message("Saving admLevel polygons as RDS")
    saveRDS(object = listCtryPolys, file = getPolyFnameRDS(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  }
  else
  {
    message("Polygon dir for ", ctryCode, " already exists")
    
    if(!file.exists(getPolyFnameRDS(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
    {
      message("Saving shapefile as RDS for faster access")
      message("Getting admLevels in ", ctryCode)
      if(is.null(custPolyPath))
        allCtryLevels <- sort(unlist(grep("adm", rgdal::ogrListLayers(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)), value = T)))
      else
        allCtryLevels <- rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
      
      message("Reading in all admLevels")
      listCtryPolys <- lapply(allCtryLevels, function(lvl) rgdal::readOGR(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), layer = lvl))
      
      message("Saving admLevel polygons as RDS")
      saveRDS(listCtryPolys, getPolyFnameRDS(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    }
  }
  
  #save ctry structure as CSV in data dir
  if(!file.exists(getCtryStructFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
  {
    message("Saving country admLevel structure to CSV")
  
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
#' @return character string The filename
#'
#' @examples
#' Rnightlights:::getCtryStructFname("KEN")
#'
getCtryStructFname <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  fName <- if(is.null(custPolyPath))
              paste0("NL_STRUCT_", ctryCode, "_GADM-", gadmVersion, ".csv")
            else
              paste0("NL_STRUCT_", ctryCode, "_CUST-", basename(custPolyPath), ".csv")
  
  return(fName)
}

######################## getCtryStructFnamePath ###################################

#' Construct the full path to the country struct file
#'
#' Construct the full path to the country struct file
#' 
#' @param ctryCode The ISO3 ctryCode of the country
#'
#' @return character string The file path
#'
#' @examples
#' Rnightlights:::getCtryStructFnamePath("KEN")
#'
getCtryStructFnamePath <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  return(file.path(getNlDir("dirNlData"), getCtryStructFname(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
}

######################## createCtryStruct ###################################

#' Save ctry admin structure to text file for faster access
#'
#' Save ctry admin structure to text file for faster access
#' 
#' @param ctryCode The ISO3 ctryCode of the country
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'   Rnightlights:::createCtryStruct("KEN")
#' }
#'
createCtryStruct <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  ctryStructFnamePath <- getCtryStructFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(!exists(ctryStructFnamePath))
  {
    ctryNlDataDF <- createCtryNlDataDF(ctryCode = ctryCode, admLevel = getCtryShpLowestLyrNames(ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), custPolyPath = custPolyPath)
    
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
#' @return None
#'
#' @examples
#' \dontrun{
#' Rnightlights:::createCtryStruct("KEN")
#' }
#'
readCtryStruct <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  nlCtryData <- NULL
  
  ctryStructFnamePath <- getCtryStructFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(file.exists(ctryStructFnamePath))
  {
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
#' @return Character string url of the zipped ESRI shapefile for the ctryCode
#'
#' @examples
#' ctryCode <- "KEN"
#' Rnightlights:::getCtryPolyUrl(ctryCode)
#'  #returns url for the zipped country ESRI shapefile
#'
getCtryPolyUrl <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  if(is.null(custPolyPath))
  {
    #Sample url: http://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
    ctryPolyUrl <- if(gadmVersion == "2.8")
      paste0("http://biogeo.ucdavis.edu/data/gadm2.8/shp/", ctryCode, "_adm_shp.zip")
    else if(gadmVersion == "3.6")
      paste0("http://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_", ctryCode, "_shp.zip")
    else
      stop("Invalid gadmVersion")
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
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsPolyFnamePath("KEN")
#'  #returns TRUE/FALSE
#'
existsPolyFnamePath <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
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
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::existsPolyFnameZip("KEN")
#'  #returns TRUE/FALSE
#'
existsPolyFnameZip <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
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
getCtryShpLyrNames <- function(ctryCodes, lyrNums, dnldPoly, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCodes))
    stop("Missing required parameter ctryCode")
  
  if(!allValidCtryCodes(ctryCodes))
    stop("Invalid ctryCode detected")
  
  if(missing(dnldPoly))
    dnldPoly <- TRUE
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  if(!existsCtryPoly(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    if(!dnldPoly)
      message("ctryPoly doesn't exist. Set dnldPoly=TRUE to download it")
  else
    dnldCtryPoly(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  admLyrNames <- stats::setNames(lapply(ctryCodes, function(ctryCode)
  {
    layers <- rgdal::ogrListLayers(dsn = path.expand(path = getPolyFnamePath(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
    
    if(is.null(custPolyPath))
    {
      if(gadmVersion == "2.8")
        admLayers <- layers[grep(pattern = "adm", x = layers)]
      else if(gadmVersion == "3.6")
        admLayers <- layers[grep(pattern = "adm", x = layers)]
      
      admLayerNums <- sapply(X = lyrNums, FUN = function(lyrNum) grep(paste0(lyrNum,"$"), admLayers))
      
      admLyrName <- admLayers[as.numeric(admLayerNums)]
    }else
    {
      admLyrName <- layers[as.numeric(lyrNums)]
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
#' @return character string The name of the lowest admin level
#'
getCtryShpLowestLyrNames <- function(ctryCodes, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCodes))
    stop("Missing required parameter ctryCode")
  
  if(!allValidCtryCodes(ctryCodes = ctryCodes))
    stop("Invalid ctryCode(s) detected ")
  
  if(!dir.exists(path.expand(getPolyFnamePath(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath))))
    dnldCtryPoly(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  if(!dir.exists(path.expand(getPolyFnamePath(ctryCode = ctryCodes, gadmVersion = gadmVersion, custPolyPath = custPolyPath))))
    stop("Unable to find/download ctry polygon")
  
  lowestAdmLyrNames <- sapply(ctryCodes,
                              function(ctryCode)
  {
    layers <- as.character(rgdal::ogrListLayers(path.expand(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))))
    
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
#' @return character vector of admin level names
#'
#' @examples
#' \dontrun{
#' Rnightlights:::getCtryPolyAdmLevelNames("KEN")
#' #returns vector [1] "County"       "Constituency" "Ward"
#' #if KEN shapefile exists otherwise errors
#' }
#'
getCtryPolyAdmLevelNames <- function(ctryCode, lowestAdmLevel=getCtryShpLowestLyrNames(ctryCodes = ctryCode, custPolyPath = custPolyPath), gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ISO3 ctryCode: ", ctryCode)
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  lowestLayer <- lowestAdmLevel
  
  allLayers <- rgdal::ogrListLayers(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  numLayers <- which(allLayers == lowestLayer)
  
  admLevels <- NULL
  
  #get the names of each layer starting from level 1
  if (length(numLayers) > 0 && numLayers > 0)
  {
    for (lyrNum in 1:(numLayers-1)) #skip country layer
    {
      lyrPoly <- readCtryPolyAdmLayer(ctryCode = ctryCode, admLevel = as.character(getCtryShpLyrNames(ctryCode, lyrNum, gadmVersion = gadmVersion, custPolyPath = custPolyPath)), custPolyPath = custPolyPath)
      
      if(is.null(custPolyPath))
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
        lvlName <- allLayers[as.numeric(lyrNum)]
      }
      
      admLevels <- c(admLevels, as.character(lvlName))
    }
  }else
  {
    admLevels <- NULL
  }
  
  #admLevels <- as.data.frame(cbind(1:numLayers, admLevels))
  
  #names(admLevels) <- c("id", "name")
  
  return (admLevels)
}

######################## searchAdmLevel ###################################

#' Search for the admLevel by official name
#'
#' Search for the admLevel by official name. Expects the shapefile
#'     to already exist.
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#' 
#' @param admLevelName \code{character} The name to search for
#' 
#' @param dnldPoly \code{logical} If the country polygon doesn't exist 
#'     should we download it?
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
searchAdmLevel <- function(ctryCode, admLevelName, dnldPoly, downloadMethod=pkgOptions("downloadMethod"), gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ISO3 ctryCode: ", ctryCode)
  
  if(missing(admLevelName))
    stop("Missing required parameter admLevelName")

  if(length(admLevelName) > 1)
    stop("Only 1 admLevel allowed")
  
  if(missing(dnldPoly))
    dnldPoly <- TRUE
  
  if(!existsCtryPoly(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
    if(!dnldPoly)
      message("ctryPoly doesn't exist. Set dnldPoly=TRUE to download it")
    else
      dnldCtryPoly(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath, downloadMethod = downloadMethod)
  
  allAdmLevels <- getCtryPolyAdmLevelNames(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)

  if(admLevelName=="country")
    return(getCtryShpLyrNames(ctryCodes = ctryCode, lyrNums = 0, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  else if(admLevelName %in% c("bottom", "lowest"))
    return(getCtryShpLyrNames(ctryCodes = ctryCode, lyrNums = length(allAdmLevels), gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  else if(admLevelName %in% c("top","highest"))
    return(getCtryShpLyrNames(ctryCodes = ctryCode, lyrNums = 1, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  idxFound <- grep(pattern = admLevelName, x = allAdmLevels, ignore.case = TRUE)
  
  if(length(idxFound) == 0)
  {
      return(NA)
  }

  return (getCtryShpLyrNames(ctryCodes = ctryCode, lyrNums = idxFound, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
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
#' @return \code{logical} whether inputted admLevels are valid
#'
#' @examples
#' \dontrun{
#' validCtryAdmLvls("KEN", "adm0")
#' #returns "KEN_adm1"
#' }
#'
validCtryAdmLvls <- function(ctryCode, admLevels, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(length(ctryCode) > 1)
    stop("Only one ctryCode can be processed at a time")
  
  if(!validCtryCodes(ctryCodes = ctryCode))
    stop("Invalid ISO3 ctryCode: ", ctryCode)
  
  ctryAdmLvls <- unlist(getCtryShpAllAdmLvls(ctryCodes = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
  validAdmLvls <- sapply(admLevels, function(x) toupper(x) %in% toupper(ctryAdmLvls))
  
  if(!all(validAdmLvls))
    message("Invalid admLevels: ", ctryCode, ":", admLevels[!validAdmLvls], " in polygon '", getPolyFname(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath), "'")
  
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
#' @return \code{logical} whether inputted admLevels are valid
#'
#' @examples
#' \dontrun{
#' validCtryAdmLvls("KEN", "adm0")
#' #returns "KEN_adm1"
#' }
#'
allValidCtryAdmLvls <- function(ctryCode, admLevels, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  return(all(validCtryAdmLvls(ctryCode = ctryCode, admLevels = admLevels, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
}

######################## existsCtryPoly ###################################

#' Checks if a country polygon exists
#'
#' Checks if a country polygon exists
#' 
#' @param ctryCode \code{character} The ctryCode of the country of interest
#'
#' @examples
#' \dontrun{
#' existCtryPoly("KEN")
#' #returns TRUE/FALSE
#' }
#'
existsCtryPoly <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ISO3 ctryCode: ", ctryCode)
  
  return(dir.exists(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)))
}

######################## getCtryShpAllAdmLvls ###################################

#' Get all the admLevels in a country
#'
#' Get all the admLevels in a country
#' 
#' @param ctryCodes \code{character} The ctryCode of the country of interest
#'
#' @return \code{logical} whether inputted admLevels are valid
#'
#' @examples
#' \dontrun{
#' getCtryShpAllAdmLvls("KEN")
#' #returns "KEN_adm1"
#' }
#'
getCtryShpAllAdmLvls <- function(ctryCodes, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
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
#' @return character name of shapefile directory
#'
#' @examples
#' Rnightlights:::getPolyFname("KEN")
#'  #returns "KEN_adm_shp"
#'
getPolyFname <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(missing(custPolyPath))
    custPolyPath <- NULL
  
  #if any value is given for custAdmPoly override GADM
  if(!is.null(custPolyPath))
  {
    fName <- basename(custPolyPath)
    
    #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
    polyFname <- paste0("SHP_", ctryCode, "_CUST-", fName)
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
#' @return character path to polygon shapefile directory
#'
#' @examples
#' Rnightlights:::getPolyFnamePath("KEN")
#'  #returns "dataPath/polygons/KEN_adm_shp"
#'  
#' #@export only due to exploreData() shiny app
#' @export
getPolyFnamePath <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
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
#' @return character path to zip
#'
#' @examples
#' Rnightlights:::getPolyFnameZip("KEN")
#'  #returns "path/to/"
#'
getPolyFnameZip <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCodes = ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
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
#' @return character path to zip
#'
#' @examples
#' Rnightlights:::getPolyFnameZip("KEN")
#'  #returns "path/to/"
#'
getPolyFnameRDS <- function(ctryCode, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCodes = ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
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
#' @param layerName - the name of the polygon layer
#'
#' @return Integer layer number
#'
#' @examples
#' Rnightlights:::ctryShpLyrName2Num("KEN_adm1") #returns 1
#'
ctryShpLyrName2Num <- function(ctryCode, layerName, gadmVersion = pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(layerName))
    stop("Missing required parameter layerName")
  
  if (class(layerName) != "character" || is.null(layerName) || is.na(layerName) || layerName =="")
    stop("Invalid layerName: ", layerName)
  
  allLyrNames <- rgdal::ogrListLayers(dsn = getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  
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
readCtryPolyAdmLayer <- function(ctryCode, admLevel, polyType="rds", dnldPoly=TRUE, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ISO3 ctryCode: ", ctryCode)
  
  if(missing(admLevel))
    stop("Missing required parameter admLevelName")
  
  if(!existsCtryPoly(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath))
  {
    if(!dnldPoly)
      message("ctryPoly doesn't exist. Set dnldPoly=TRUE to download it")
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
        stop("RDS doesn't exist. Set dnldPoly=TRUE to download/create it")
    }
    
    if(file.exists(rdsPath))
    {
      ctryPolys <- readRDS(rdsPath)
      
      layerNum <- ctryShpLyrName2Num(ctryCode = ctryCode, layerName = admLevel, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
      
      ctryPoly <- ctryPolys[[layerNum+1]]
    }
    else
    {
      stop("Unable to retrieve RDS. Retry with dnldPoly=TRUE to download/create it")
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
        stop("Shapefile doesn't exist. Set dnldPoly=TRUE to download it")
    }
    
    if(dir.exists(shpPath))
    {
      ctryPoly <- rgdal::readOGR(dsn = shpPath, layer = admLevel)
    }
    else
    {
      stop("Shapefile not found. set dnldPoly=TRUE to download shapefile and save as RDS")
    }
  }
  
  return (ctryPoly)
}