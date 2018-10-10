######################## validNlStats ###################################

#' Check if given statistics are valid
#'
#' Check if given statistics are valid. A valid statistic is one which
#'    is a function available in the current environment and returns
#'    a valid value to match.fun
#'
#' @param nlStats the statistics to check
#'
#' @return named logical TRUE/FALSE
#'
#' @examples
#'
#' Rnightlights:::validNlStats(c("sum", "mean"))
#'  #returns TRUE TRUE
#'  
#' Rnightlights:::validNlStats("unknownFunction")
#'  #returns FALSE
#'
validNlStats <- function(nlStats)
{
  if(missing(nlStats))
    stop(Sys.time(), ": Missing required parameter nlStats")
  
  if((!is.character(nlStats) && !is.list(nlStats)) || is.null(nlStats) || is.na(nlStats) || nlStats == "")
    stop(Sys.time(), ": Invalid nlStats")
  
  if(is.list(nlStats) && length(nlStats) > 1 && all(sapply(2:length(nlStats), function(i) !is.list(nlStats[[i]]) && (grepl("=", nlStats[i]) || length(names(nlStats[i])) > 0))))
    nlStats <- list(nlStats)
  
  matchedFuns <- sapply(nlStats, function(nlStat)
    tryCatch(
    {
      if(is.list(nlStat) && all(sapply(2:length(nlStat), function(i) grepl("=", nlStat[i]) || length(names(nlStat[i])) > 0)))
        matched <- is.character(nlStat[[1]]) && !is.null(match.fun(nlStat[[1]]))
      else
        matched <- is.character(nlStat) && !is.null(match.fun(nlStat))
    }, error = function(err)
    {
      #sep when a=b without a name may cause fn(=param=arg...)
      message(Sys.time(), ": ", paste0("Invalid nlStat: ", gsub("\\(=|,=|funArgs=","\\(", paste0(as.character(nlStat[[1]]), "(", paste(names(nlStat[-1]), nlStat[-1], sep = "=", collapse=",") ,")"))))
      matched <- FALSE
      return(matched)
    }
  ))
  
  return(matchedFuns)
}

######################## myZonal ###################################

#' Calculate zonal statistics. Used internally
#'
#' Calculate zonal statistics. Used internally by zonalpipe. Modified from 
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param rast the country raster
#'
#' @param zone the zonal country polygon layer
#'
#' @param nlStats a character list of statistics to calculate
#'
#' @param digits round off to how many decimals
#' 
#' @param retVal Whether to return the raster data as a vector, or 
#'     data.frame with spatial context NULL returns a vector of all
#'     values, colrowval returns a data.frame with row, col and raster
#'     value while lonlatval returns a data.frame with lon,lat and val.
#'
#' @param na.rm how to handle NAs
#'
#' @param ... Other params to pass to the nlStats functions e.g. na.rm
#'
#' @return numeric value result of the given nlStat function
#' 
#' @import data.table
myZonal <- function (rast, zone, nlStats, digits = 0, retVal=NULL, na.rm = TRUE, ...)
{
  options(fftempdir=getNlDir("dirNlTemp"), fffinalizer="delete")
  
  options(stringsAsFactors = FALSE)
  
  #retVal <- "colrowval"
  
  vals <- NULL
  
  zones <- NULL
  
  #create the text for the functions
  fun <- sapply(nlStats,
                       function(nlStat)
                       {
                         #remove preceding = from "(=" or ",="
                         if(length(nlStat) > 1)
                           nlStatParams <- gsub("(\\(\\s*)=|(,\\s*)=", "\\1\\2", paste0(", ", paste(names(nlStat[-1]),nlStat[-1], sep="=", collapse=",")))
                         else
                           nlStatParams <- NULL
                         
                         nlStat <- nlStat[[1]]
                         
                         #paste0(nlStat,"=", nlStat, "(x, na.rm = TRUE)")
                         nlStatArgs <- formals(nlStat)
                         
                         retVal <- if(all(sapply(c("col","row"), "%in%",names(nlStatArgs))))
                           "colrowval"
                         else if(all(sapply(c("lon","lat"), "%in%",names(nlStatArgs))))
                           "lonlatval"
                         else
                           NULL
                         
                         fnTxt <- if(is.null(retVal))
                           paste0(nlStat, "(vals", nlStatParams, ")")
                         else if(retVal == "colrowval")
                           paste0(nlStat, "(col=cols, row=rows, val=vals", nlStatParams, ")")
                         else if(retVal == "lonlatval")
                           paste0(nlStat, "(lon=lons, lat=lats, val=vals", nlStatParams, ")") 
                        })

  funNames <- gsub("\\(.*\\)", "", fun)
  
  #create the aggregation function
  #funs <- paste0("dta[, as.list(unlist(lapply(.SD, function(dta) list(", paste(fun, collapse=","), ")))), by=zones]")
  funs <- paste0("dta[, as.list(unlist(stats::setNames(lapply(fun, function(fn) eval(parse(text=fn))), funNames))), by=zones]")
  
  message(Sys.time(), ": Reading in raster data")

  #the number of columns in the raster/zone file which are identical in size
  nc <- base::ncol(rast)
  
  #get the block size recommendation
  tr <- raster::blockSize(rast)
  
  #init the progress bar
  pb <- utils::txtProgressBar(min=0, max=tr$n, style=3)
  
  rowVals <- colVals <- lonVals <- latVals <- NULL
  
  #for each block
  for (i in 1:tr$n)
  {
    start <- ((tr$row[i]-1) * nc) + 1
    
    end <- start + (tr$nrows[i] * nc) - 1
    
    rastVals <- raster::getValuesBlock(rast, row=tr$row[i], nrows=tr$nrows[i])
    zoneVals <- raster::getValuesBlock(zone, row=tr$row[i], nrows=tr$nrows[i])

    colrowVals <- stats::setNames(expand.grid(tr$row[i]:(tr$row[i]+tr$nrows[i]-1), 1:rast@ncols), c("cols","rows"))

    extent <- raster::extent(rast)
    lonlatVals <- stats::setNames(expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(rast@ncols-1)),
                          seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(tr$nrows[i]-1))), c("lons","lats"))
    
    #convert negative values to NA 
    rastVals[rastVals < 0] <- NA
    
    #get all pixels in zone 0 which should be
    #the pixels outside the polygon
    idxZone0 <- which(zoneVals == 0)
    
    #remove zone 0 since in large rasters it can be pretty
    #large and may not fit in memory thus causing calculations to fail
    zoneVals <- zoneVals[-idxZone0]
    rastVals <- rastVals[-idxZone0]
    
    colrowVals <- colrowVals[-idxZone0,]
    lonlatVals <- lonlatVals[-idxZone0,]

    #for first block init the ff to the given filename
    if (i == 1)
    {
      vals <- ff::ff(initdata = rastVals, finalizer = "delete", overwrite = T)
      zones <- ff::ff(initdata = zoneVals, finalizer = "delete", overwrite = T)
      
      cols <- ff::ff(initdata = colrowVals$cols, finalizer = "delete", overwrite = T)
      rows <- ff::ff(initdata = colrowVals$rows, finalizer = "delete", overwrite = T)
      lons <- ff::ff(initdata = lonlatVals$lons, finalizer = "delete", overwrite = T)
      lats <- ff::ff(initdata = lonlatVals$lats, finalizer = "delete", overwrite = T)
    } 
    else 
    {
      #otherwise append
      vals <- ffbase::ffappend(vals, rastVals, adjustvmode = T)
      zones <- ffbase::ffappend(zones, zoneVals, adjustvmode = T)

      cols <- ffbase::ffappend(cols, colrowVals$cols, adjustvmode = T)
      rows <- ffbase::ffappend(rows, colrowVals$rows, adjustvmode = T)
      lons <- ffbase::ffappend(lons, lonlatVals$lons, adjustvmode = T)
      lats <- ffbase::ffappend(lats, lonlatVals$lats, adjustvmode = T)
    }
    
    #upddate progress bar
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  #merge the zone and raster ffvectors into an ffdf
  rDT <- ff::ffdf(zones, cols, rows, vals, lons, lats)

  message(Sys.time(), ": Calculating nlStats ")
  
  if(length(unique(rDT$zones)) == 1)
  {
    #no longer required. fixed bug in ffdfdply when zone was length 1
    dta <- data.table::as.data.table(rDT)
    
    result <- eval(parse(text = funs))
  }
  else
  {
  #calculate the nlStats on the ffdf
  #hard coded the batchbytes which is the size of the
  #data to load into memory. Currently set at 1% of an 8GB memory
  #about 80MB. Has to be set for non-Windows systems. Need a better
  #way to figure this out
  result <- ffbase::ffdfdply(x=rDT,
                             split=as.character(zones),
                             trace=TRUE,
                             BATCHBYTES = 80.85*2^20, #try approx 1% of RAM
                             FUN = function(dta){
                               ## This happens in RAM - containing **several** split 
                               #elements so here we can use data.table which works 
                               #fine for in RAM computing
                               dta <- data.table::as.data.table(dta)
                               
                               #calc aggregations
                               result <- eval(parse(text = funs))
                               
                               as.data.frame(result)
                               
                             })
  }
  
  result <- as.data.table(result)
  
  #count cols with nlStat in them
  nlStatColCounts <- sapply(funNames, function(funName) length(grep(paste0("^", funName,"\\.*"), names(result))))
  
  #collapse multi-value cols into one
  for(funName in funNames)
  {
    if(nlStatColCounts[[funName]] > 1)
    {
      message(Sys.time(), ": ", funName, " => Multi-column result detected. Merging")
      #if more than one col detected, get their names
      nlStatCols <- grep(funName, names(result), value = T)
      
      #merge them row-wise into one character vector separated by comma
      result[[funName]] <- apply(result[, nlStatCols, with=F], 1, function(x) paste(x, collapse=","))
      
      #remove the separate cols
      result[,c(nlStatCols) := NULL]
    }else
    {
      nlStatCols <- grep(funName, names(result), value = T)
      
      names(result)[which(names(result)==nlStatCols)] <- funName
    }
  }
  
  resultDF <- as.data.frame(result[,c("zones",funNames), with=F])
  
  ff::delete(rDT, result)
  rm(rDT, result)

  gc()

  return(resultDF)
}

######################## ZonalPipe ###################################

#' Create a zonal file if it does not exist and calculate the zonal stats
#'
#' Create a zonal file if it does not exist and calculate the zonal stats by calling the 
#'     myZonal function. Modified from 
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param ctryCode the ctryCode of interest
#' 
#' @param admLevel The country admin level of interest
#'
#' @param ctryPoly the SpatialPolygonsDataFrame country polygon to process
#'
#' @param path.in.shp The path to the country shapefile
#'
#' @param path.in.r The path to the raster tile
#'
#' @param path.out.r The path where to save the output zonal raster
#'
#' @param path.out.shp The path to save the output zonal shapefile (Ignored)
#'
#' @param zone.attribute The zonal attribute to calculate
#'
#' @param nlStats The stats to calculate
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return TRUE/FALSE
#'
ZonalPipe <- function (ctryCode, admLevel, ctryPoly, path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, nlStats, gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  #Source: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  #path.in.shp: Shapefile with zone (INPUT)
  #path.in.r: Raster from which the stats have to be computed (INPUT)
  #path.out.r: Path of path.in.shp converted in raster (intermediate OUTPUT)
  #path.out.shp: Path of path.in.shp with stat value (OUTPUT)
  #zone.attribute: Attribute name of path.in.shp corresponding to the zones (ID, Country...)
  #nlStat: function to summary path.in.r values ("mean", "sum"...)
  
  if(missing(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(missing(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(missing(path.in.shp))
    stop(Sys.time(), ": Missing required parameter path.in.shp")
  
  if(missing(path.in.r))
    stop(Sys.time(), ": Missing required parameter path.in.r")
  
  if(missing(path.out.r))
    stop(Sys.time(), ": Missing required parameter path.out.r")
  
  if(missing(zone.attribute))
    stop(Sys.time(), ": Missing required parameter zone.attribute")
  
  if(missing(nlStats))
    stop(Sys.time(), ": Missing required parameter nlStats")
  
  if(!validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)
  
  if(!allValid(nlStats, validNlStats))
    stop(Sys.time(), ": Invalid stat(s) detected")
  
  # 1/ Rasterize using GDAL
  
  # Find a better multi-platform way to check for gdal.
  # suppressWarnings(
  #   if(system("which gdal_rasterize", intern = T) != 0)
  #     stop(Sys.time(), ": gdal_rasterize not found. Please check that GDAL is installed")
  # )
  
  #Initiate parameter
  r<-raster::stack(path.in.r)
  
  if (!file.exists(path.out.r))
  {
    message(Sys.time(), ": Zonal file ", path.out.r, " doesn't exist. Creating", date())
    
    #get the extent and change to minx, miny, maxx, maxy order for use
    #in gdal_rasterize. Explanation below
    ext<-raster::extent(r)
    ext<-paste(ext[1], ext[3], ext[2], ext[4])
    
    #get the resolution of the raster. will be used in gdal_rasterize
    #for target resolution which should be the same as the source resolution.
    #Specifying makes it run faster (?)
    res<-paste(raster::res(r)[1], raster::res(r)[2])
    
    lyrName <- admLevel #getCtryShpLowestLyrNames(ctryCode)
    lowestIDCol <- if(!is.null(custPolyPath))
    {
      if(gadmVersion == "2.8")
        paste0("ID_", stringr::str_extract(lyrName, "\\d+$"))
      else if(gadmVersion == "3.6")
        paste0("GID_", stringr::str_extract(lyrName, "\\d+$"), "_IDX")
    }else
    {
      paste0("GID_IDX")
    }
    
    tempRast <- file.path(getNlDir("dirNlTemp"), paste0(basename(tempfile()), ".tif"))
    
    #Gdal_rasterize
    message(Sys.time(), ": Creating zonal raster")
    command<-'gdal_rasterize'
    #Speed-up with more cache (avice: max 1/3 of your total RAM)
    command<-paste(command, paste0("--config GDAL_CACHEMAX ", pkgOptions("gdalCacheMax")))
    command<-paste(command, "-l", lyrName)
    #Identifies an attribute field on the features to be used for a burn
    #in value. The value will be burned into all output bands.
    command<-paste(command, "-a", zone.attribute) 
    #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed
    #in georeferenced units. If not specified, the extent of the output file
    #will be the extent of the vector layers.
    command<-paste(command, "-te", as.character(ext))
    #(GDAL >= 1.8.0) set target resolution. The values must be expressed in
    #georeferenced units. Both must be positive values.
    command<-paste(command, "-tr", res)
    command<-paste(command, path.in.shp)
    command<-paste(command, tempRast)
    
    system(command)
    
    message(Sys.time(), ": Compressing zonal raster")
    gdalUtils::gdal_translate(co = "compress=LZW", 
                              src_dataset = tempRast, 
                              dst_dataset = path.out.r)
    
    file.remove(tempRast)
  }
  
  if(file.exists(path.out.r))
    message(Sys.time(), ": Zonal file ", path.out.r, " found")
  else
    stop(path.out.r, " not found. Zonal creation failed.")
  
  # 2/ Zonal Stat using myZonal function
  zone<-raster::raster(path.out.r)
  
  message(Sys.time(), ": Calculating zonal stats ...")
  Zstat<-data.frame(myZonal(r, zone, nlStats))
  
  message(Sys.time(), ": Calculating zonal stats ... DONE")

  colnames(Zstat)[2:length(Zstat)] <- sapply(nlStats, function(x) x[[1]])
  
  return(Zstat)
  
  # 3/ Merge data in the shapefile and write it #Not required at this point
  #shp<-rgdal::readOGR(path.in.shp, sub("^([^.]*).*", "\\1", basename(path.in.shp)))
  
  #shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])
  
  #rgdal::writeOGR(shp, path.out.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)), driver="ESRI Shapefile")
}

######################## fnAggRadGdal ###################################

#' Calculate zonal statistics using GDAL
#'
#' Calculate zonal statistics using GDAL. Alternative to fnAggRadRast and 
#'     faster. Modified from 
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param ctryCode character string the ISO3 country code to be processed
#' 
#' @param admLevel character string The admin level to process. Should match
#'     the \code{ctryPoly} given but no checks are made currently.
#'
#' @param ctryPoly Polygon the loaded country polygon layer
#' 
#' @param nlType the nlType of interest
#'
#' @param nlPeriod character string the nlPeriod to be processed
#' 
#' @param nlStats character vector The stats to calculate
#' 
#' @param gadmVersion The GADM version to use
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return data.frame of polygon attributes and the calculated stats, one column per stat
#'
#' @examples
#' #read the Kenya polygon downloaded from GADM and load the lowest admin level (ward)
#' \dontrun{
#' ctryPoly <- readCtryPolyAdmLayer(ctryCode="KEN", 
#'     Rnightlights:::getCtryShpLowestLyrNames(ctryCodes="KEN"))
#'     
#' #calculate the sum of radiances for the wards in Kenya
#' sumAvgRadRast <- Rnightlights:::fnAggRadGdal(ctryCode="KEN", ctryPoly=ctryPoly,
#'     nlType="VIIRS.M", nlPeriod="201401", nlStats=c("sum","mean"))
#' }
#'
fnAggRadGdal <- function(ctryCode, admLevel, ctryPoly, nlType, nlPeriod, nlStats=pkgOptions("nlStats"), gadmVersion=pkgOptions("gadmVersion"), custPolyPath=NULL)
{
  if(missing(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if(missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if(!validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if(!allValid(nlStats, validNlStats))
    stop(Sys.time(), ": Invalid stat(s) detected")
  
  #source: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  
  path.in.shp<- getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  path.in.r<- getCtryRasterOutputFnamePath(ctryCode = ctryCode, nlType = nlType, nlPeriod = nlPeriod, gadmVersion = gadmVersion, custPolyPath = custPolyPath) #or path.in.r<-list.files("/home/, pattern=".tif$")
  
  if(stringr::str_detect(nlType, "VIIRS"))
    nlTp <- "VIIRS"
  else
    nlTp <- "OLS"
  
  if(is.null(custPolyPath))
    path.out.r<- file.path(getNlDir("dirZonals"), paste0(admLevel, "_zone_", nlTp, "_", gadmVersion, ".tif"))
  else
    path.out.r<- file.path(getNlDir("dirZonals"), paste0(admLevel, "_zone_", nlTp, ".tif"))
  
  if(is.null(custPolyPath))
    path.out.shp <- file.path(getNlDir("dirZonals"), paste0(admLevel, "_zone_", nlTp, "_", gadmVersion, ".shp"))
  else
    path.out.shp <- file.path(getNlDir("dirZonals"), paste0(admLevel, "_zone_", nlTp, ".shp"))
  
  zone.attribute <- if(is.null(custPolyPath))
  {
    if(gadmVersion == "2.8")
      paste0("ID_", stringr::str_extract(admLevel, "\\d+$"))
    else if(gadmVersion == "3.6")
      paste0("GID_", stringr::str_extract(admLevel, "\\d+$"), "_IDX")
  }else
  {
    paste0("GID_IDX")
  }
  
  lyrName <- admLevel #getCtryShpLowestLyrNames(ctryCode)
  
  lyrIDCol <- if(is.null(custPolyPath))
  {
    if(gadmVersion == "2.8")
      paste0("ID_", stringr::str_extract(lyrName, "\\d+$"))
    else if(gadmVersion == "3.6")
      paste0("GID_", stringr::str_extract(lyrName, "\\d+$"), "_IDX")
  }else
  {
    paste0("GID_IDX")
  }
  
  sumAvgRad <- ZonalPipe(ctryCode = ctryCode,
                         admLevel = admLevel,
                         ctryPoly = ctryPoly,
                         path.in.shp = path.in.shp,
                         path.in.r = path.in.r,
                         path.out.r = path.out.r,
                         path.out.shp = path.out.shp,
                         zone.attribute = zone.attribute,
                         nlStats = nlStats,
                         gadmVersion = gadmVersion,
                         custPolyPath = custPolyPath)
  
  ctryPolyData <- ctryPoly@data
  
  ctryPolyData[,lyrIDCol] <- as.integer(ctryPolyData[,lyrIDCol])
  
  ctryPolyData <- ctryPolyData[order(ctryPolyData[,lyrIDCol]),]
  
  #if there is only the country adm level i.e. no lower adm levels than the country adm level then we only have 1 row each but IDs may not match as seen with ATA. treat differently
  #since we do not have IDs to merge by, we simply cbind the columns and return column 2

  if (grepl("^ID_0$",lyrIDCol))
  {
    sumAvgRad <- cbind(ctryPolyData$ID_0, sumAvgRad[sumAvgRad$z!=0, ])
  }
  else
  {
    sumAvgRad <- merge(ctryPolyData, sumAvgRad, by.x=lyrIDCol, by.y="zones", all.x=T, sort=T)
  }
  
  return(sumAvgRad)
}

######################## fnAggRadRast ###################################

#' Calculate statistics on a nightlight raster that fall within a polygon
#'
#' Calculate stats on the radiance of the pixels in a nightlight raster
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
#' @param nlType Character vector The nlType to process
#' 
#' @param nlStats The statistics to calculate
#' 
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#' 
#' @return data.frame of polygon attributes and the calculated stats, one column per nlStat
#'
#' @examples
#' #read the Kenya polygon downloaded from GADM and load the lowest admin level (ward)
#' \dontrun{
#' ctryPoly <- readCtryPolyAdmLayer(ctryCode="KEN", 
#'     Rnightlights:::getCtryShpLowestLyrNames(ctryCodes="KEN"))
#'     
#' # the VIIRS nightlight raster cropped earlier to the country outline
#' ctryRastCropped <- raster::raster(Rnightlights:::getCtryRasterOutputFnamePath(ctryCode="KEN",
#'     nlType="VIIRS.M", nlPeriod="201401"))
#' 
#' #calculate the sum of radiances for the wards in Kenya
#' sumAvgRadRast <- Rnightlights:::fnAggRadRast(ctryPoly=ctryPoly,
#'     ctryRastCropped=ctryRastCropped, nlType="VIIRS.M", nlStats=c("sum","mean"))
#' }
#' @importFrom foreach %dopar%
fnAggRadRast <- function(ctryPoly, ctryRastCropped, nlType, nlStats, custPolyPath=NULL)
{
  if(missing(ctryPoly))
    stop(Sys.time(), ": Missing required parameter ctryPoly")
  
  if(missing(ctryRastCropped))
    stop(Sys.time(), ": Missing required parameter ctryRastCropped")
  
  if ((class(ctryPoly) != "SpatialPolygons" && class(ctryPoly) != "SpatialPolygonsDataFrame" ) || is.null(ctryPoly))
    stop(Sys.time(), ": Invalid ctryPoly type: ", class(ctryPoly))
  
  if (class(ctryRastCropped) != "RasterLayer" || is.null(ctryRastCropped))
    stop(Sys.time(), ": Invalid ctryRastCropped type: ", class(ctryRastCropped))
  
  if(missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if(!allValid(nlStats, validNlStats))
    stop(Sys.time(), ": Invalid stat(s) detected")

  options(stringsAsFactors = FALSE)
  
  cl <- snow::makeCluster(pkgOptions("numCores"))
  
  doSNOW::registerDoSNOW(cl = cl)
  
  #max=nrow+1 to handle single row cases since must max > min
  pb <- utils::txtProgressBar(min=0, max=nrow(ctryPoly@data), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  
  #to avoid RCheck notes
  i <- NULL
  
  nlStatNames <- sapply(nlStats, function(x) x[[1]])
  
  result <- foreach::foreach(i=1:nrow(ctryPoly@data),
                              .combine=rbind,
                              .export = c("masqOLS", "masqVIIRS", nlStatNames),
                              .packages = c("raster"),
                              .options.snow = list(progress=progress)) %dopar% {
  #for(i in 1:nrow(ctryPoly@data)){
                                
                                  options(stringsAsFactors = FALSE)
                                
                                  pid <- Sys.getpid()
                                  
                                  message(Sys.time(), ": PID:", pid, " Extracting data from polygon " , i)
                                  
                                  if(stringr::str_detect(nlType, "OLS"))
                                    dta <- masqOLS(ctryPoly, ctryRastCropped, i)
                                  else if(stringr::str_detect(nlType, "VIIRS"))
                                    dta <- masqVIIRS(ctryPoly, ctryRastCropped, i)
                                  
                                  message(Sys.time(), ": PID:", pid, " Calculating the NL stats of polygon ", i)
                                  
                                  result <- data.frame(
                                        lapply(nlStats,
                                               FUN=function(nlStat)
                                               {
                                                 if(length(nlStat) > 1)
                                                   nlStatParams <- gsub("(\\(\\s*)=|(,\\s*)=", "\\1\\2", paste0(", ", paste(names(nlStat[-1]),nlStat[-1], sep="=", collapse=",")))
                                                 else
                                                   nlStatParams <- NULL
                                                 
                                                 nlStat = nlStat[[1]]
                                                 
                                                 nlStatArgs <- formals(nlStat)
                                                 
                                                 retVal <- if(all(sapply(c("col","row"), "%in%",names(nlStatArgs))))
                                                   "colrowval"
                                                 else if(all(sapply(c("lon","lat"), "%in%",names(nlStatArgs))))
                                                   "lonlatval"
                                                 else
                                                   NULL

                                                 fnTxt <- if(is.null(retVal))
                                                   paste0(nlStat, "(dta$vals", nlStatParams, ")")
                                                 else if(retVal == "colrowval")
                                                   paste0(nlStat, "(col=dta$cols, row=dta$rows, val=dta$vals", nlStatParams, ")")
                                                 else if(retVal == "lonlatval")
                                                   paste0(nlStat, "(lon=dta$lons, lat=dta$lats, val=dta$vals", nlStatParams, ")")
                                                 #browser()
                                                 fnTxt <- paste0("data.frame('", nlStat, "' = matrix(", fnTxt, ", nrow=1))")
                                                 eval(parse(text=fnTxt))
                                               }
                                               )
                                        )
                                }

  #count cols with nlStat in them
  nlStatColCounts <- sapply(nlStatNames, function(nlStat) length(grep(paste0("^",nlStat,"\\.*"), names(result))))
  
  #collapse multi-value cols into one
  for(nlStatName in nlStatNames)
  {
    if(nlStatColCounts[[nlStatName]] > 1)
    {
      message(Sys.time(), ": ", nlStatName, " => Multi-column result detected. Merging")
      
      #if more than one col detected, get their names
      nlStatCols <- grep(nlStatName, names(result), value = T)

      #merge them row-wise into one character vector separated by comma
      result[[nlStatName]] <- apply(result[, nlStatCols], 1, function(x) paste(x, collapse=","))
      
      #remove the separate cols
      result[,nlStatCols] <- NULL
    }else
    {
      nlStatCols <- grep(nlStatName, names(result), value = T)
      
      names(result)[which(names(result)==nlStatCols)] <- nlStatName
    }
  }
  
  #change the col order to the order of nlStats
  result <- result[, nlStatNames]
  
  close(pb)
  snow::stopCluster(cl)
  
  raster::removeTmpFiles(h=0)
  
  gc()
  
  return(stats::setNames(data.frame(result), nlStatNames))
}
