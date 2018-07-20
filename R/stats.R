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
    stop("Missing required parameter nlStats")
  
  if(!is.character(nlStats) || is.null(nlStats) || is.na(nlStats) || nlStats == "")
    stop("Invalid nlStats")
  
  matchedFuns <- sapply(nlStats, function(nlStat)
    tryCatch(
    {
      matched <- !is.null(match.fun(nlStat))
    }, error = function(err)
    {
      message(paste0("Invalid nlStat: ", nlStat))
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
#' @param x the country raster
#'
#' @param z the zonal country polygon layer
#'
#' @param nlStats a character list of statistics to calculate
#'
#' @param digits round off to how many decimals
#'
#' @param na.rm how to handle NAs
#'
#' @param ... Other params to pass to the nlStats functions e.g. na.rm
#'
#' @return numeric value result of the given nlStat function
#' 
#' @import data.table
myZonal <- function (x, z, nlStats, digits = 0, na.rm = TRUE, ...)
{
  options(fftempdir=getNlDir("dirNlTemp"), fffinalizer="delete")
  
  #vreate the text for the functions
  fun <- paste0(sapply(nlStats, function(nlStat) paste0(nlStat,"=", nlStat, "(x, na.rm = TRUE)")), collapse = ", ")

  #create the aggregation function
  funs <- paste0("dta[, as.list(unlist(lapply(.SD, function(x) list(", fun, ")))), by=zones]")
  
  vals <- NULL
  
  zones <- NULL
  
  message("Reading in raster data")

  #the number of columns in the raster/zone file which are identical in size
  nc <- base::ncol(x)
  
  #get the block size recommendation
  tr <- raster::blockSize(x)
  
  #init the progress bar
  pb <- utils::txtProgressBar(min=0, max=tr$n, style=3)
  
  #for each block
  for (i in 1:tr$n)
  {
    start <- ((tr$row[i]-1) * nc) + 1
    
    end <- start + (tr$nrows[i] * nc) - 1
    
    rastVals <- raster::getValues(x, row=tr$row[i], nrows=tr$nrows[i])
    zoneVals <- raster::getValues(z, row=tr$row[i], nrows=tr$nrows[i])
   
    #convert negative values to NA 
    rastVals[rastVals < 0] <- NA
    
    #get all pixels in zone 0 which should be
    #the pixels outside the polygon
    idxZone0 <- which(zoneVals == 0)
    
    #remove zone 0 since in large rasters it can be pretty
    #large and may not fit in memory thus causing calculations to fail
    zoneVals <- zoneVals[-idxZone0]
    rastVals <- rastVals[-idxZone0]
    
    #for first block init the ff to the given filename
    if (i == 1)
    {
      vals <- ff::ff(initdata = rastVals, finalizer = "delete", overwrite = T)
      zones <- ff::ff(initdata = zoneVals, finalizer = "delete", overwrite = T)
    } 
    else 
    {
      #otherwise append
      vals <- ffbase::ffappend(vals, rastVals, adjustvmode = T)
      zones <- ffbase::ffappend(zones, zoneVals, adjustvmode = T)
    }
    
    #upddate progress bar
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  #merge the zone and raster ffvectors into an ffdf
  rDT <- ff::ffdf(zones, vals)

  message("Calculating nlStats ", base::date())
  
  if(length(unique(rDT$zones) == 1))
  {
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
                             BATCHBYTES = 80.85*2^20,
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
  #name the columns
  result <- stats::setNames(result, c("z", nlStats))

  resultDF <- as.data.frame(result)
  
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
  
  if(missing(nlStats))
    stop("Missing required parameter nlStats")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!allValid(nlStats, validNlStats))
    stop("Invalid stat(s) detected")
  
  # 1/ Rasterize using GDAL
  
  # Find a better multi-platform way to check for gdal.
  # suppressWarnings(
  #   if(system("which gdal_rasterize", intern = T) != 0)
  #     stop("gdal_rasterize not found. Please check that GDAL is installed")
  # )
  
  #Initiate parameter
  r<-raster::stack(path.in.r)
  
  if (!file.exists(path.out.r))
  {
    message("Zonal file ", path.out.r, " doesn't exist. Creating", date())
    
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
    message("Creating zonal raster")
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
    
    message("Compressing zonal raster")
    gdalUtils::gdal_translate(co = "compress=LZW", 
                              src_dataset = tempRast, 
                              dst_dataset = path.out.r)
    
    file.remove(tempRast)
  }
  
  if(file.exists(path.out.r))
    message("Zonal file ", path.out.r, " found")
  else
    stop(path.out.r, " not found. Zonal creation failed.")
  
  # 2/ Zonal Stat using myZonal function
  zone<-raster::raster(path.out.r)
  
  message("Calculating zonal stats ...", date())
  Zstat<-data.frame(myZonal(r, zone, nlStats))
  
  message("Calculating zonal stats ... DONE", date())

  colnames(Zstat)[2:length(Zstat)] <- nlStats
  
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
    stop("Missing required parameter ctryCode")
  
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(!validCtryCodes(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop("Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if(!allValid(nlStats, validNlStats))
    stop("Invalid stat(s) detected")
  
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
  
  sumAvgRad <- ZonalPipe(ctryCode = ctryCode, admLevel = admLevel, ctryPoly = ctryPoly, path.in.shp = path.in.shp, path.in.r = path.in.r, path.out.r = path.out.r, path.out.shp = path.out.shp, zone.attribute = zone.attribute, nlStats=nlStats, gadmVersion = gadmVersion, custPolyPath = custPolyPath)
  
  ctryPolyData <- ctryPoly@data
  
  ctryPolyData[,lyrIDCol] <- as.integer(ctryPolyData[,lyrIDCol])
  
  ctryPolyData <- ctryPolyData[order(ctryPolyData[,lyrIDCol]),]
  
  #if there is only the country adm level i.e. no lower adm levels than the country adm level then we only have 1 row each but IDs may not match as seen with ATA. treat differently
  #since we do not have IDs to merge by, we simply cbind the columns and return column 2

  if (grepl("ID_0",lyrIDCol))
  {
    sumAvgRad <- cbind(ctryPolyData$ID_0, sumAvgRad[sumAvgRad$z!=0, ])
  }
  else
  {
    sumAvgRad <- merge(ctryPolyData, sumAvgRad, by.x=lyrIDCol, by.y="z", all.x=T, sort=T)
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
    stop("Missing required parameter ctryPoly")
  
  if(missing(ctryRastCropped))
    stop("Missing required parameter ctryRastCropped")
  
  if ((class(ctryPoly) != "SpatialPolygons" && class(ctryPoly) != "SpatialPolygonsDataFrame" ) || is.null(ctryPoly))
    stop("Invalid ctryPoly type: ", class(ctryPoly))
  
  if (class(ctryRastCropped) != "RasterLayer" || is.null(ctryRastCropped))
    stop("Invalid ctryRastCropped type: ", class(ctryRastCropped))
  
  if(missing(nlType))
    stop("Missing required parameter nlType")
  
  if(!allValid(nlStats, validNlStats))
    stop("Invalid stat(s) detected")

  cl <- snow::makeCluster(pkgOptions("numCores"))
  
  doSNOW::registerDoSNOW(cl = cl)
  
  #max=nrow+1 to handle single row cases since must max > min
  pb <- utils::txtProgressBar(min=0, max=nrow(ctryPoly@data), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  
  #to avoid RCheck notes
  i <- NULL
  
  sumAvgRad <- foreach::foreach(i=1:nrow(ctryPoly@data), 
                                .combine=rbind,
                                .export = c("masqOLS", "masqVIIRS", nlStats),
                                .packages = c("raster"),
                                .options.snow = list(progress=progress)) %dopar% {
                                  
                                  pid <- Sys.getpid()
                                  
                                  message("PID:", pid, " Extracting data from polygon " , i, " ", base::date())
                                  
                                  if(stringr::str_detect(nlType, "OLS"))
                                    dat <- masqOLS(ctryPoly, ctryRastCropped, i)
                                  else if(stringr::str_detect(nlType, "VIIRS"))
                                    dat <- masqVIIRS(ctryPoly, ctryRastCropped, i)
                                  
                                  message("PID:", pid, " Calculating the NL stats of polygon ", i, " ", base::date())
                                  
                                  #calculate and return the mean of all the pixels
                                  #data.frame(sum = sum(dat, na.rm=TRUE))
                                  
                                  sumAvgRad <- data.frame(sapply(nlStats, FUN=function(nlStat) data.frame(nlStat = eval(parse(text=paste0(nlStat, "(dat, na.rm=TRUE)"))))))
                                  
                                  stats::setNames(sumAvgRad, nlStats)
                                }
  
  close(pb)
  snow::stopCluster(cl)
  
  raster::removeTmpFiles(h=0)
  
  gc()
  
  return(sumAvgRad)
}
