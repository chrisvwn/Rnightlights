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
#' \dontrun{validStat("sum")}
#'  #returns TRUE
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
#' @param ... Other params to pass to the stats function
#'
#' @return numeric value result of the given stat function
#' 
#' @import data.table
myZonal <- function (x, z, stats, digits = 0, na.rm = TRUE, ...)
{
  #http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/

  varx <- function(x, ...) ifelse(length(x) > 1, stats::var(x, ...), x)
  
  statsFn <- lapply(stats, function(x) switch(x, sum="sum", mean="mean", var="varx"))
  
  fun <- paste0(sapply(statsFn, function(stat) paste0(stat,"=", stat, "(x, na.rm = TRUE)")), collapse = ", ")
  
  #reference .SD as data.table::.SD yields empty output
  #funs <- paste0("rDT[, as.list(unlist(lapply(data.table::.SD, function(x) list(", fun, ")))), by=z]")
  funs <- paste0("rDT[, as.list(unlist(lapply(.SD, function(x) list(", fun, ")))), by=z]")

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

    message("Calculating partial stats")
    #result <- rbind(result, rDT[, lapply(.SD, fun, na.rm = TRUE), by=z])
    result <- rbind(result, eval(parse(text = funs)))
  }
  
  resultfun <- paste0(paste0(stats,"="),paste0(statsFn, paste0("(",names(result)[2:ncol(result)],", na.rm=TRUE)")), collapse = ", ")
  
  resultfuns <- paste0("result[, list(", resultfun, "), by=z]")
  
  result <- eval(parse(text = resultfuns))
  
  result <- stats::setNames(result, c("z",stats))
  
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
#' @param ctryCode the ctryCode of interest
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
#' @param stats The stats to calculate
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
  
  # Find a better multi-platform way to check for gdal.
  # suppressWarnings(
  #   if(system("which gdal_rasterize", intern = T) != 0)
  #     stop("gdal_rasterize not found. Please check that GDAL is installed")
  # )
  
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
    
    tempRast <- file.path(getNlDir("dirZonals"), "temprast.tif")
    
    #Gdal_rasterize
    message("Creating zonal raster")
    command<-'gdal_rasterize'
    command<-paste(command, paste0("--config GDAL_CACHEMAX ", pkgOptions("gdal_cachemax"))) #Speed-up with more cache (avice: max 1/3 of your total RAM)
    command<-paste(command, "-l", lowestLyrName)
    #command<-paste(command, "-where", paste0(lowestIDCol, "=", i))
    command<-paste(command, "-a", zone.attribute) #Identifies an attribute field on the features to be used for a burn in value. The value will be burned into all output bands.
    command<-paste(command, "-te", as.character(ext)) #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed in georeferenced units. If not specified, the extent of the output file will be the extent of the vector layers.
    command<-paste(command, "-tr", res) #(GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced units. Both must be positive values.
    command<-paste(command, path.in.shp)
    command<-paste(command, tempRast)
    
    system(command)
    
    message("Compressing zonal raster")
    gdalUtils::gdal_translate(co = "compress=LZW", src_dataset = tempRast, dst_dataset = path.out.r)
    
    file.remove(tempRast)
  }
  
  if(file.exists(path.out.r))
    message("Zonal file ", path.out.r, " found")
  else
    stop(path.out.r, " not found. Zonal creation failed.")
  
  # 2/ Zonal Stat using myZonal function
  zone<-raster::raster(path.out.r)
  
  message("Calculating zonal stats ...")
  Zstat<-data.frame(myZonal(r, zone, stats))
  
  message("Calculating zonal stats ... DONE")

  colnames(Zstat)[2:length(Zstat)] <- stats
  
  return(Zstat)
  
  # 3/ Merge data in the shapefile and write it #Not required at this point
  #shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))
  
  #shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])
  
  #writeOGR(shp, path.out.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)), driver="ESRI Shapefile")
}

######################## fnAggRadGdal ###################################

#' Calculate zonal statistics using GDAL. Faster than fnAggRadRast for large polygons.
#'
#' Calculate zonal statistics. Alternative to fnAggRadRast using GDAL. Faster for large polygons. 
#'     Modified from \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param ctryCode character string the ISO3 country code to be processed
#'
#' @param ctryPoly Polygon the loaded country polygon layer
#'
#' @param nlPeriod character string the nlPeriod to be processed
#' 
#' @param fnStats character vector The stats to calculate
#' 
#' @param nlType the nlType of interest
#'
#' @return data.frame of polygon attributes and the calculated stats
#'
#' @examples
#' \dontrun{validNlMonthNum("01","VIIRS")}
#'  #returns TRUE
#'
fnAggRadGdal <- function(ctryCode, ctryPoly, nlPeriod, fnStats=pkgOptions("stats"), nlType)
{
  if(missing(ctryCode))
    stop("Missing required parameter ctryCode")
  
  if(missing(nlPeriod))
    stop("Missing required parameter nlPeriod")
  
  if(!validCtryCode(ctryCode))
    stop("Invalid ctryCode: ", ctryCode)
  
  if(nchar(nlPeriod) == 4)
    nlType <- "OLS"
  else if(nchar(nlPeriod) == 6)
    nlType <- "VIIRS"
  
  if(!validNlPeriod(nlPeriod, nlType))
    stop("Invalid nlPeriod: ", nlPeriod, " for nlType: ", nlType)
  
  if(!allValid(fnStats, validStat))
    stop("Invalid stat(s) detected")
  
  #source: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  
  path.in.shp<- getPolyFnamePath(ctryCode)
  
  path.in.r<- getCtryRasterOutputFname(ctryCode, nlType, nlPeriod) #or path.in.r<-list.files("/home/, pattern=".tif$")
  path.out.r<- file.path(getNlDir("dirZonals"), paste0(ctryCode, "_zone_", nlType, ".tif"))
  
  path.out.shp<-file.path(getNlDir("dirZonals"), "zone_withZstat.shp")
  
  zone.attribute<-paste0("ID_", gsub("[^[:digit:]]", "", getCtryShpLowestLyrName(ctryCode)))
  
  lowestLyrName <- getCtryShpLowestLyrName(ctryCode)
  
  lowestIDCol <- paste0("ID_", gsub("[^[:digit:]]", "", lowestLyrName))
  
  sumAvgRad <- ZonalPipe(ctryCode, ctryPoly, path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stats=fnStats)
  
  ctryPolyData <- ctryPoly@data
  
  ctryPolyData[,lowestIDCol] <- as.integer(ctryPolyData[,lowestIDCol])
  
  ctryPolyData <- ctryPolyData[order(ctryPolyData[,lowestIDCol]),]
  
  #if there is only the country adm level i.e. no lower adm levels than the country adm level then we only have 1 row each but IDs may not match as seen with ATA. treat differently
  #since we do not have IDs to merge by, we simply cbind the columns and return column 2

  if (lowestIDCol == "ID_0")
  {
    sumAvgRad <- cbind(ctryPolyData$ID_0, sumAvgRad[sumAvgRad$z!=0, ])
  }
  else
  {
    sumAvgRad <- merge(ctryPolyData, sumAvgRad, by.x=lowestIDCol, by.y="z", all.x=T, sort=T)
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
#' @param stats The statistics to calculate
#' 
#' @param nlType Character vector The nlType to process
#'
#' @return Integer Sum of radiances of all pixels in a raster that fall within a polygon region
#'
#' @examples
#' #read the Kenya polygon downloaded from GADM and load the lowest admin level (ward)
#' \dontrun{
#' ctryPoly <- readOGR(getPolyFnamePath("KEN"), getCtryShpLowestLyrName("KEN"))
#'     
#' # the VIIRS nightlight raster cropped earlier to the country outline
#' ctryRastCropped <- getCtryRasterOutputFname("KEN","VIIRS","201401")
#' 
#' #calculate the sum of radiances for the wards in Kenya
#' sumAvgRadRast <- fnAggRadRast(ctryPoly, ctryRastCropped)
#' }
#' @importFrom foreach %dopar%
fnAggRadRast <- function(ctryPoly, ctryRastCropped, stats, nlType)
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
  
  doParallel::registerDoParallel(cores=pkgOptions("numCores"))
  
  #to avoid notes
  i <- NULL
  
  sumAvgRad <- foreach::foreach(i=1:nrow(ctryPoly@data), 
                                .combine=rbind,
                                .export = c("masqOLS", "masqVIIRS"),
                                .packages = c("raster")) %dopar% {
                                  
                                  message("Extracting data from polygon " , i, " ", base::date())
                                  
                                  if(nlType=="OLS")
                                    dat <- masqOLS(ctryPoly, ctryRastCropped, i)
                                  else if(nlType=="VIIRS")
                                    dat <- masqVIIRS(ctryPoly, ctryRastCropped, i)
                                  
                                  message("Calculating the NL stats of polygon ", i, " ", base::date())
                                  
                                  #calculate and return the mean of all the pixels
                                  #data.frame(sum = sum(dat, na.rm=TRUE))
                                  
                                  sumAvgRad <- data.frame(sapply(stats, FUN=function(stat) data.frame(stat = eval(parse(text=paste0(stat, "(dat, na.rm=TRUE)"))))))
                                  
                                  stats::setNames(sumAvgRad, stats)
                                }
  
  raster::removeTmpFiles(h=0)
  
  gc()
  
  return(sumAvgRad)
}
