######################## masqVIIRS ###################################

#' extract data from a raster using one polygon in a multipolygon
#'
#' extract data from a raster using one polygon in a multipolygon. 
#'
#' @param ctryPoly the country Polygon layer as SpatialPolygon
#'
#' @param ctryRast the clipped country raster
#'
#' @param idx the index of the polygon in the country polygon layer (shp)
#' 
#' @param retVal Whether to return the raster data as a vector, or 
#'     data.frame with spatial context NULL returns a vector of all
#'     values, colrowval returns a data.frame with row, col and raster
#'     value while lonlatval returns a data.frame with lon,lat and val.
#'     
#' @param configName character the type of raster being processed
#'
#' @return numeric vector of radiances
#'
#' @examples
#' \dontrun{
#' ctryPoly <- rgdal::readOGR('path/to/polygon.shp')
#' 
#' ctryRaster <- raster::raster('path/to/raster.tif')
#' 
#' #get the sum of nightlight pixels in the first polygon in a multipolygon
#' sumPolygon1 <- sum(masqVIIRS(ctryPoly, ctryRaster, 1), na.rm=T)
#' }
#'
masqVIIRS <- function(ctryPoly, ctryRast, idx, retVal="colrowval", configName)
{
  #Extract one polygon based on index value i
  polygon <- ctryPoly[idx,] #extract one polygon
  
  extent <- raster::extent(x = polygon) #extract the polygon extent
  
  #Raster extract
  outer <- raster::crop(x = ctryRast, y = extent) #extract raster by polygon extent
  
  #inner <- mask(outer,polygon) #keeps values from raster extract that are within polygon
  
  inner <- raster::rasterize(x = polygon, y = outer, mask = TRUE) #crops to polygon edge & converts to raster
  
  #Convert cropped raster into a vector
  #Specify coordinates
  lonlats <- expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
                        seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
  
  colrows <- expand.grid(1:ncol(inner), 1:nrow(inner))

  #Convert raster into vector
  vals <- as.vector(x = inner)
  
  #data <- data[!is.na(data)] #keep non-NA values only ... shall this affect mask values?
  #data[is.na(data)] <- 0
  vals[vals < 0] <- NA #any negative values are either recording problems or error values as per: ... Negative values would distort most calculations.
  
  data <- stats::setNames(data.frame(colrows,lonlats,vals), c("cols","rows","lons","lats","vals"))
  
  return(data)
}

######################## masqOLS ###################################

#' Extract raster pixel values within the boundaries of a polygon
#'
#' Extract raster pixel values within the boundaries of a polygon
#'
#' @param shp the country Polygon layer as SpatialPolygon
#'
#' @param rast the clipped country raster
#'
#' @param i the index of the polygon in the country polygon layer (shp)
#' 
#' @param retVal Whether to return the raster data as a vector, or 
#'     data.frame with spatial context NULL returns a vector of all
#'     values, colrowval returns a data.frame with row, col and raster
#'     value while lonlatval returns a data.frame with lon,lat and val.
#'     
#' @param configName character the type of raster being processed
#'
#' @return numeric vector of radiances
#'
#' @examples
#' \dontrun{
#' ctryPoly <- rgdal::readOGR(getPolyFnamePath("KEN"), getCtryShpLyrNames("KEN", 1))
#' ctryRaster <- raster::raster(getCtryRasterOutputFnamePath("KEN", "OLS", "1999"))
#' temp <- NULL
#' KenAdm1Sum <- NULL
#' for (i in 1:length(ctryPoly@polygons))
#' {
#'  temp$name <- as.character(ctryPoly@data$NAME_1[i])
#'  temp$sum <- sum(masqOLS(ctryPoly, ctryRaster, i), na.rm=T)
#'
#'  KenAdm1Sum <- rbind(KenAdm1Sum)
#' }
#' }
#'
masqOLS <- function(shp, rast, i, retVal="colrowval", configName)
{
  #Extract one polygon based on index value i
  polygon <- shp[i,] #extract one polygon
  extent <- raster::extent(polygon) #extract the polygon extent
  
  #Raster extract
  #if we are in a gasflare zone it is possible that the raster has been cropped and
  #will not intersect with the poly. We expect an error here
  #if an error is encountered we will assign NA as the only value
  result <- try(outer <- raster::crop(rast, extent), silent = TRUE) #extract raster by polygon extent

  if(!inherits(x = result, what = "try-error"))
  {
    #we should be okay here
    inner <- raster::rasterize(x = polygon, y = outer, mask=TRUE) #crops to polygon edge & converts to raster
    
    rowVals <- NULL
    colVals <- NULL
    
    #get the local rows and cols
    #colrowsLocal <- expand.grid(1:ncol(inner), 1:nrow(inner))
    
    ext <- raster::extent(inner) #extract the cropped raster extent
    
    #get the cells that this polygon represent in the ctryPolygon
    xyrast <- raster::cellFromXY(object = rast, raster::xyFromCell(inner, 1:raster::ncell(inner)))
    
    #get the rows and cols this polygon represents in the ctryPolygon
    colrows <- raster::rowColFromCell(object = rast, cell = xyrast)
    
    colrows <- colrows[ ,c(2,1)]
    
    #Get the cell coordinates
    lonlats <- raster::xyFromCell(object = rast, cell = xyrast)
    
    rm(xyrast)
    
    #xyFromCell gives the center of the cell. Subtract 1/2 res to get top-left
    #lonlatsTopLeft <- lonlats - raster::res(inner)/2
    
    vals <- as.vector(inner)
  } else
  {
    #put NAs
    colrows <- data.frame(cols=NA,rows=NA)
    lonlats <- data.frame(lats=NA,lons=NA)
    vals <- NA
  }
  
  # source: https://ngdc.noaa.gov/eog/gcv4_readme.txt
  # Three image types are
  # available as geotiffs for download from the version 4 composites:
  #   
  #   
  #   F1?YYYY_v4b_cf_cvg.tif: Cloud-free coverages tally the total 
  # number of observations that went into each 30 arc second grid cell. This
  # image can be used to identify areas with low numbers of observations
  # where the quality is reduced. In some years there are areas with zero
  # cloud- free observations in certain locations.
  # 
  # 
  # F1?YYYY_v4b_avg_vis.tif: Raw avg_vis contains the average of the 
  # visible band digital number values with no further filtering. Data
  # values range from 0-63. Areas with zero cloud-free observations are
  # represented by the value 255.
  # 
  # 
  # F1?YYYY_v4b_stable_lights.avg_vis.tif: The cleaned up avg_vis 
  # contains the lights from cities, towns, and other sites with persistent
  # lighting, including gas flares. Ephemeral events, such as fires have
  # been discarded. Then the background noise was identified and replaced
  # with values of zero. Data values range from 1-63. Areas with zero
  # cloud-free observations are represented by the value 255.
  
  if(configName %in% c("avg_vis", "stable_lights"))
  {
    #not for cf_cvg
    #in DMSP-OLS 255 == NA
    vals[which(vals == 255)] <- NA
  }

  #negative values are errors replace with NA
  #vals[vals < 0] <- NA
  
  #Convert raster into vector
  dta <- stats::setNames(data.frame(colrows, lonlats, vals), c("cols","rows","lons","lats","vals"))

  dta <- dta[!is.na(dta$vals), ]
  
  return(dta)
}
