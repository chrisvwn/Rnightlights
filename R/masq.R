######################## masqVIIRS ###################################

#' extract data from a raster using one polygon in a multipolygon
#'
#' extract data from a raster using one polygon in a multipolygon. 
#'     Modified from \url{https://commercedataservice.github.io/tutorial_viirs_part1/}
#'
#' @param ctryPoly the country Polygon layer as SpatialPolygon
#'
#' @param ctryRast the clipped country raster
#'
#' @param idx the index of the polygon in the country polygon layer (shp)
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
#' @return numeric vector of radiances
#'
#' @examples
#' \dontrun{
#' ctryPoly <- readOGR(getPolyFnamePath("KEN"), getCtryShpLyrName("KEN", 1))
#' ctryRaster <- raster(getCtryRasterOutputFname("KEN", "OLS", "1999"))
#' temp <- NULL
#' KenAdm1Sum <- NULL
#' for (i in 1:5)#length(ctryPoly@polygons))
#' {
#'  temp$name <- as.character(ctryPoly@data$NAME_1[i])
#'  temp$sum <- sum(masqOLS(ctryPoly, ctryRaster, i), na.rm=T)
#'
#'  KenAdm1Sum <- rbind(KenAdm1Sum)
#' }
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
