dnldGADMCtryRDS <- function(ctryCode,
                            gadmVersion = pkgOptions("gadmVersion"),
                            gadmPolyType=pkgOptions("gadmPolyType"))
{
  if(file.exists(getPolyFnameRDS(ctryCode = ctryCode)))
    stop(ctryCode, " ", gadmVersion, " RDS already exists. Please delete it to force")
  
  if(gadmVersion == "2.8")
    baseUrl <- paste0("https://biogeo.ucdavis.edu/data/gadm", gadmVersion, "/rds/")
  else
    baseUrl <- paste0("https://biogeo.ucdavis.edu/data/gadm", gsub("\\.", "", gadmVersion), "/Rsp/")
  
  ctryPolyList <- NULL
  
  res <- 0
  
  for (idx in 0:4)
  {
    if(gadmVersion == "2.8")
      fName <- paste0(ctryCode, "_adm", idx, ".rds")
    else
      fName <- paste0("gadm", gsub("\\.", "", gadmVersion), "_", ctryCode, "_", idx, "_sp.rds")
    
    message("Processing ", fName)
    
    dnldUrl <- paste0(baseUrl, fName)
    
    tempFname <- file.path(getNlDir("dirNlTemp"), fName)
    
    if(!file.exists(tempFname))
      res <- try(utils::download.file(url = dnldUrl, destfile = tempFname, method = "auto"), TRUE)
    
    if(inherits(res, "try-error"))
      break()
    
    ctryPoly <- readRDS(file = tempFname)
    
    lowestIDCol <- paste0("GID_", idx, "_IDX")
    
    #for GADM 3.6 polygons the attribute col GID_3 contains
    #strings which cannot be used by gdal_rasterize
    #Create an integer col in the shapefile corresponding to the unique GIDs
    if(gadmVersion == "3.6" && is.null(ctryPoly@data[[lowestIDCol]]))
    {
      message(Sys.time(), ": Creating integer zone attribute col for polygon")
      
      lowestIDColOrig <- gsub("_IDX", "", lowestIDCol)
      
      ctryPoly@data[,lowestIDCol] <- 1:length(sort(ctryPoly@data[,lowestIDColOrig]))  # Make new attribute
    }
    
    if(!dir.exists(getPolyFnamePath(ctryCode = ctryCode,
                                    gadmVersion = gadmVersion,
                                    gadmPolyType=gadmPolyType)))
    {
      message(Sys.time(), ": Writing shapefile layer")
      rgdal::writeOGR(obj = ctryPoly,
                      dsn = getPolyFnamePath(ctryCode = ctryCode,
                                             gadmVersion = gadmVersion,
                                             gadmPolyType = gadmPolyType),
                      layer = paste0("gadm_", ctryCode, "_", idx),
                      driver = "ESRI Shapefile",
                      overwrite_layer = T) # Save new version of shapefile
    }
    
    ctryPolyList <- append(ctryPolyList, ctryPoly)
  }
  
  message("saving combined RDS")
  
  if(!file.exists(getPolyFnameRDS(ctryCode = ctryCode,
                                  gadmVersion = gadmVersion,
                                  gadmPolyType = gadmPolyType)))
    saveRDS(ctryPolyList, getPolyFnameRDS(ctryCode = ctryCode,
                                          gadmVersion = gadmVersion,
                                          gadmPolyType = gadmPolyType))
  
  if(!file.exists(getCtryStructFnamePath(ctryCode,
                                         gadmVersion = gadmVersion,
                                         gadmPolyType = gadmPolyType)))
    createCtryStruct(ctryCode = ctryCode,
                     gadmVersion = gadmVersion,
                     gadmPolyType = gadmPolyType)
}
