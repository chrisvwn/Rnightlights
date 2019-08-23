######################## myfun ###################################

#' Function to test the use of rowcols and lonlats in getCtryNlData
#'
#' Function to test the use of rowcols and lonlats in getCtryNlData as
#'     input to the nlStats parameter.
#'     
#'     The function gets the top numPts brightest pixels in a sub-polygon
#'     or zone and looks at increasing neighbouring distances looking
#'     for the distance to the first pixel that is 1/2 the value of the
#'     central pixel. 
#' 
#' @param col character parameter to tell the package to pass in the colrow
#'     values i.e. column and row indices as per the country polygon
#' 
#' @param row character parameter to tell the package to pass in the colrow
#'     values i.e. column and row indices of non-NA pixels as per the country
#'     polygon
#'     
#' @param lon character parameter to tell the package to pass in the lonlat
#'     values i.e. column and row indices of non-NA pixels as per the country
#'     polygon
#' 
#' @param lat character parameter to tell the package to pass in the lonlat
#'     values i.e. column and row indices of non-NA pixels as per the country
#'     polygon
#' 
#' @param val character variable which will hold the raster values found in
#'     in the raster
#'     
#' @param numPts integer The number of top pixels to detect
#' 
#' @examples
#' 
#' \dontrun{
#' pkgReset();
#'   system.time(rastGFT92 <-  getCtryNlData(ctryCode = "NGA",
#'      admLevel = "lowest", nlTypes = "OLS.Y", nlPeriods = "1992",
#'      removeGasFlares = F, ignoreMissing=FALSE, nlStats = list("myfun")))
#'    
#'   pkgOptions(extractMethod="gdal",cropMaskMethod="gdal");
#'   system.time(gdalGFT92 <-  getCtryNlData(ctryCode = "NGA",
#'       admLevel = "lowest", nlTypes = "OLS.Y", nlPeriods = "1992",
#'       removeGasFlares = F, ignoreMissing=FALSE, nlStats="myfun"))
#' }
#'
#' @export 
myfun <- function(col, row, lon, lat, val, numPts=3) #function(dt, numPts=3)
{
  dt <- data.frame(col, row)
  
  topPts <- dt[order(dt$val, decreasing = T)[1:numPts],]
  
  res <- NULL
  
  for(i in 1:nrow(topPts))
  {
    topPt <- topPts[i,]
    
    #print(topPt)
    
    nbIdx <- 1
    
    while(TRUE)
    {
      #print(paste0("nbIdx: ", nbIdx))
      
      nbrs <- dt[(abs(dt$col-topPt$col) == nbIdx & abs(dt$row - topPt$row) <= nbIdx) |
                   (abs(dt$row-topPt$row) == nbIdx & abs(dt$col - topPt$col) <= nbIdx), ]
      
      # plot(dt$col, dt$row)
      # points(topPt$col, topPt$row, pch=21)
      # points(nbrs$col[is.na(nbrs$val)], nbrs$row[is.na(nbrs$val)], pch=0)
      # points(nbrs$col[!is.na(nbrs$val)], nbrs$row[!is.na(nbrs$val)], pch=19)
      # points(nbrs$col[nbrs$val <= topPt$val/2], nbrs$row[nbrs$val <= topPt$val/2], pch=11)
      
      nbrs <- nbrs[!is.na(nbrs$val),]
      
      if(nrow(nbrs)==0 || any(nbrs$val <= topPt$val/2))
        break()

      nbIdx <- nbIdx + 1
    }
    
    res <- rbind(res, cbind.data.frame(topPt, nbIdx))
  }
  
  return(res$nbIdx)
}

#pkgReset();pkgOptions(numCores=6);system.time(rastGFT92 <-  getCtryNlData(ctryCode = "NGA", admLevel = "lowest", nlTypes = "OLS.Y", nlPeriods = "1992", removeGasFlares = F, ignoreMissing=FALSE, nlStats = list("myfun")))

#pkgOptions(extractMethod="gdal",cropMaskMethod="gdal"); system.time(gdalGFT92 <-  getCtryNlData(ctryCode = "NGA", admLevel = "lowest", nlTypes = "OLS.Y", nlPeriods = "1992", removeGasFlares = F, ignoreMissing=FALSE, nlStats="myfun"))