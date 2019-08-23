#'@export
myfun <- function(col, row, val, numPts=3) #function(dt, numPts=3)
{
  dt <- data.frame(col, row, val)
  
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