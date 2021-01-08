######################## getNlGasFlaresRdsFnamePath ###################################

#' Get the path to the saved world gasflare RDS
#'
#' Get the path to the saved world gasflare RDS
#'
#' @return \code{character} the path to the world gasflare RDS
#'
#' @examples
#' \dontrun{
#'   getNlGasFlaresRdsFnamePath(ctryCode="NGA")
#'   #returns path to the world RDS
#' }
#'
getNlGasFlaresRdsFnamePath <- function()
{
  return(file.path(getNlDir("dirNlGasFlares"), "gasflaresmosaic.rds"))
}

######################## getNlCtryGasFlaresRdsFnamePath ###################################

#' Get the path to the saved country gasflare RDS
#'
#' Get the path to the saved country gasflare RDS
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#'
#' @return \code{character} the path to the world gasflare RDS
#'
#' @examples
#' \dontrun{
#'   getNlCtryGasFlaresRdsFnamePath(ctryCode="NGA")
#'   #returns path to the world RDS
#' }
#'
getNlCtryGasFlaresRdsFnamePath <- function(ctryCode)
{
  return(file.path(
    getNlDir("dirNlGasFlares"),
    paste0("gasflares_", ctryCode, ".rds")
  ))
}

######################## getNlGasFlaresRdsFnamePath ###################################

#' Get the path to the saved world gasflare RDS
#'
#' Get the path to the saved world gasflare RDS
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'   getNlGasFlaresRdsFnamePath(ctryCode="NGA")
#'   #returns path to the world RDS
#' }
#'
createNlGasFlares <- function()
{
  message(
    Sys.time(),
    ": removeGasFlaresMethod = OGP. Downloading DMSP-OLS gas flare country polygons. This will take a while"
  )
  
  wgs84 <- getCRS()
  
  gasFlareRdsFnamePath <- getNlGasFlaresRdsFnamePath()
  
  if (!file.exists(gasFlareRdsFnamePath))
  {
    #the page that lists all available nightlight files
    gasFlarePageHtml <-
      "https://www.ngdc.noaa.gov/eog/interest/gas_flares_countries_shapefiles.html"
    
    #the local name of the file once downloaded
    gasFlarePageLocalName <-
      file.path(getNlDir("dirNlTemp"), basename(gasFlarePageHtml))
    
    #if the file does not exist or is older than a day download it afresh
    #not working. download.file does not seem to update mtime
    if (!file.exists(gasFlarePageLocalName) ||
        (
          Sys.Date() - lubridate::date(file.mtime(gasFlarePageLocalName)) > lubridate::as.difftime(lubridate::period("1 day"))
        ))
    {
      utils::download.file(
        url = gasFlarePageHtml,
        destfile = gasFlarePageLocalName,
        method = "auto",
        extra = "-N"
      )
    }
    #else
    #  message(paste0(ntLtsPageHtml, " already downloaded"))
    
    #read in the html page
    gasFlarePage <- xml2::read_html(gasFlarePageLocalName)
    
    gasFlarePage <- rvest::html_nodes(gasFlarePage, "table tr td a")
    
    gasFlareRgxp <- paste0("Flares_.*\\.tgz")
    
    gasFlareNodes <-
      gasFlarePage[grep(pattern = gasFlareRgxp, x = gasFlarePage)]
    
    gasFlareUrls <- rvest::html_attr(gasFlareNodes, name = "href")
    
    for (gasFlareUrl in gasFlareUrls)
    {
      message(Sys.time(),
              ": Downloading gas flare polygon ",
              basename(gasFlareUrl))
      
      gfTgzLclFnamePath <-
        file.path(getNlDir("dirNlGasFlares"), basename(gasFlareUrl))
      
      if (!file.exists(gfTgzLclFnamePath))
        res <-
        try(utils::download.file(url = gasFlareUrl,
                                 destfile = gfTgzLclFnamePath,
                                 method = "auto"),
            TRUE)
      else
      {
        message(Sys.time(), ": Exists")
        
        res <- 0
      }
      
      if (res == 0)
      {
        message(Sys.time(), ": Extracting ", basename(gasFlareUrl))
        
        gfShpDirLclFnamePath <-
          tools::file_path_sans_ext(gfTgzLclFnamePath)
        
        if (!dir.exists(gfShpDirLclFnamePath))
        {
          dir.create(gfShpDirLclFnamePath)
          
          utils::untar(tarfile = gfTgzLclFnamePath, exdir = gfShpDirLclFnamePath)
        } else
        {
          message(Sys.time(),
                  ": ",
                  gfShpDirLclFnamePath,
                  " already exists")
        }
      }
      
    }
    
    if (res == 0)
    {
      #all shapefiles downloaded
      message(Sys.time(), ": Creating global gas flare shapefile")
      
      gfPoly <- list()
      
      message(Sys.time(), ": Reading in the downloaded gas flare polygons")
      
      for (gasFlareUrl in gasFlareUrls)
      {
        gfTgzLclFnamePath <-
          file.path(getNlDir("dirNlGasFlares"), basename(gasFlareUrl))
        
        gfShpDirLclFnamePath <-
          tools::file_path_sans_ext(x = gfTgzLclFnamePath)
        
        gfPoly <-
          append(gfPoly, rgdal::readOGR(dsn = gfShpDirLclFnamePath))
      }
      
      message(Sys.time(), ": mosaicing")
      
      gfPolyMosaic <- do.call(raster::bind, gfPoly)
      
      rm(gfPoly)
      
      raster::projection(gfPolyMosaic) <- sp::CRS(projargs = wgs84)
      
      #clean the geometries
      message(Sys.time(), ": cleaning the mosaiced polygon")
      
      gfPolyMosaic <- cleangeo::clgeo_Clean(gfPolyMosaic)
      
      message(Sys.time(), ": Saving mosaiced polygon to RDS")
      
      saveRDS(object = gfPolyMosaic, file = gasFlareRdsFnamePath)
      
      rm(gfPolyMosaic)
      
      gc()
    }
  } else
  {
    message("Gas flare RDS already exists")
    return(TRUE)
  }
}

######################## existsNlGasFlaresRds ###################################

#' Check if the world gasflare RDS exists
#'
#' Check if the world gasflare RDS exists
#'
#' @return \code{logical} if the gasflare rds exists
#'
#' @examples
#' \dontrun{
#'   existsNlGasFlaresRds()
#'   #returns TRUE/FALSE
#' }
#'
existsNlGasFlaresRds <- function()
{
  gasFlareRdsFnamePath <- getNlGasFlaresRdsFnamePath()
  
  return(file.exists(gasFlareRdsFnamePath))
}

######################## getNlGasFlaresRds ###################################

#' Returns the world gasflares polygon if it exists
#'
#' Returns the world gasflares polygon if it exists
#'
#' @return \code{spPolygons} the country gasflare polygon
#'
#' @examples
#' \dontrun{
#'   getNlGasFlaresRds()
#'   #returns mosaiced gasflare polygon
#' }
#'
getNlGasFlaresRds <- function()
{
  if (!existsNlGasFlaresRds())
    createNlGasFlares()
  
  readRDS(getNlGasFlaresRdsFnamePath())
}

######################## hasNlCtryGasFlares ###################################

#' Checks if a country has gasflares
#'
#' Checks if a country has gasflares
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#'
#' @param gadmVersion The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return \code{logical} whether the country has gasflares
#'
#' @examples
#' \dontrun{
#'   hasNlCtryGasFlares(ctryCode="NGA")
#'   #returns TRUE
#' }
#'
#' @export
hasNlCtryGasFlares <- function(ctryCode,
                               gadmVersion = pkgOptions("gadmVersion"),
                               gadmPolyType = pkgOptions("gadmPolyType"),
                               custPolyPath = NULL)
{
  ctryPolyAdm0 <- readCtryPolyAdmLayer(
    ctryCode = ctryCode,
    admLevel = unlist(
      getCtryShpLyrNames(
        ctryCodes = ctryCode,
        lyrNums = 0,
        gadmVersion = gadmVersion,
        gadmPolyType = gadmPolyType,
        custPolyPath = custPolyPath
      )
    ),
    gadmVersion = gadmVersion,
    custPolyPath = custPolyPath
  )
  gasFlarePoly <- getNlGasFlaresRds()
  
  return(any(rgeos::gIntersects(ctryPolyAdm0, gasFlarePoly, byid = TRUE)))
}

######################## hasNlCtryGasFlares ###################################

#' Checks if a country has gasflares
#'
#' Checks if a country has gasflares
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#'
#' @param gadmVersion The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return \code{logical} whether the country has gasflares
#'
#' @examples
#' \dontrun{
#'   hasNlCtryGasFlares(ctryCode="NGA")
#'   #returns TRUE
#' }
#'
createNlCtryGasFlares <- function(ctryCode,
                                  gadmVersion = pkgOptions("gadmVersion"),
                                  gadmPolyType = pkgOptions("gadmPolyType"),
                                  custPolyPath = NULL)
{
  if (!existsNlCtryGasFlaresRds(ctryCode))
  {
    ctryPolyAdm0 <- readCtryPolyAdmLayer(
      ctryCode = ctryCode,
      admLevel = unlist(
        getCtryShpLyrNames(
          ctryCodes = ctryCode,
          lyrNums = 0,
          gadmVersion = gadmVersion,
          gadmPolyType = gadmPolyType,
          custPolyPath = custPolyPath
        )
      ),
      gadmVersion = gadmVersion,
      custPolyPath = custPolyPath
    )
    gasFlarePoly <- getNlGasFlaresRds()
    
    if (hasNlCtryGasFlares(
      ctryCode = ctryCode,
      gadmVersion = gadmVersion,
      gadmPolyType = gadmPolyType,
      custPolyPath = custPolyPath
    ))
    {
      if (!file.exists(getNlCtryGasFlaresRdsFnamePath(ctryCode)))
      {
        message(Sys.time(),
                ": Creating country gas flare removal polygon. Approx time 2 mins")
        
        gasFlarePoly <- rgeos::gUnionCascaded(gasFlarePoly)
        
        ctryPolyAdm0GFRemoved <-
          rgeos::gDifference(spgeom1 = ctryPolyAdm0,
                             spgeom2 = gasFlarePoly,
                             byid = FALSE)
        
        saveRDS(object = ctryPolyAdm0GFRemoved, file = getNlCtryGasFlaresRdsFnamePath(ctryCode))
        
        ctryPolyAdm0 <- ctryPolyAdm0GFRemoved
      } else
      {
        ctryPolyAdm0 <- readRDS(getNlCtryGasFlaresRdsFnamePath(ctryCode))
      }
    } else
    {
      message(Sys.time(), ": No gas flares for: ", ctryCode)
    }
  } else
  {
    message(Sys.time(), ": Already exists")
  }
}


######################## existsNlCtryGasFlaresRds ###################################

#' Checks if the country gasflares rds exists
#'
#' Checks if the country gasflares rds exists
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#'
#' @return \code{logical} whether the country has gasflares
#'
#' @examples
#' \dontrun{
#'   existsNlCtryGasFlaresRds(ctryCode="NGA")
#'   #returns TRUE
#' }
#'
existsNlCtryGasFlaresRds <- function(ctryCode)
{
  gfNlCtryRdsFnamePath <- getNlCtryGasFlaresRdsFnamePath(ctryCode)
  
  return(file.exists(gfNlCtryRdsFnamePath))
}

######################## getNlCtryGasFlaresPoly ###################################

#' Creates and returns a country's gasflares polygon
#'
#' Creates and returns a country's gasflares polygon
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#'
#' @param gadmVersion The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return \code{spPolygons} the country gasflare polygon
#'
#' @examples
#' \dontrun{
#'   getNlCtryGasFlaresRds(ctryCode="NGA")
#'   #returns gasflare polygon
#' }
#'
getNlCtryGasFlaresRds <- function(ctryCode = ctryCode,
                                  gadmVersion = pkgOptions("gadmVersion"),
                                  gadmPolyType = pkgOptions("gadmPolyType"),
                                  custPolyPath = NULL)
{
  if (!existsNlCtryGasFlaresRds(ctryCode))
    createNlCtryGasFlares(
      ctryCode = ctryCode,
      gadmVersion = gadmVersion,
      gadmPolyType = gadmPolyType,
      custPolyPath = custPolyPath
    )
  
  if (existsNlCtryGasFlaresRds(ctryCode))
    res <- readRDS(file = getNlCtryGasFlaresRdsFnamePath(ctryCode))
  else
    res <- NULL
  
  return(res)
}

######################## getNlCtryGasFlaresPoly ###################################

#' Returns a country's gasflares polygon if it exists
#'
#' Returns a country's gasflares polygon if it exists
#'
#' @param ctryCode \code{character} The ctryCode of the country of interest
#'
#' @param gadmVersion The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return \code{spPolygons} the country gasflare polygon
#'
#' @examples
#' \dontrun{
#'   getNlCtryGasFlaresPoly(ctryCode="NGA")
#'   #returns gasflare polygon
#' }
#'
getNlCtryGasFlaresPoly <- function(ctryCode = ctryCode,
                                   gadmVersion = pkgOptions("gadmVersion"),
                                   gadmPolyType = pkgOptions("gadmPolyType"),
                                   custPolyPath = NULL)
{
  return(
    getNlCtryGasFlaresRds(
      ctryCode = ctryCode,
      gadmVersion = gadmVersion,
      gadmPolyType = gadmPolyType,
      custPolyPath = custPolyPath
    )
  )
}

removeGasFlares <- function(removeGasFlaresMethod, ctryPolyAdm0 = NULL,
                            ctryRastCropped = NULL, ctryCode, nlType, nlPeriod, 
                            gadmVersion, gadmPolyType, custPolyPath,
                            cropMaskMethod = pkgOptions("cropMaskMethod"))
{
  message(Sys.time(), ": Processing gas-flare/background removal")
  
  wgs84 <- getCRS()
  
  ctryMaskCropped <- NULL
  
  if (removeGasFlaresMethod == "OGP")
  {
    #read in the mosaiced gas flare polys

    #does this country require gas flare removal
    if (hasNlCtryGasFlares(
      ctryCode = ctryCode,
      gadmVersion = gadmVersion,
      gadmPolyType = gadmPolyType,
      custPolyPath = custPolyPath
    ))
    {
      message(Sys.time(), ": RGF: Processing gas flare removal")

      ctryPolyAdm0 <- getNlCtryGasFlaresPoly(
        ctryCode,
        gadmVersion = gadmVersion,
        gadmPolyType = gadmPolyType,
        custPolyPath = custPolyPath
      )

    } else
    {
      message(Sys.time(),
              ": RGF: Gas flare removal not required for: ",
              ctryCode)
    }
  }else if(removeGasFlaresMethod %in% c("OTM","VTM"))
  {
    if(missing(ctryPolyAdm0) || is.null(ctryPolyAdm0))
    {
      ctryPolyAdm0 <- readCtryPolyAdmLayer(
        ctryCode = ctryCode,
        admLevel = unlist(
          getCtryShpLyrNames(
            ctryCodes = ctryCode,
            lyrNums = 0,
            gadmVersion = gadmVersion,
            gadmPolyType = gadmPolyType,
            custPolyPath = custPolyPath
          )
        ),
        gadmVersion = gadmVersion,
        custPolyPath = custPolyPath
      )
    }
    
    maskNlType <- ifelse(removeGasFlaresMethod == "OTM", "OLS.Y", "VIIRS.Y")
    
    maskConfigName <- pkgOptions(paste0("configName_", maskNlType))
    
    maskExtension <- pkgOptions(paste0("extension_", maskNlType))
    
    allMaskNlPeriods <- getAvailableNlPeriods(nlTypes = maskNlType, configNames = maskConfigName)
    
    allMaskNlPeriods <- allMaskNlPeriods[[maskNlType]][[maskConfigName]]
    
    maskNlPeriod <- allMaskNlPeriods[length(allMaskNlPeriods)]
    
    #get the path we will use to save the cropped raster
    ctryGFMaskOutputFnamePath <-
      getCtryGFMaskOutputFnamePath(
        ctryCode = ctryCode,
        nlType = maskNlType,
        nlPeriod = maskNlPeriod,
        gadmVersion = gadmVersion,
        gadmPolyType = gadmPolyType,
        custPolyPath = custPolyPath
      )
    
    if (!file.exists(ctryGFMaskOutputFnamePath))
    {
      message(Sys.time(), ": RGF: Country mask raster not found. Creating")
      
      message(Sys.time(), ": RGF: Identified tile ", nlType, " ", maskNlPeriod)
      
      maskTileList <- getCtryTileList(ctryCodes = ctryCode, nlType = maskNlType)
      
      #download tiles
      #download all required tiles
      if (!downloadNlTiles(
        nlType = maskNlType,
        configName = maskConfigName,
        extension = maskExtension,
        multiTileStrategy = pkgOptions("multiTileStrategy"),
        nlPeriod = maskNlPeriod,
        tileList = maskTileList
      ))
      {
        message(Sys.time(),
                ": RGF: Something went wrong with the tile downloads. Aborting ...")
        
        return()
      }

      ctryMaskCropped <- NULL
      
      #crop tiles to country outline and save
      #mask file
      for (tile in maskTileList)
      {
        maskFilename <- getNlTileTifLclNamePath(
          nlType = maskNlType,
          configName = maskConfigName,
          extension = maskExtension,
          nlPeriod = maskNlPeriod,
          tileNum = tileName2Idx(tileName = tile,
                                 nlType = nlType)
        )
        
        maskTile <- raster::raster(x = maskFilename)

        raster::projection(maskTile) <- sp::CRS(projargs = wgs84)
        
        ctryPolyAdm0 <- sp::spTransform(ctryPolyAdm0, sp::CRS(SRS_string = wgs84))
        
        message(Sys.time(), ": RGF: Cropping the raster tiles ")
        
        #extTempCrop <- crop(rastTile, ctryExtent)
        
        message(Sys.time(), ": RGF: Cropping mask tile = ", tile)
        
        #we crop using raster for both rast and gdal
        #Note: gdalwarp is not used for cropping because the crop_to_cutline option
        #causes a shift in the cell locations which then affects the stats extracted.
        #A gdal-based crop to extent would be highly desirable for performance reasons
        #though so seeking other gdal-based workarounds
        tempMaskCrop <-
          raster::crop(x = maskTile,
                       y = ctryPolyAdm0,
                       progress = 'text')
        
        #will only be non-null if there are multiple tiles to be mosaiced i.e. a country
        #that straddles multiple tiles. So in the 2nd+ loop ctryRastCropped will not be null
        if (is.null(ctryMaskCropped))
        {
          ctryMaskCropped <- tempMaskCrop
        }
        else
        {
          #if we are here we are mosaicing multiple tiles
          ctryMaskMerged <- ctryMaskCropped
          
          ctryMaskCropped <- NULL
          
          #mosaic the tiles and store back in ctryRastCropped
          ctryMaskCropped <-
            raster::merge(x = ctryMaskMerged, y = tempMaskCrop)
          
          rm(ctryMaskMerged)
        }
        
        rm(tempMaskCrop)
      }
      
      #unload the tile from memory
      rm(maskTile)
      
      #release unused memory
      gc()
      
      message(Sys.time(), ": RGF: Masking the raster ")
      
      if (cropMaskMethod == "rast")
      {
        #RASTERIZE
        message(Sys.time(), ": RGF: Convert to raster object using fasterize ")
        
        
        # Use fasterize to mask
        # Convert sf object to raster
        
        ctryPolyAdm0_raster <-
          fasterize::fasterize(sf = sf::st_as_sf(ctryPolyAdm0),
                               raster = ctryMaskCropped)
        message(Sys.time(), ": RGF: Masking ")
        
        ctryRastCropped <-
          raster::mask(x = ctryMaskCropped, mask = ctryPolyAdm0_raster)
        
        message(Sys.time(), ": RGF: Writing the raster to disk ")
        
        raster::writeRaster(
          x = ctryMaskCropped,
          filename = ctryGFMaskOutputFnamePath,
          overwrite = TRUE,
          progress = "text"
        )
        
        message(Sys.time(), ": RGF: Crop and mask using rasterize ... Done")
        
      } else if (cropMaskMethod == "gdal")
      {
        message(Sys.time(), ": RGF: Crop and mask using gdalwarp ... ")
        
        #GDALWARP
        rstTmp <-
          file.path(getNlDir(dirName = "dirNlTemp"),
                    paste0(basename(tempfile()), ".tif"))
        
        message(Sys.time(),
                ": RGF: Writing merged raster to disk for gdalwarp masking")
        
        raster::writeRaster(x = ctryMaskCropped,
                            filename = rstTmp,
                            progress = "text")
        
        ctryRastCropped <- NULL
        
        gc()
        
        #the polygon will already correspond to having gas flares removed or not
        ctryPolyAdm0TmpDir <- tools::file_path_sans_ext(rstTmp)
        
        rgdal::writeOGR(
          obj = methods::as(ctryPolyAdm0, "SpatialPolygonsDataFrame"),
          dsn = ctryPolyAdm0TmpDir,
          driver = "ESRI Shapefile",
          layer = "GID_0_IDX"
        )
        
        outputFileVrt <-
          file.path(
            getNlDir(dirName = "dirNlTemp"),
            paste0(ctryCode, "_GFMask_", nlType, "_", nlPeriod, ".vrt"),
            fsep =
          )
        
        if (file.exists(outputFileVrt))
          file.remove(outputFileVrt)
        
        message(Sys.time(), ": RGF: gdalwarp masking to VRT")
        
        gdalUtils::gdalwarp(
          srcfile = rstTmp,
          dstfile = outputFileVrt,
          s_srs = wgs84,
          t_srs = wgs84,
          cutline = ctryPolyAdm0TmpDir,
          cl = "GID_0_IDX",
          multi = TRUE,
          wm = pkgOptions("gdalCacheMax"),
          wo = paste0("NUM_THREADS=",
                      pkgOptions("numThreads")),
          q = FALSE
        )
        
        message(Sys.time(), ": RGF: gdal_translate converting VRT to TIFF ")
        gdalUtils::gdal_translate(co = "compress=LZW",
                                  src_dataset = outputFileVrt,
                                  dst_dataset = ctryGFMaskOutputFnamePath)
        
        message(Sys.time(), ": RGF: Deleting the component rasters ")
        
        file.remove(rstTmp)
        file.remove(outputFileVrt)
        unlink(ctryPolyAdm0TmpDir,
               recursive = T,
               force = T)
        
        ctryMaskCropped <- raster::raster(ctryGFMaskOutputFnamePath)
        
        #GDALWARP
        message(Sys.time(), ": RGF: Crop and mask using gdalwarp ... DONE")
      }
    }else
    {
      maskFilename <- ctryGFMaskOutputFnamePath
      
      message(Sys.time(), ": RGF: ", maskFilename, " already exists")
      
      ctryMaskCropped <- raster::raster(x = maskFilename)
      
      raster::projection(x = ctryMaskCropped) <- sp::CRS(SRS_string = wgs84)
    }
    
    ctryMaskCropped <- raster::mask(x = ctryRastCropped, mask = ctryMaskCropped,
                                    maskvalue = 0, updatevalue = 0, updateNA = TRUE)
  }
  
  list("ctryGFPolyAdm0" = ctryPolyAdm0, "ctryGFMaskCropped" = ctryMaskCropped)
}