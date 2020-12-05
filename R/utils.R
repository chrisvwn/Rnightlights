######################## allValid ###################################

#' Check if a vector/list of values given is valid as per the given validation function
#'
#' Check if a vector/list of values given is valid as per the given
#'     validation function. The function will also print a warning
#'     showing the values that are invalid. One can stop the warning
#'     being printed by wrapping the function in the
#'     \code{suppressWarnings} function.
#'
#' @param testData The list/vector of values to validate
#'
#' @param testFun The validation function to test each value of testData against
#'
#' @param ... Other parameters to pass on to the testFun
#'
#' @return TRUE/FALSE
#'
#' @examples
#'
#' \donttest{
#'   Rnightlights:::allValid(c("KEZ", "UGA", "RWA", "TZA"), Rnightlights:::validCtryCodes)
#' }
#'
#' \donttest{
#'   Rnightlights:::allValid(c("2012", "2015"), validNlPeriods, "OLS.Y")
#' }
#'
allValid <- function(testData, testFun, ...)
{
  valid <-
    unlist(sapply(testData, function(x)
      eval(parse(text = "testFun(x, ...)"))))
  
  invalidData <- testData[!valid]
  
  if (length(invalidData) > 0)
    message(Sys.time(),
            ": Invalid data: ",
            paste0(invalidData, collapse = ", "))
  
  return(all(valid))
}

######################## getFreeRAM ###################################

#' Detect the amount of available RAM on the computer
#'
#' Detect the amount of available RAM on the computer for dynamic
#'     configuration of available memory
#'
#' @export
getFreeRAM <- function()
{
  if (Sys.info()[["sysname"]] == "Windows")
  {
    x <-
      system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    freeMem <- gsub("\r", "", x, fixed = TRUE)
    
    freeMem <- as.integer(freeMem)
    
  } else if (Sys.info()[["sysname"]] == "macOS")
  {
    freeBlocks <-
      system2("vm_stat | grep free | awk '{ print $3 }' | sed 's/\\.//'", stdout = TRUE)
    speculativeBlocks <-
      system2("vm_stat | grep speculative | awk '{ print $3 }' | sed 's/\\.//'")
    freeMem <- (freeBlocks + speculativeBlocks) * 4096
    
    freeMem <- as.integer(freeMem)
    
  } else if (Sys.info()[["sysname"]] == "Linux")
  {
    x <- system2('free', stdout = TRUE)
    
    x <- strsplit(x, "\\s+")
    
    freeCol <- grep("free", unlist(x[1]), fixed = T)
    cacheCol <- grep("buff", unlist(x[1]), fixed = T)
    
    free <- unlist(x[2])[freeCol]
    cache <- unlist(x[2])[cacheCol]
    
    freeMem <- as.integer(free) + as.integer(cache)
  } else
  {
    message("Cannot determine free RAM on your OS. Defaulting to conservative 1GB")
    
    freeMem <- 1048576
  }
  
  return(freeMem)
}

######################## getBatchBytes ###################################

#' Calculate the RAM to provide to the package for gdal calculations
#'
#' Calculate the RAM to provide to the package to calculate when
#'     pkgOption("extractMethod") == "gdal"
#'
#' @param freeRAM the amount of available RAM to consider
#'
#' @export
getBatchBytes <- function(freeRAM = pkgOptions("batchBytes"))
{
  sysFreeRAM <- getFreeRAM() * 2 ^ 10
  
  if (grepl("%", freeRAM))
  {
    freeRAM <- as.numeric(gsub("%", "", freeRAM))
    
    freeRAM <- freeRAM / 100
    
    freeRAM <- freeRAM * sysFreeRAM
  } else if (grepl("KB", freeRAM))
  {
    freeRAM <- as.numeric(gsub("KB", "", freeRAM))
    
    freeRAM <- freeRAM * 2 ^ 10
  } else if (grepl("MB", freeRAM))
  {
    freeRAM <- as.numeric(gsub("MB", "", freeRAM))
    
    freeRAM <- freeRAM * 2 ^ 20
  } else if (grepl("GB", freeRAM))
  {
    freeRAM <- as.numeric(gsub("GB", "", freeRAM))
    
    freeRAM <- freeRAM * 2 ^ 30
  }
  
  if (freeRAM > sysFreeRAM)
    freeRAM <- 0.1 * sysFreeRAM * 2 ^ 20
  
  return(freeRAM)
}

######################## nlCleanup ###################################

#' Clean up the environment after processing (Not yet implemented)
#'
#' Clean up the environment after processing (Not yet implemented)
#'
#' @param temp boolean should the temp folder be cleared?
#'
#' @param tileCache boolean should unnecessary tiles be cleared?
#'
#' @return NULL
#'
#' @examples
#'  \dontrun{
#'  Rnightlights:::nlCleanup()
#'  }
#'
nlCleanup <- function(temp = TRUE, tileCache = FALSE)
{
  #remove any global vars we created in .onLoad
  #suppressWarnings(rm(map, shpTopLyrName, wgs84, nlTiles, tilesSpPolysDFs))
  
  #message("Cleaning up the environment")
  
  #the destructor
  
  #del temp files used in this session in the nlTempDir
  unlink(list.files(getNlDir("dirNlTemp"), full.names = TRUE),
         recursive = TRUE,
         force = TRUE)
  
  #del temp dataPath directory if it was created
  #if(getNlDataPath() == tempdir())
  #  unlink(file.path(tempdir(), ".Rnightlights"), recursive = TRUE, force = TRUE)
}

######################## printCredits ###################################

#' Format credits to print to the console
#'
#' Format credits to print to the console
#'
#' @param credits character a single length character vector with newlineChar
#'     used to separate lines. Two consecutive newlineChara are used to
#'     put in a horizontal divider
#'
#' @param newLineChar character the character/sequence used to split the
#'     credits into new lines
#'
#' @param surroundChar character the character to use as a decoration
#'
#' @param horzPadding integer the number of blank spaces between text and
#'     the surrounding text horizontally
#'
#' @param vertPadding integer the number of blank spaces between text and
#'     the surrounding text vertically
#'
#' @param horzWidth integer the width of the decoration horizontally
#'
#' @param vertWidth integer the width of the decoration vertically
#'
#' @return character a formatted credits character vector
#'
#' @examples
#'   printCredits(credits="DMSP data collected by US Air Force Weather Agency|
#'   Image and data processing by NOAA's National Geophysical Data Center|
#'   (https://www.ngdc.noaa.gov/eog/download.html)||
#'   Maps distributed by GADM|(https://gadm.org)", newLineChar="|")
#'
#' @export
printCredits <-
  function(credits,
           newLineChar = "\n",
           surroundChar = "*",
           horzPadding = 1,
           vertPadding = 1,
           horzWidth = 3,
           vertWidth = 2)
  {
    width <- getOption("width")
    
    if (newLineChar != "\n")
      credits <- gsub(pattern = "\n",
                      replacement = "",
                      x = credits)
    
    credits <-
      unlist(strsplit(x = credits, split = newLineChar, fixed = T))
    
    sideFrame <- paste(rep(surroundChar, horzWidth), collapse = "")
    
    longestLine <- max(sapply(credits, nchar))
    
    fullHorzFrame <-
      paste(rep(surroundChar, longestLine + horzPadding * 2 + horzWidth * 2),
            collapse = "")
    
    emptyHorzFrame <-
      paste0(sideFrame, paste0(rep(" ", longestLine + horzPadding * 2), collapse =
                                 ""), sideFrame)
    
    header <- rep(fullHorzFrame, vertWidth)
    
    footer <- header

    header <- c(header, rep(emptyHorzFrame, vertPadding))
    
    footer <- c(rep(emptyHorzFrame, vertPadding), footer)
    
    mainBody <- sapply(credits, USE.NAMES = F, function(x) {
      leftPad <- floor((longestLine - nchar(x)) / 2)
      
      rightPad <-
        ifelse((longestLine - nchar(x)) %% 2 == 0, leftPad, leftPad + 1)
      
      leftSpace <- paste(rep(" ", leftPad + horzPadding), collapse = "")
      
      rightSpace <-
        paste(rep(" ", rightPad + horzPadding), collapse = "")
      
      # print((longestLine-nchar(x)))
      # print(paste0(leftPad, ":", rightPad))
      out <- if (!grepl(pattern = "^\\s*$", x = x))
      {
        paste0(sideFrame, leftSpace, x, rightSpace, sideFrame)
      } else
      {
        if (vertWidth > 0)
          c(rep(emptyHorzFrame, vertPadding), fullHorzFrame, rep(emptyHorzFrame, vertPadding))
        else
          fullHorzFrame
      }
      # message("'", leftSpace,"'")
      # message("'", x, "'")
      # message("'", rightSpace,"'")
      
      #paste0(out,"\n")
    })
    
    credits <-
      c(header,
        unlist(mainBody),
        footer)
    
    #cat(paste(credits, collapse = "\n"))
    
    credits <- sapply(credits, function(cred)
    {
      ws <- paste(rep(" ", floor((
        width - nchar(cred)
      ) / 2)), collapse = "")
      
      paste(ws, cred, sep = "", collapse = "")
    }, USE.NAMES = F)
    
    packageStartupMessage(paste(credits, collapse = "\n"))
  }

######################## writeNightlightsMap ###################################

writeNightlightsMap <- function()
{
  tplHead <- "
  MAP
    IMAGETYPE   PNG
    EXTENT	    -180 -90 180 90
    SIZE		    800 600
    IMAGECOLOR  \"#ffffffff\"
    TRANSPARENT ON
    SHAPEPATH	  <SHAPE_PATH>

    PROJECTION
      \"init=epsg:4326\"
    END

    OUTPUTFORMAT
      NAME png
      DRIVER 'GD/PNG'
      MIMETYPE 'image/png'
      IMAGEMODE RGBA
      EXTENSION 'png'
    END

    # OUTPUTFORMAT
    #   NAME      GEOTIFF
    #   DRIVER    GDAL/GTiff
    #   MIMETYPE  image/tiff
    #   IMAGEMODE RGBA
    #   EXTENSION tif
    # END

    WEB
      METADATA
        \"wms_title\" \"Nightlight Rasters\"
        \"wms_onlineresource\" \"http://localhost/cgi-bin/mapserv?map=<SHAPE_PATH>nightlights.map\"
        \"wms_description\" \"nightlights\"
        \"wms_name\" \"Nightlights\"
        \"wms_label\" \"Nightlights\"
        \"wms_srs\" \"EPSG:3857\"
        \"wms_extent\" \"-180 -90 180 90\"
        \"wms_formats\" \"GEOTIFF\"
        \"wms_enable_request\" \"*\"
      END
    END"
  
  tplTileIndexHead <- "
    LAYER
      NAME	<VAL_NAME>
      STATUS ON

      TILEINDEX	<VAL_NAME>_tindex.shp
      TILEITEM	\"location\"
      #OFFSITE 0 0 0

      TYPE RASTER
      #PROCESSING \"SCALE=AUTO\"

      PROJECTION
        \"init=epsg:4326\"
      END
      METADATA
        \"DESCRIPTION\" \"nightlights\"
        \"wms_title\" \"<VAL_NAME>\"
        \"wms_timeitem\"  \"nlPeriod\"
        \"wms_timeextent\"  \"201401/201412\"
      END"
  
  tplTileIndexClasses <-
    "CLASSITEM \"[pixel]\"
        CLASS
          NAME \"NODATA\"
          #EXPRESSION ([pixel] = <VAL_NODATA>)
          EXPRESSION ([pixel] <= 0)
          STYLE
            COLOR \"#000000\"
            OPACITY 0
          END
        END #END CLASS

        CLASS
          NAME DEC0
          EXPRESSION ([pixel] > 0 AND [pixel] < <VAL_DEC0>)

          STYLE
            OPACITY 100
            COLOR \"#000000\"
          END
        END

        CLASS
          NAME DEC1
          EXPRESSION ([pixel] >= <VAL_DEC0> AND [pixel] < <VAL_DEC1> )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC1>\"
          END
        END

        CLASS
          NAME DEC2
          EXPRESSION ([pixel] >= <VAL_DEC1>  AND [pixel] < <VAL_DEC2>  )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC2>\"
          END
        END

        CLASS
          NAME DEC3
          EXPRESSION ([pixel] >= <VAL_DEC2> AND [pixel] < <VAL_DEC3> )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC3>\"
          END
        END

        CLASS
          NAME DEC4
          EXPRESSION ([pixel] >= <VAL_DEC3> AND [pixel] < <VAL_DEC4> )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC4>\"
          END
        END

        CLASS
          NAME DEC5
          EXPRESSION ([pixel] >= <VAL_DEC4> AND [pixel] < <VAL_DEC5> )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC5>\"
          END
        END

        CLASS
          NAME DEC6
          EXPRESSION ([pixel] >= <VAL_DEC5> AND [pixel] < <VAL_DEC6> )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC6>\"
          END
        END

        CLASS
          NAME DEC7
          EXPRESSION ([pixel] >= <VAL_DEC6> AND [pixel] < <VAL_DEC7> )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC7>\"
          END
        END

        CLASS
          NAME DEC8
          EXPRESSION ([pixel] >= <VAL_DEC7> AND [pixel] < <VAL_DEC8>)

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC8>\"
          END
        END

        CLASS
          NAME DEC9
          EXPRESSION ([pixel] >= <VAL_DEC8> AND [pixel] < <VAL_DEC9>)

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC9>\"
          END
        END

        CLASS
          NAME DEC10
          EXPRESSION ([pixel] >= <VAL_DEC9> AND [pixel] < <VAL_DEC10> )

          STYLE
            OPACITY 100
            COLOR \"<COL_DEC10>\"
          END
        END

        CLASS
          NAME DEC11
          EXPRESSION ([pixel] > <VAL_DEC10>)

          STYLE
            OPACITY 100
            COLOR \"#FFFFFF\"
          END
        END
      END #END CLASSITEM"
  
  fList <- dir(
    path = getNlDir("dirRasterOutput"),
    pattern = "^NL_.*_VIIRS.M_.*_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP.tif$",
    full.names = T
  )
  
  layers <- NULL
  
  tplHead <-
    stringr::str_replace_all(tplHead,  "<SHAPE_PATH>", getNlDir("dirRasterOutput"))
  
  tplHead <-
    stringr::str_replace_all(tplHead,  "<SHAPE_PATH>", getNlDir("dirRasterOutput"))
  
  tplMap <- tplHead
  
  #gdaltindex NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP.shp NL_*_VIIRS.M_*_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP.tif
  gdalUtils::gdaltindex(
    index_file = file.path(
      path.expand(Rnightlights::getNlDir("dirRasterOutput")),
      "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP_tindex.shp"
    ),
    gdal_file = list.files(
      path = file.path(path.expand(
        Rnightlights::getNlDir("dirRasterOutput")
      )),
      pattern = "^NL_.*_VIIRS.M_.*_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP.tif$",
      full.names = T
    ),
    write_absolute_path = T
  )
  
  polyTindex <-
    rgdal::readOGR(
      dsn = file.path(
        path.expand(Rnightlights::getNlDir("dirRasterOutput")),
        "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP_tindex.shp"
      ),
      layer = "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP_tindex"
    )
  
  polyTindex@data$nlPeriod <-
    gsub(".*_(\\d{4,8})_.*", "\\1", polyTindex@data$location)
  
  rgdal::writeOGR(
    obj = polyTindex,
    dsn = file.path(
      path.expand(Rnightlights::getNlDir("dirRasterOutput")),
      "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP_tindex.shp"
    ),
    layer = "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP_tindex",
    driver = "ESRI Shapefile",
    overwrite_layer = TRUE
  )
  
  res <-
    system2(
      command = "shptree",
      args = file.path(
        path.expand(Rnightlights::getNlDir("dirRasterOutput")),
        "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP_tindex.shp"
      )
    )
  
  lyrName <-
    gsub("NL_[A-Z]{3}_",
         "NL_CTRYCODE_",
         tools::file_path_sans_ext(basename(fList[1])))
  lyrName <- gsub("_\\d{4,6}_", "_NLPERIOD_", lyrName)
  
  tplTileIndexHead <-
    stringr::str_replace_all(tplTileIndexHead,  "<VAL_NAME>", lyrName)
  
  gdalInfo <- rgdal::GDALinfo(fList[1], returnStats = FALSE)
  
  noDataVal <-
    format(eval(parse(text = (
      attr(gdalInfo, "df")$NoDataValue
    ))), scientific = F)
  
  tplLayers <-
    stringr::str_replace_all(tplTileIndexHead,
                             "<SHAPE_PATH>",
                             getNlDir("dirRasterOutput"))
  
  tplTileIndexClasses <-
    stringr::str_replace_all(tplTileIndexClasses,  "<VAL_NODATA>", noDataVal)
  
  deciles <-
    as.character(stats::quantile(x = -1:200, seq(
      from = 0, to = 1, by = 0.1
    )))
  
  colFunc <-
    grDevices::colorRampPalette(c("#000001", "slategray3", "slategray2", "slategray1", "white"))
  
  decileColors <- colFunc(11)
  
  #plot(rep(1,length(decileColors)),col=decileColors, pch=19,cex=2)
  
  for (i in seq_along(deciles))
  {
    decval <- paste0("<VAL_DEC", i - 1, ">")
    
    tplTileIndexClasses <-
      stringr::str_replace_all(tplTileIndexClasses,  decval, deciles[i])
    
    decCol <- paste0("<COL_DEC", i - 1, ">")
    
    tplTileIndexClasses <-
      stringr::str_replace_all(tplTileIndexClasses,  decCol, decileColors[i])
  }
  
  tplMap <- paste0(
    tplMap,
    "\n",
    tplTileIndexHead,
    "\n",
    tplTileIndexClasses,
    "\n    END #END LAYER",
    "\n  END #END MAP"
  )
  
  readr::write_file(tplMap, file.path(path.expand(
    Rnightlights::getNlDir("dirRasterOutput")
  ), "nightlights.map"))
}

######################## myquantile ###################################

myquantile <- function (x)
{
  vals <- raster::sampleRandom(x, Reduce("*", dim(x)) * 0.1)
  
  result <- stats::quantile(vals, c(0.02, 0.98), na.rm = T)
  
  return(result)
}
