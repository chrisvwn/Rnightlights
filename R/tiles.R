getNlFileRegex <-function(nlTypes = NULL, configNames = NULL, extensions = NULL, regexNum = NULL, nlPeriod = NULL, tileName = NULL, extension = NULL)
{
  allConfigNames <- getAllNlConfigNames(nlTypes = nlTypes, configNames = configNames, extensions = extensions)
  
  regex <- unique(if(regexNum == 1)
    allConfigNames$fileRegex1
  else if(regexNum == 2)
    allConfigNames$fileRegex2)
  
  if(!missing(nlPeriod) && !is.null(nlPeriod))
  {
    regex <- gsub(pattern = "{nlPeriod}", replacement = nlPeriod, x = regex, fixed = TRUE)
  } else
  {
    nlP <- unlist(getAllNlPeriods(nlTypes = nlTypes))[1]
    
    regex <- gsub(pattern = "{nlPeriod}", replacement = paste0("\\d{",nchar(nlP),"}"), x = regex, fixed = TRUE)
  }
  
  if(!missing(configNames) && !is.null(configNames))
  {
    regex <- gsub(pattern = "{configName}", replacement = tolower(configNames), x = regex, fixed = TRUE)
  } else
  {
    regex <- gsub(pattern = "{configName}", replacement = ".*", x = regex, fixed = TRUE)
  }
      
  if(!missing(tileName) && !is.null(tileName))
  {
    regex <- gsub(pattern = "{tileName}", replacement = tileName, x = regex, fixed = TRUE)
  } else
  {
    regex <- gsub(pattern = "{tileName}", replacement = "\\d{2,3}N\\d{3}[E|W]", x = regex, fixed = TRUE)
  }
  
  if(!missing(extension) && !is.null(extension))
  {
    regex <- gsub(pattern = "{extension}", replacement = extension, x = regex, fixed = TRUE)
  } else
  {
    regex <- gsub(pattern = "{extension}", replacement = ".*", x = regex, fixed = TRUE)
  }
  
  regex
}

######################## getAllNlConfigNames ###################################

#' Generate a list of all possible configNames for a given nlType
#'
#' Generate a list of all possible configNames for a given nlType
#'
#' @param nlTypes if present show only configNames matching the nlTypes
#'
#' @param configNames if present show only configNames matching the configNames
#' 
#' @param extensions if present show only extensions matching the extensions
#'
#' @param description \code{(character)} whether to print the long or
#'     short description. Possible values are:
#'     \itemize{
#'         \item NULL Do not show descriptions
#'         \item short Show short descriptions
#'         \item long Show long descriptions
#'     }
#'
#' @param printOut Whether to print the output. Output may be formatted
#'     to aid viewing.
#' 
#' @param summary Return a summarized view showing only abbrevs.
#'
#' @examples
#' getAllNlConfigNames("OLS.Y")
#'  #returns '"cf_cvg", "avg_vis", "stable_lights"'
#'
#' @export
getAllNlConfigNames <-
  function(nlTypes = NULL,
           configNames = NULL,
           extensions = NULL,
           description = "short",
           printOut = FALSE,
           summary = FALSE)
  {
    if (!is.null(nlTypes) && !all(validNlTypes(nlTypes = nlTypes)))
      stop(Sys.time(), ": Invalid nlTypes detected")
    
    if (!is.null(description) && !description %in% c("long", "short"))
      stop(Sys.time(),
           ": Invalid description. Only 'long' and 'short' allowed")
    
    allConfigNames <- list(
      list(
        nlType = "OLS.Y",
        productName = "dnb_composite_v4",
        configName = "stable_lights",
        extension = "cf_cvg",
        fileRegex1 = "F\\d{2}{nlPeriod}\\.v4\\.tar",
        fileRegex2 = "F\\d{2}{nlPeriod}\\.v4b_web\\.{extension}\\.tif\\.gz",
        shortDescription = "Cloud-free coverages",
        longDescription = "Tally of the total number of observations that went into
      each 30 arc second grid cell. This image can be used to identify
      areas with low numbers of observations where the quality is reduced.
      In some years there are areas with zero cloud-free observations in
      certain locations"
      ),
      list(
        nlType = "OLS.Y",
        productName = "dnb_composite_v4",
        configName = "stable_lights",
        extension = "avg_vis",
        fileRegex1 = "F\\d{2}{nlPeriod}\\.v4\\.tar",
        fileRegex2 = "F\\d{2}{nlPeriod}\\.v4b_web\\.{extension}\\.tif\\.gz",
        shortDescription = "Raw average visible band",
        longDescription = "Contains the average of the visible band digital number
      values with no further filtering. Data values range from 0-63. Areas
      with zero cloud-free observations are represented by the value 255."
      ),
      list(
        nlType = "OLS.Y",
        productName = "dnb_composite_v4",
        configName = "stable_lights",
        extension = "avg_vis",
        fileRegex1 = "F\\d{2}{nlPeriod}\\.v4\\.tar",
        fileRegex2 = "F\\d{2}{nlPeriod}\\.v4b_web\\.stable_lights\\.{extension}\\.tif",
        shortDescription = "Cleaned up avg_vis",
        longDescription = "Contains the lights from cities, towns, and other sites with persistent
          lighting, including gas flares. Ephemeral events, such as fires
          have been discarded. Then the background noise was identified and
          replaced with values of zero. Data values range from 1-63. Areas
          with zero cloud-free observations are represented by the value 255."
      ),
      list(
        nlType = "OLS.Y",
        productName = "dnb_composite_v4",
        configName = "avg_lights_x_pct",
        extension = "pct_lights",
        fileRegex1 = "F\\d{2}{nlPeriod}\\.v4b\\.avg_lights_x_pct\\.tgz",
        fileRegex2 = "F\\d{2}{nlPeriod}\\.v4b\\.{extension}\\.tif",
        shortDescription = "Percent detection freq",
        longDescription = "The percent frequency of light detections in cloud-free 
          observations. The number of light detections is not provided but can be 
          calculated by multiplying pct_lights by the number of cloud-free observations (cf_cvg). 
          Data values range from 0-100."
      ),
      list(
        nlType = "OLS.Y",
        productName = "dnb_composite_v4",
        configName = "avg_lights_x_pct",
        extension = "avg_lights_x_pct",
        fileRegex1 = "F\\d{2}{nlPeriod}\\.v4b\\.avg_lights_x_pct\\.tgz",
        fileRegex2 = "F\\d{2}{nlPeriod}\\.v4b\\.{extension}\\.tif",
        shortDescription = "Avg vis band x percent detection freq",
        longDescription = "Derived from the average visible band digital number(DN) of
           cloud-free light detections multiplied by the percent frequency
           of light detection. The inclusion of the percent frequency
           of detection term normalizes the resulting digital values for
           variations in the persistence of lighting. For instance, the
           value for a light only detected half the time is discounted by 50%.
           Note that this product contains detections from fires and a
           variable amount of background noise. This is the product used
           to infer gas flaring volumes from the nighttime lights."
      ),
      list(
        nlType = "VIIRS.D",
        productName = "daily_radiance",
        configName = "vcmcfg",
        extension = "rade9",
        fileRegex1 = "SVDNB_npp_d{nlPeriod}\\.d\\.{tileName}\\.{extension}\\.tif",
        fileRegex2 = "",
        shortDescription = "Stray Light Removed",
        longDescription = "Excludes any data impacted by stray light."
      ),
      list(
        nlType = "VIIRS.M",
        productName = "avg_rad_composite",
        configName = "vcmcfg",
        extension = "avg_rade9",
        fileRegex1 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.{extension}h\\.tif",
        shortDescription = "Cloud-free coverages",
        longDescription = ""
      ),
      list(
        nlType = "VIIRS.M",
        productName = "avg_rad_composite",
        configName = "vcmcfg",
        extension = "cf_cvg",
        fileRegex1 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "VCMCFG cloud-free tallies",
        longDescription = "Integer counts of the number of cloud-free coverages, 
        or observations, that went in to constructing the average radiance image.
        "
      ),
      list(
        nlType = "VIIRS.M",
        productName = "avg_rad_composite",
        configName = "vcmslcfg",
        extension = "avg_rade9",
        fileRegex1 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.{extension}h\\.tif",
        shortDescription = "VCMSL Avg Radiances",
        longDescription = ""
      ),
      list(
        nlType = "VIIRS.M",
        productName = "avg_rad_composite",
        configName = "vcmslcfg",
        extension = "cf_cvg",
        fileRegex1 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}01-{nlPeriod}\\d{2}_{tileName}_{configName}_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "VCMSL Cloud-Free Tallies",
        longDescription = "Integer counts of the number of cloud-free coverages, 
        or observations, that went in to constructing the average radiance image.
        "
      ),
      list(
        nlType = "VIIRS.Y",
        productName = "avg_rad_composite",
        configName = "vcm-ntl",
        extension = "avg_rade9",
        fileRegex1 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_{configName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_{configName}_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "VIIRS Cloud Mask - Nighttime Lights",
        longDescription = "This product contains the 'vcm' average, with background
      (non-lights) set to zero"
      ),
      list(
        nlType = "VIIRS.Y",
        productName = "avg_rad_composite",
        configName = "vcm-orm-ntl",
        extension = "avg_rade9",
        fileRegex1 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_vcm-orm-ntl_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "VIIRS Cloud Mask - Outlier Removed - Nighttime Light",
        longDescription = "This product contains the 'vcm-orm' average,
      with background (non-lights) set to zero."
      ),
      list(
        nlType = "VIIRS.Y",
        productName = "avg_rad_composite",
        configName = "vcm-orm",
        extension = "avg_rade9",
        fileRegex1 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_vcm-orm_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "VIIRS Cloud Mask - Outlier Removed",
        longDescription = "This product contains cloud-free average radiance values
      that have undergone an outlier removal process to filter out fires
      and other ephemeral lights."
      ),
      list(
        nlType = "VIIRS.Y",
        productName = "avg_rad_composite",
        configName = "vcm",
        extension = "avg_rade9",
        fileRegex1 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_vcm_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "Average Radiance",
        longDescription = ""
      ),
      list(
        nlType = "VIIRS.Y",
        productName = "avg_rad_composite",
        configName = "vcm",
        extension = "cf_cvg",
        fileRegex1 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_vcm_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "Cloud-Free Coverage Tallies",
        longDescription = "Integer counts of the number of cloud-free coverages, 
        or observations, that went in to constructing the average radiance image.
        Counts the total number of observations that went into each grid cell. 
        This image can be used to identify areas with low numbers of observations 
        where the quality is reduced. In some time periods there are areas
      with zero cloud-free observations in certain locations"
      ),
      list(
        nlType = "VIIRS.Y",
        productName = "avg_rad_composite",
        configName = "vcm",
        extension = "cvg",
        fileRegex1 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_v10_c\\d{12}\\.tgz",
        fileRegex2 = "SVDNB_npp_{nlPeriod}0101-{nlPeriod}1231_{tileName}_vcm_v10_c\\d{12}\\.{extension}\\.tif",
        shortDescription = "Coverage Tallies",
        longDescription = "Integer counts of the number of coverages or total 
        observations available (regardless of cloud-cover)"
      )
    )
    
    res <- do.call(rbind.data.frame, allConfigNames)
    
    rownames(res) <- NULL
    
    res$longDescription <-
      gsub("\\s*\\n\\s*", " ", res$longDescription)
    
    if (!is.null(nlTypes))
      res <- res[toupper(res$nlType) %in% toupper(nlTypes),]
    
    if (!is.null(configNames))
      res <- res[toupper(res$configName) %in% toupper(configNames),]
    
    if (!is.null(extensions))
      res <- res[toupper(res$extension) %in% toupper(extensions),]
    
    if (summary)
    {
      res <-
        stats::aggregate(configName ~ extension + nlType, res, paste, collapse = ", ")
    } else
    {
      if (is.null(description))
      {
        res$shortDescription <- NULL
        res$longDescription <- NULL
      } else if (description == "short")
      {
        res$longDescription <- NULL
      }
    }
    
    if(printOut)
    {
      if(!summary && !is.null(description) && description == "long")
      {
        longDesc <-
          strwrap(
            res$longDescription,
            width = getOption("width") - 50,
            simplify = F
          )
        
        res1 <- res
        res <- res[FALSE,]
        
        for(i in 1:nrow(res1))
        {
          lDesc <- unlist(longDesc[i])
          
          for(j in 1:length(lDesc))
          {
            if(j == 1)
              res <- rbind(res, stats::setNames(cbind(res1[i, -(ncol(res1))], lDesc[j]), names(res1)))
            else
              res <- rbind(res, stats::setNames(cbind(as.data.frame(matrix(data = rep(x = "", times = ncol(res1)-1), nrow = 1)), lDesc[j]), names(res1)))
          }
        }
      }
      
      print(x = res, width = getOption("width"), right=FALSE)
    }
    
    invisible(res)
  }

######################## validNlConfigName ###################################

#' Check if a configName is valid for a given nlType
#'
#' Check if a configName is valid for a given nlType
#'
#' @param configName the raster in use
#'
#' @param nlType types of nightlight to check
#'
#' @return logical a vector of logical values
#'
#' @examples
#' Rnightlights:::validNlConfigName("VCMCFG", "OLS.Y")
#'  #returns FALSE
#'
#' Rnightlights:::validNlConfigName("VCMCFG", "VIIRS.M")
#'  #returns TRUE
#'
validNlConfigName <- function(configName, nlType)
{
  toupper(configName) %in% toupper(unique(getAllNlConfigNames(nlTypes = nlType)$configName))
}

######################## validNlConfigExtension ###################################

#' Check if a configName is valid for a given nlType
#'
#' Check if a configName is valid for a given nlType
#'
#' @param configName character the config shortname of raster
#' 
#' @param extension character the extension of raster
#'
#' @param nlType types of nightlight to check
#'
#' @return logical a vector of logical values
#'
#' @examples
#' Rnightlights:::validNlConfigName("VCMCFG", "OLS.Y")
#'  #returns FALSE
#'
#' Rnightlights:::validNlConfigName("VCMCFG", "VIIRS.M")
#'  #returns TRUE
#'
validNlConfigExtension <- function(extension, configName, nlType)
{
  toupper(extension) %in% toupper(unique(getAllNlConfigNames(nlTypes = nlType, configNames = configName)$extension))
}

######################## downloadNlTiles ###################################

#' Download the listed tiles for a given nlType in a given nlPeriod
#'
#' Download the listed tiles for a given nlType in a given nlPeriod
#'
#' @param nlType character The nightlight type
#'
#' @param configName character the config short name of raster being processed
#' 
#' @param extension character the extension of raster being processed
#'
#' @param nlPeriod character The nlPeriod to process in the appropriate
#'     format
#'
#' @param tileList integer vector or character vector of digits containing
#'     valid tile numbers as obtained by tileName2Idx for VIIRS. Ignore for
#'     nlType=="OLS"
#'
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#'
#' @return TRUE/FALSE if the download was successful
#'
#' @examples
#' #download VIIRS tiles for "KEN" which are tiles 2 and 5 for the specified
#'     #time periods
#' \dontrun{
#' Rnightlights:::downloadNlTiles("VIIRS.M", "201401", c(2, 5))
#' }
#'
#' #same as above but getting the tileList automatically
#' \dontrun{
#' Rnightlights:::downloadNlTiles(nlType="VIIRS.M",
#'     nlPeriod="201401",
#'     tileList=Rnightlights:::getCtryTileList(ctryCodes="KEN",
#'         nlType="VIIRS.M")
#' )
#' }
#'
#' #returns TRUE if the download was successful or tile is cached locally
#'
downloadNlTiles <-
  function(nlType,
           configName = pkgOptions(paste0("configName_", nlType)),
           extension = extension,
           nlPeriod,
           tileList,
           multiTileStrategy = pkgOptions("multiTileStrategy"))
  {
    if (missing(nlType))
      stop(Sys.time(), ": Missing required parameter nlType")
    
    if (missing(nlPeriod))
      stop(Sys.time(), ": Missing required parameter nlPeriod")
    
    if (stringr::str_detect(nlType, "VIIRS") && missing(tileList))
      stop(Sys.time(), ": Missing required parameter tileList")
    
    if (!validNlTypes(nlType))
      stop(Sys.time(), ": Invalid nlType detected")
    
    if (!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
      stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
    
    if (stringr::str_detect(nlType, "VIIRS") &&
        !allValid(tileList, validNlTileNameVIIRS, nlType))
      stop(Sys.time(), ": Invalid tile detected")
    
    success <- TRUE
    
    #ensure we have all required tiles
    if (stringr::str_detect(nlType, "OLS"))
      success <- success && downloadNlTilesOLS(
        nlPeriod = nlPeriod,
        downloadMethod = pkgOptions("downloadMethod"),
        nlType = nlType,
        configName = configName,
        extension = extension,
        multiTileStrategy = multiTileStrategy
      )
    else if (stringr::str_detect(nlType, "VIIRS"))
    {
      i <- 1
      
      for (tile in tileList)
      {
        nlTile <- tileName2Idx(tile, nlType)
        
        message(Sys.time(), ": Downloading tile (", i, "/", length(tileList), "): ", paste0(nlPeriod, nlTile))
        
        #download tile
        success <-
          success &&
          downloadNlTilesVIIRS(
            nlPeriod = nlPeriod,
            tileNum = nlTile,
            nlType = nlType,
            configName = configName,
            extension = extension
          )
        
        i <- i + 1
      }
    }
    
    return (success)
  }

######################## getCtryTileList ###################################

#' Returns a list of VIIRS nightlight tiles that a country or countries
#'     intersects with
#'
#' Given a list of countries, this function will provide alist of VIIRS
#'     nightlight tiles that intersect with them. This helps in processing
#'     multiple countries by determining which nightlight tiles are required
#'     for processing by allowing the download of all required tiles before
#'     processing. Note all VIIRS_* nlTypes have the same nlTiles.
#'
#' @param ctryCodes character vector of country codes to process
#'
#' @param nlType character string The nlType of interest
#'
#' @param omitCountries countries to exclude from processing. This is
#'     helpful when the number of countries to exclude is smaller than
#'     the number to process e.g. when one wants to process all countries
#'     and exclude countries that take long to process i.e.
#'     omitCountries = "long"
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::getCtryTileList(ctryCodes=c("BDI", "KEN", "RWA", "TZA", "UGA"),
#'     nlType="VIIRS.M", omitCountries="none")
#'
#' #only 1 tile for OLS
#' Rnightlights:::getCtryTileList(ctryCodes=c("BDI", "KEN", "RWA", "TZA", "UGA"),
#'     nlType="OLS.Y", omitCountries="none")
#'     #returns "DUMMY"
#'
getCtryTileList <- function(ctryCodes, nlType, omitCountries = "none")
{
  if (missing(ctryCodes))
    stop(Sys.time(), ": Missing required parameter ctryCodes")
  
  if (missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if (!allValid(ctryCodes, validCtryCodes))
    stop(Sys.time(), ": Invalid ctryCode(s) detected")
  
  if (!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType: ", nlType)
  
  if (stringr::str_detect(nlType, "OLS"))
    ctryTiles <- "DUMMY"
  else if (stringr::str_detect(nlType, "VIIRS"))
    ctryTiles <-
      unlist(mapCtryPolyToTilesVIIRS(ctryCodes, omitCountries)$tiles)
  
  return (ctryTiles)
}

######################## getNlTiles ###################################

#' Create mapping of nightlight tiles
#'
#' Creates a data.frame mapping nightlight tile names to their vertice coordinates. This is used to
#'     identify nightlight tiles as well as to build a spatial polygons dataframe used to plot the tiles. OLS
#'     only has one tile for the whole world and thus has a dummy entry. OLS is included to
#'     prevent code duplication by writing separate functions for OLS.
#'
#' @param nlType the nlType of interest
#'
#' @return A data.frame of names of tiles and lon-lat coordinate of top-left corner of each
#'
#' @examples
#' Rnightlights:::getNlTiles("VIIRS.M")
#'
#' Rnightlights:::getNlTiles("OLS.Y")
#'
getNlTiles <- function(nlType)
{
  #6 nightlight tiles named by top-left geo coordinate numbered from left-right & top-bottom
  #creates columns as strings. createSpPolysDF converts relevant columns to numeric
  nlTiles <- data.frame(
    id = c(1, 1, 2, 3, 4, 5, 6),
    type = c("OLS", "VIIRS", "VIIRS", "VIIRS", "VIIRS", "VIIRS", "VIIRS"),
    name = c(
      "DUMMY",
      "75N180W",
      "75N060W",
      "75N060E",
      "00N180W",
      "00N060W",
      "00N060E"
    ),
    minx = c(-1,-180,-60, 60,-180,-60, 60),
    maxx = c(-1,-60, 60, 180,-60, 60, 180),
    miny = c(-1, 0, 0, 0,-75,-75,-75),
    maxy = c(-1, 75, 75, 75, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  
  if (!missing(nlType))
  {
    if (!validNlTypes(nlType))
      stop(Sys.time(), ": Invalid nlType")
    
    if (length(grep("VIIRS", nlType)) > 0)
      nlType <- "VIIRS"
    else if (length(grep("OLS", nlType)) > 0)
      nlType <- "OLS"
    
    nlTiles <- nlTiles[grepl(nlType, nlTiles$type), ]
  }
  
  return (nlTiles)
}

######################## createNlTilesSpPolysDF ###################################

#' Creates a tile Spatial Polygons DataFrame from the \code{"nlTiles"} dataframe
#'
#' Creates a Spatial Polygons DataFrame from the \code{"nlTiles"} dataframe of VIIRS tiles
#'
#' @return TRUE/FALSE
#'
#' @examples
#'   tilesSpPolysDFs <- Rnightlights:::createNlTilesSpPolysDF()
#'
createNlTilesSpPolysDF <- function()
{
  if (!exists("nlTiles"))
  {
    nlTiles <- getNlTiles(grep("VIIRS", getAllNlTypes(), value = T)[1])
  }
  
  wgs84 <- getCRS()
  
  #convert nlTiles min/max columns to numeric
  for (cIdx in grep("id|min|max", names(nlTiles)))
    nlTiles[, cIdx] <- as.numeric(as.character(nlTiles[, cIdx]))
  
  #create the empty obj to hold the data frame of tile PolygonsDataFrams
  tilesSpPolysDFs <- NULL
  
  #for each row in nlTiles
  for (i in seq_len(nrow(nlTiles)))
  {
    #grab the row containing the tile
    t <- nlTiles[i, ]
    
    #convert the tile x,y extents to a matrix
    #format is 2 cols x & y
    tMat <-
      as.matrix(cbind(
        rbind(t$minx, t$maxx, t$maxx, t$minx),
        rbind(t$maxy, t$maxy, t$miny, t$miny)
      ))
    
    #create a Polygon object from the tile extents matrix
    tPoly <- list(sp::Polygon(tMat))
    
    #create a Polygons object with a list of 1 polygon
    tPolys <- sp::Polygons(srl = tPoly, ID = i)
    
    #create a SpatialPolygons object with a list of 1 list of Polygons
    tilesSpPolys <- sp::SpatialPolygons(Srl = list(tPolys))
    
    #we assign the CRS at this point (note other objects cannot be assigned CRS)
    raster::projection(tilesSpPolys) <- sp::CRS(projargs = wgs84)
    
    #convert the SpatialPolygons object into a SpatialPolygonsDataFrame
    #tilesSpPolysDF <- methods::as(tilesSpPolys, "SpatialPolygonsDataFrame")
    
    #z used for plotCtryWithTilesVIIRS to color the tiles
    tilesSpPolysDF <-
      sp::SpatialPolygonsDataFrame(tilesSpPolys,
                                   data.frame(
                                     z = factor(i),
                                     name = nlTiles[i, "name"],
                                     row.names = i
                                   ))
    
    #append the SPDF into a dataframe of SPDFs
    if (is.null(tilesSpPolysDFs))
      tilesSpPolysDFs <- tilesSpPolysDF
    else
      tilesSpPolysDFs <-
      sp::rbind.SpatialPolygonsDataFrame(tilesSpPolysDFs, tilesSpPolysDF)
  }
  return (tilesSpPolysDFs)
}

######################## plotCtryWithTilesVIIRS ###################################

#' Plot a country boundary with the VIIRS tiles and world map
#'
#' Plot a country boundary as defined in the \pkg{rworldmap} package along
#'     with the VIIRS nightlight tiles for a visual inspection of the tiles
#'     required for download in order to process a country's nightlight
#'     data. Output corresponds to that of \code{getCtryNlTiles()}
#'
#'     It utilizes \code{rworldmap::rwmgetISO3()} to resolve country
#'     codes as well as names.
#'
#' @param ctry \code{character} the 3-letter ISO3 country code e.g. "KEN"
#'     or a common name of the country e.g. "Kenya" as found valid by
#'     \code{rworldmap::rwmgetISO3()}
#'
#' @return None
#'
#' @examples
#' #by ctryCode
#' \dontrun{plotCtryWithTilesVIIRS("KEN")}
#'
#' @export
plotCtryWithTilesVIIRS <- function(ctry)
{
  if (missing(ctry))
    stop(Sys.time(), ": You must supply a country code or index")
  
  if (!is.character(ctry))
    stop(Sys.time(),
         ": The parameter you supplied needs to be type character")
  
  wgs84 <- getCRS()
  
  #if the map variable does not exist
  map <- getWorldMap()
  
  #if the tiles spatial polygons dataframe does not exist create it
  if (!exists("tilesSpPolysDFs"))
    tilesSpPolysDFs <- createNlTilesSpPolysDF()
  
  ctryISO3 <- ctryNameToCode(ctry)
  
  if (is.na(ctryISO3))
    ctryName <- ctryCodeToName(ctry)
  
  if (is.na(ctryISO3) && !is.na(ctryName))
    ctryISO3 <- ctryNameToCode(ctryName)
  
  if (is.na(ctryISO3))
    stop(Sys.time(), ": Invalid ctryCode/Name ", ctry)
  
  #if ctryISO3 is empty then the country was not found
  if (is.na(ctryISO3) || ctryISO3 == "")
    return("Country code/name not found")
  
  #otherwise we have a valid country ISO3 code. get its index
  idx <- which(as.character(map@data$ISO3) == ctryISO3)
  
  #get the polygon that matches the index
  ctryPolys <- map@polygons[[idx]]
  
  #get the name of the polygon
  ctryPolyTitle <-
    paste0("VIIRS Nightlight Tiles Required for:\n",
           map@data$ADMIN[[idx]],
           " (",
           map@data$ISO3[[idx]],
           ")")
  
  #create a SpatialPolygons object with the list of Polygons
  ctrySpPolys <- sp::SpatialPolygons(Srl = list(ctryPolys))
  
  #set the coordinate reference system
  raster::projection(ctrySpPolys) <- sp::CRS(projargs = wgs84)
  
  #convert the spatial polygons to an SPsDF
  ctrySpPolysDF <-
    methods::as(ctrySpPolys, "SpatialPolygonsDataFrame")
  
  #set the 4 margins to 2 inches from the border to avoid boundary errors
  #graphics::par(mar=rep(2,4))
  
  #plot the tiles first
  #sp::plot(tilesSpPolysDFs, main=ctryPolyName)
  
  
  #plot the country on the same plot and fill with blue
  #sp::plot(ctrySpPolysDF, col="blue", add=TRUE)
  
  #get the extents of the SpatialPolygonsDataFrame. Used to draw a bounding box around the plotted country. Especially helpful for very small countries
  e <- raster::extent(ctrySpPolysDF)
  
  #draw back the boundaries by 10 units
  e@xmin <- e@xmin - 10
  e@xmax <- e@xmax + 10
  e@ymin <- e@ymin - 10
  e@ymax <- e@ymax + 10
  
  #convert the Extents object to a SpatialLines object for spplot to use
  extents <- methods::as(e, 'SpatialLines')
  
  #get a list of the intersecting tiles. Used to highlight tiles which intersect with plotted country
  tilesIntersected <-
    tileName2Idx(
      tileName = getTilesCtryIntersectVIIRS(map@data$ISO3[[idx]]),
      nlType = grep("VIIRS", getAllNlTypes(), value = TRUE)[1]
    )
  
  #create a list which serves as a subtitle showing the mapping of tile index to tile name
  tileIdxNames <-
    paste(tilesSpPolysDFs@data$z, tilesSpPolysDFs@data$name, sep = "=")
  
  #plot the map
  sp::spplot(
    tilesSpPolysDFs,
    #the tiles SPDF
    zcol = "z",
    #the col in the tiles SPDF which determines their color
    col.regions = as.vector(
      ifelse(
        seq_len(nrow(tilesSpPolysDFs)) %in% tilesIntersected,
        "lightblue",
        "transparent"
      )
    ),
    #colors of the tiles. intersected tiles are lightblue, otherwise transparent
    colorkey = FALSE,
    sp.layout = list(
      list(
        map,
        col = 'grey',
        fill = 'transparent',
        first = FALSE
      ),
      #plot the world map from rworldmap
      list(
        ctrySpPolysDF,
        col = 'black',
        fill = 'blue',
        first = FALSE
      ),
      #plot the selected country
      list('sp.lines', extents, col = 'green', lwd =
             2),
      #plot the bounding box
      list(
        'sp.text',
        sp::coordinates(tilesSpPolysDFs),
        seq_len(nrow(tilesSpPolysDFs)),
        col = 'black',
        cex = 2
      ) #label the tiles with their index numbers
    ),
    main = ctryPolyTitle,
    #the main title
    sub = tileIdxNames #the sub title
  )
  
  #ggplot(tilesSpPolysDFs, aes(x=long,y=lat))+geom_polygon(col="black", fill="white", alpha=0.5)#+geom_polygon(data=ctrySpPolysDF, alpha=0.5)
  #ggplot(ctrySpPolysDF, aes(x=long,y=lat, group=group))+geom_polygon(col="black", fill="white",alpha=0.5)
  
  #a <- spplot(tilesSpPolysDFs, main=map@polygons[[idx]]@ID)
  #b <- spplot(ctrySpPolysDF)
  
  #a+as.layer(b)
}

######################## mapAllCtryPolyToTilesVIIRS ###################################

#' Create a mapping of all countries and the tiles they intersect
#'
#' This is simply another name for mapCtryPolyToTilesVIIRS with ctryCodes="all"
#'
#' @param omitCountries A character vector or list of countries to leave
#' out when processing. Default is \code{"none"}
#'
#' @return None
#'
#' @examples
#' #no countries omitted
#' \dontrun{
#' tileMap <- Rnightlights:::mapAllCtryPolyToTilesVIIRS()
#' }
#'
#' #no countries omitted
#' \dontrun{
#' tileMap <- Rnightlights:::mapAllCtryPolyToTilesVIIRS(omitCountries="none")
#' }
#'
#' #include countries that take long to process
#' \dontrun{
#' tileMap <- Rnightlights:::mapAllCtryPolyToTilesVIIRS(omitCountries=c("error", "long"))
#' }
#'
mapAllCtryPolyToTilesVIIRS <-
  function(omitCountries = pkgOptions("omitCountries"))
  {
    mapCtryPolyToTilesVIIRS(ctryCodes = "all", omitCountries)
  }

######################## mapCtryPolyToTilesVIIRS ###################################

#' Create a mapping of all countries and the tiles they intersect
#'
#' Create a dataframe mapping each country in the rworldmap to the VIIRS
#'     tiles which they intersect with and thus need to be retrieved to
#'     process their nightlight imagery. Since some functions use this
#'     dataframe for long-term processing, omitCountries can eliminate
#'     countries that should be excluded from the list hence from processing.
#'     Countries can be added in the omitCountries function. Default is "none".
#'
#' @param ctryCodes A character vector or list of countries to map. Default
#'     is \code{"all"}
#' @param omitCountries A character vector or list of countries to leave out.
#'     Default is \code{"none"}
#'
#' @return ctryCodeTiles A data frame of countries and the tiles they
#'     intersect with as give by \code{getNlTiles}
#'
#' @examples
#' #map all countries
#' \dontrun{
#' tileMap <- Rnightlights:::mapCtryPolyToTilesVIIRS()
#' }
#'
#' #map all countries, no countries omitted
#' \dontrun{
#' tileMap <- Rnightlights:::mapCtryPolyToTilesVIIRS(ctryCodes="all", omitCountries="none")
#' }
#'
#' #will not omit countries that do not have polygons on GADM
#' \dontrun{
#' tileMap <- Rnightlights:::mapCtryPolyToTilesVIIRS(omitCountries=c("error", "missing"))
#' }
#'
mapCtryPolyToTilesVIIRS <-
  function(ctryCodes = "all",
           omitCountries = pkgOptions("omitCountries"))
  {
    #if ctryCodes is "all" otherwise consider ctryCodes to be a list of countries
    if (length(ctryCodes) == 1 && tolower(ctryCodes) == "all")
    {
      #get list of all country codes
      ctryCodes <- getAllNlCtryCodes(omitCountries)
    }
    
    #if the rworldmap::getMap() hasn't been loaded, load it
    map <- getWorldMap()
    
    wgs84 <- getCRS()
    
    #get the indices of the country polygons from the rworldmap
    ctryCodeIdx <- which(map@data$ISO3 %in% ctryCodes)
    
    ctryCodeTiles <- NULL
    
    #for each ctryCode index
    for (i in ctryCodeIdx)
    {
      #get the matching polygon
      ctryPolys <- map@polygons[[i]]
      
      #create a SpatialPolygons object with a list of 1 list of Polygons
      ctrySpPolys <- sp::SpatialPolygons(Srl = list(ctryPolys))
      
      #set the CRS
      raster::projection(ctrySpPolys) <- sp::CRS(SRS_string = wgs84)
      
      #convert the SpatialPolygons to a SpatialPolygonsDataFrame
      ctrySpPolysDF <-
        methods::as(ctrySpPolys, "SpatialPolygonsDataFrame")
      
      #find the tiles the SPDF intersects with and add to the list of tiles
      ctryCodeTiles <-
        rbind(ctryCodeTiles, list(tilesPolygonIntersectVIIRS(ctrySpPolys)))
    }
    
    #combine the ctryCodes and intersecting tile columns into a dataframe
    ctryCodeTiles <-
      as.data.frame(cbind(code = as.character(ctryCodes), tiles = ctryCodeTiles))
    
    #name the columns
    names(ctryCodeTiles) <- c("code", "tiles")
    
    #convert the code column to character since it is picked as factor
    ctryCodeTiles$code <- as.character(ctryCodeTiles$code)
    
    #return the data frame
    return(ctryCodeTiles)
  }

######################## getTilesCtryIntersectVIIRS ###################################

#' Get a list of tiles that a country polygon intersects with
#'
#' Create a dataframe mapping each country in the rworldmap to the VIIRS
#'     tiles which they intersect with and thus need to be retrieved to
#'     process their nightlight imagery. Since some functions use this
#'     dataframe for long-term processing, omitCountries can eliminate
#'     countries that should be excluded from the list hence from processing.
#'     Countries can be added in the omitCountries function.
#'     Default is "none".
#'
#' @param ctryCode The country's ISO3 code
#'
#' @return None
#'
#' @examples
#'
#' Rnightlights:::getTilesCtryIntersectVIIRS("KEN")
#'
getTilesCtryIntersectVIIRS <- function(ctryCode)
{
  if (missing(ctryCode))
    stop(Sys.time(), ": Missing equired parameter ctryCode")
  
  ctryCode <- as.character(ctryCode)
  
  if (!validCtryCodes(ctryCode))
  {
    warning("Invalid/Unknown ctryCode: ", ctryCode)
    return(NA)
  }
  
  ctryISO3 <- ctryCode
  
  map <- getWorldMap()
  
  wgs84 <- getCRS()
  
  #print(ctryISO3)
  
  if (is.na(ctryISO3) || ctryISO3 == "")
    return("Unknown country")
  
  idx <- which(map@data$ISO3 == ctryISO3)
  
  ctryCodeTiles <- NULL
  
  ctryPolys <- map@polygons[[idx]]
  
  #create a SpatialPolygons object with a list of 1 list of Polygons
  ctrySpPolys <- sp::SpatialPolygons(Srl = list(ctryPolys))
  
  raster::projection(ctrySpPolys) <- sp::CRS(SRS_string = wgs84)
  
  ctrySpPolysDF <-
    methods::as(ctrySpPolys, "SpatialPolygonsDataFrame")
  
  ctryCodeTiles <- tilesPolygonIntersectVIIRS(ctrySpPolys)
  
  #Plot for debug
  #plot(tilesSpPolysDFs, add=TRUE)
  #plot(ctrySpPolysDF, add=TRUE)
  
  return (ctryCodeTiles)
}

######################## validNlTileNameVIIRS ###################################

#' Check valid VIIRS nightlight tile name
#'
#' Check if a tile name is valid for a given VIIRS nightlight type.
#'
#' @param tileName the name of the tile
#'
#' @param nlType character the nlType
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::validNlTileNameVIIRS("00N060W", "VIIRS.M")
#'  #returns TRUE
#'
validNlTileNameVIIRS <- function(tileName, nlType)
{
  if (missing(tileName))
    stop(Sys.time(), ": Missing required parameter tileName")
  
  if (!is.character(tileName) ||
      is.null(tileName) || is.na(tileName) || tileName == "")
    stop(Sys.time(), ": Invalid tileName: ", tileName)
  
  if (length(tileName2Idx(tileName, nlType)) != 0)
    return(TRUE)
  else
    return(FALSE)
}

######################## tileName2Idx ###################################

#' Get the index of a tile given its name
#'
#' Get the index of a VIIRS tile as given by getNlTiles() given its name
#'
#' @param tileName name as given by getNlTiles()
#'
#' @param nlType the nlType of interest
#'
#' @return Integer index of the tile
#'
#' @examples
#' Rnightlights:::tileName2Idx("00N060W", "VIIRS.M")
#'
tileName2Idx <- function(tileName, nlType)
{
  if (missing(tileName))
    stop(Sys.time(), ": Missing required parameter tileName")
  
  if (missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if (!is.character(tileName) ||
      is.null(tileName) || is.na(tileName) || tileName == "")
    stop(Sys.time(), ": Invalid tileName: ", tileName)
  
  nlType <- toupper(nlType)
  
  tileName <- toupper(tileName)
  
  if (!exists("nlTiles"))
    nlTiles <- getNlTiles(nlType)
  
  return (which(nlTiles$name %in% tileName))
}

######################## tileIdx2Name ###################################

#' Get the name of a tile given its index
#'
#' Get the name of a VIIRS tile as given by getNlTiles() given its index
#'
#' @param tileNum index as given by getNlTiles()
#'
#' @param nlType the nlType of interest
#'
#' @return Character name of the tile
#'
#' @examples
#' Rnightlights:::tileName2Idx("00N060W", "VIIRS.M") #returns 6
#'
tileIdx2Name <- function(tileNum, nlType)
{
  if (missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if (missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if (!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType: ", nlType)
  
  if (!validNlTile(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  if (!exists("nlTiles"))
    nlTiles <- getNlTiles(nlType)
  
  nlType <- toupper(nlType)
  
  #return (nlTiles[tileNum, "name"])
  return(nlTiles[as.numeric(tileNum), "name"])
}

######################## tilesPolygonIntersectVIIRS ###################################

#' Get the list of VIIRS tiles that a polygon intersects with
#'
#' Get the list a VIIRS tiles that a polygon intersects with
#'
#' @param shpPolygon a SpatialPolygon or SpatialPolygons
#'
#' @return Character vector of the intersecting tiles as given by \code{getNlTiles}
#'
#' @examples
#' \dontrun{
#' #download shapefile if it doesn't exist
#' ctryShapefile <- Rnightlights:::dnldCtryPoly("KEN")
#'
#' #read in shapefile top layer
#' ctryPoly <- readCtryPolyAdmLayer("KEN",
#'     Rnightlights:::getCtryShpLyrNames("KEN",0))
#'
#' #get list of intersecting tiles
#' tileList <- Rnightlights:::tilesPolygonIntersectVIIRS(ctryPoly)
#' }
#'
tilesPolygonIntersectVIIRS <- function(shpPolygon)
{
  if (missing(shpPolygon))
    stop(Sys.time(), ": Missing required parameter shpPolygon")
  
  #given a polygon this function returns a list of the names of the viirs tiles
  #that it intersects with
  #Input: a Spatial Polygon e.g. from a loaded shapefile
  #Output: a character vector of tile names as given in the nlTiles dataframe
  
  if (!exists("tilesSpPolysDFs"))
  {
    tilesSpPolysDFs <- createNlTilesSpPolysDF()
  }
  
  if (!exists("nlTiles"))
    nlTiles <-
      getNlTiles(grep("VIIRS", getAllNlTypes(), value = TRUE)[1])
  
  wgs84 <- getCRS()
  
  raster::projection(shpPolygon) <- sp::CRS(projargs = wgs84)
  
  #init list to hold tile indices
  tileIdx <- NULL
  
  #loop through the 6 tile rows in our SpatialPolygonsDataFrame
  for (i in seq_len(nrow(tilesSpPolysDFs)))
  {
    #check whether the polygon intersects with the current tile
    tileIdx[i] <-
      rgeos::gIntersects(tilesSpPolysDFs[i, ], shpPolygon)
  }
  
  #return a list of tiles that intersected with the SpatialPolygon
  return (nlTiles[tileIdx, "name"])
}

######################## validNlTileNumVIIRS ###################################

#' Check valid tile number for a given VIIRS nightlight type
#'
#' Check if a tile number is valid for a given VIIRS nightlight type.
#'
#' @param nlTileNum the index of the tile
#'
#' @param nlType A character string of nlType
#'
#' @return TRUE/FALSE
#'
#' @examples
#' Rnightlights:::validNlTile("1", "VIIRS.M")
#'  #returns TRUE
#'
#' Rnightlights:::validNlTile("9", "VIIRS.D")
#'  #returns FALSE
#'
validNlTile <- function(nlTile, nlType)
{
  if (missing(nlTile))
    stop(Sys.time(), ": Missing parameter nlTile")
  
  if (missing(nlType))
    stop(Sys.time(), ": Missing parameter nlType")
  
  if (!class(nlTile) %in% c("character", "integer", "numeric") ||
      nlTile == "" ||
      length(nlTile) == 0)
    return(FALSE)

  if (!exists("nlTiles"))
    nlTiles <- getNlTiles(nlType)
  
  if(is.numeric(nlTile) || grepl(pattern = "^\\d+$", x = nlTile))
  {
    #if numeric assume it is a tileNum check index
    nlT <- as.numeric(nlTile)
    
    valid <- any(grepl(pattern = nlT, x = nlTiles$id))

  } else
  {
    #if not numeric assume it is a tileName and search
    nlT <- as.character(nlTile)
  
    valid <- any(grepl(pattern = nlT, x = nlTiles$name))
  }
  
  return(valid)
}