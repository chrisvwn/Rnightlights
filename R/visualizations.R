######################## plotData ###################################
#' 
#' Plots the ctryNlData
#' 
#' Plots the ctryNlData in various plot styles including line, box
#'     and density plot using ggplot2. If ctryData is not provided the subsequent
#'     parameters will need to be supplied so thta getCtryNlData can be used to 
#'     retrieve the data prior to plotting.
#'     
#' @param ctryData data.frame If provided the plot will use data from it rather 
#'     than retrieving it. If not available the follow parameters are required
#'     to retrieve the data using getCtryNlData.
#'     
#'     It is assumed that the ctryData data.frame is in the same format as the
#'     output from getCtryNlData i.e. admin cols first, an area column,
#'     followed by the stat columns. In future it may be possible to supply
#'     a custom data.frame and direct the function on which columns to find
#'     the particular data.
#' 
#' @param ctryCode \code{character} The ctryCode of interest
#'
#' @param admLevel \code{character} The country admin level in the given
#'     ctryCode at which to calculate stats
#'     
#' @param admLevelFilters \code{character} The admLevel members that are selected
#'     the rest will be filtered out
#'
#' @param nlType \code{character} The nlType of interest
#'
#' @param configName character the config shortname of raster being processed
#' 
#' @param extension character the extension of raster being processed
#'
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#'
#' @param multiTileMergeFun character The function to use to merge tiles
#'
#' @param removeGasFlaresMethod character The method to use to perform gas flare removal
#'     or NULL to disable
#'     
#' @param nlPeriod \code{character} The nlPeriod of interest
#'
#' @param nlStats the statistics to calculate. If not provided will calculate
#'     the stats specified in \code{pkgOptions("nlStats")}
#'
#' @param downloadMethod The method used to download polygons and rasters
#'
#' @param cropMaskMethod \code{character} Whether to use rasterize or
#'     gdal-based functions to crop and mask the country rasters
#'
#' @param extractMethod ("rast" or "gdal") Whether to use rasterize or
#'     gdal-based functions to crop and mask the country rasters
#'
#' @param gadmVersion The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return None
#'
#' @examples
#'
#' #calculate only the sum of monthly VIIRS radiances for Dec 2014 using gdal
#' #for both cropMask and extraction for KEN
#' \dontrun{
#' Rnightlights:::processNLCountry("KEN", "KEN_adm2", "VIIRS.M", "201412", "gdal", "gdal", "sum")
#' }
#'
#' @export
plotData <- function(ctryData, ctryCodes, admLevel, admLevelFilters,
                            nlType, configName, extension,
                            nlPeriodStart, nlPeriodEnd, gadmPolyVer, gadmPolyType, 
                            multiTileMergeStrategy, multiTileMergeFun,
                            removeGasFlaresMethod, nlStat, custPolyPath=NULL,
                            cropMaskMethod, scale=FALSE, graphType="line",
                            normArea=FALSE, plotly=TRUE)
{
  #for cran checks
  ..density.. <- NULL
  country <- NULL
  variable <- NULL
  value <- NULL
  
  
  if(missing(ctryData) || length(ctryData) == 0)
  {
    ctryAdmLevels <- getCtryPolyAdmLevelNames(ctryCode = ctryCodes,
                                              gadmVersion = gadmPolyVer,
                                              gadmPolyType = gadmPolyType, 
                                              custPolyPath = custPolyPath)
  } else
  {
    ctryCodes <- unique(ctryData[[1]])
    
    ctryAdmLevels <- names(ctryData)[1:(grep(pattern = "area", names(ctryData))-1)]
    
    nlType <- unique(stringr::str_extract(names(ctryData), "(VIIRS|OLS)\\..?"))
    
    nlType <- nlType[!is.na(nlType)]
  }
  
  if (missing(selectedAdmLevel) || is.null(selectedAdmLevel))
  {
    admLevel <- ctryAdmLevels[length(ctryAdmLevels)]
  }
  
  if (!exists("admLevel") ||
      is.null(admLevel) || length(admLevel) == 0 ||
      length(ctryCodes) > 1)
    admLevel <- "country"
  
  nlPeriodStart <- nlPeriodToDate(nlPeriod = nlPeriodStart, nlType = nlType)
  
  nlPeriodEnd <- nlPeriodToDate(nlPeriod = nlPeriodEnd, nlType = nlType)
  
  xLabel <- if (stringr::str_detect(nlType, "\\.D"))
    "Day"
  else if (stringr::str_detect(nlType, "\\.M"))
    "Month"
  else if (stringr::str_detect(nlType, "\\.Y"))
    "Year"
  
  if(!exists("ctryData") || is.null(ctryData))
  {
    ctryData <- getCtryNlData(ctryCode = ctryCodes, admLevel = admLevel,nlTypes = nlType, 
                              nlPeriods = nlRange(nlPeriodStart, nlPeriodEnd),
                              nlStats = nlStat, gadmVersion = gadmPolyVer, 
                              gadmPolyType = gadmPolyType, 
                              custPolyPath = custPolyPath,cropMaskMethod = cropMaskMethod,
                              downloadMethod = pkgOptions("downloadMethod"),
                              extractMethod = pkgOptions("extractMethod"),
                              configNames = configName, 
                              extensions = extension,
                              multiTileStrategy = multiTileMergeStrategy, 
                              multiTileMergeFun = multiTileMergeFun, 
                              removeGasFlaresMethod = removeGasFlaresMethod)
  }
  
  ctryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
  
  if (is.null(ctryData))
  {
    if (is.null(nlType))
      nlType <- "VIIRS.M"
    
    g <- ggplot2::ggplot(data =  data.frame()) +
      ggplot2::geom_point()
    
    if (grepl("\\.D", nlType))
      g <-
      g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d")
    else if (grepl("\\.M", nlType))
      g <-
      g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
    else if (grepl("\\.Y", nlType))
      g <-
      g + ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    
    g <-
      g + 
      ggplot2::xlim(nlPeriodStart, nlPeriodEnd) +
      ggplot2::ylim(0, 100) +
      ggplot2::labs(title = "Nightlight Radiance",
                    x = xLabel,
                    y = "Radiance (W.Sr^-1.cm^-2)"
      ) #bquote(paste("Radiance (W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, ")")))
    
    return(plotly::ggplotly(g))
  }
  
  ctryData <-
    subset(ctryData,
           variable >= nlPeriodStart & variable <= nlPeriodEnd)
  
  #filter by subsequent levels till lowest level
  if(!missing(admLevelFilters) && length(admLevelFilters) > 0)
    for (lvl in seq_along(ctryAdmLevels))
    {
      if (lvl == 1)
        next()
      
      #print(paste0("lvl:",lvl))
      admLevelFilter <- admLevelFilters[[ctryAdmLevels[lvl]]]
      
      if (length(admLevelFilter) > 0)
      {
        ctryData <-
          subset(ctryData, 
                 ctryData[[ctryAdmLevels[lvl]]] %in%
                   admLevelFilter)
      }
    }
  
  if (normArea)
    ctryData$value <- ctryData$value / ctryData$area_sq_km
  
  if (graphType == "boxplot")
  {
    if (length(ctryCodes) == 1)
    {
      g <-
        ggplot2::ggplot(data = ctryData,
                        ggplot2::aes(
                          x = ctryData[[admLevel]],
                          y = value,
                          col = ctryData[[admLevel]]
                        )) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1,
          vjust = 0.5
        )) + ggplot2::labs(col = admLevel) + ggplot2::facet_grid(
          lubridate::year(variable) ~ lubridate::month(
            x = variable,
            label = T,
            abbr = T
          )
        )
    }
    else
    {
      g <-
        ggplot2::ggplot(data = ctryData,
                        ggplot2::aes(
                          x = country,
                          y = value,
                          col = country
                        )) + ggplot2::theme(
                          axis.text.x = ggplot2::element_text(
                            angle = 45,
                            hjust = 1,
                            vjust = 0.5
                          ),
                          panel.spacing.x =  grid::unit(0.1, "lines")
                        ) + ggplot2::labs(col = admLevel) + ggplot2::facet_grid(
                          lubridate::year(variable) ~ lubridate::month(
                            x = variable,
                            label = T,
                            abbr = T
                          )
                        )
    }
    
    #ggplot2::ggplot(data = ctryData, ggplot2::aes(x = factor(variable), y = value, col = country)) + ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggplot2::labs(col = admLevel) + geom_boxplot() + facet_grid(country ~ .)
    
    g <- g + ggplot2::geom_boxplot()# +facet_grid(.~variable)
  }
  else if (graphType == "line")
  {
    if (length(ctryCodes) == 1)
    {
      #switched to data.table aggregation
      #ctryData <- stats::setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
      ctryData <-
        stats::setNames(ctryData[, value, by = list(ctryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
      
      g <-
        ggplot2::ggplot(data = ctryData, ggplot2::aes(
          x = variable,
          y = value,
          col = ctryData[[admLevel]]
        ))
      
      if (stringr::str_detect(nlType, "\\.D"))
        g <-
        g + ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d")
      else if (stringr::str_detect(nlType, "\\.M"))
        g <-
        g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
      else if (stringr::str_detect(nlType, "\\.Y"))
        g <-
        g + ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    }
    else
    {
      #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
      #switched to data.table aggregation
      ctryData <-
        stats::setNames(ctryData[, value, by = list(country, variable)], c("country", "variable", "value"))
      g <-
        ggplot2::ggplot(data = ctryData, ggplot2::aes(
          x = variable,
          y = value,
          col = country
        ))
      
      if (stringr::str_detect(nlType, "VIIRS"))
        g <-
        g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
    }
    
    g <- g + ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        vjust = 0.5
      )) +
      ggplot2::labs(col = admLevel)
  }
  else if (graphType == "histogram")
  {
    #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
    
    g <- ggplot2::ggplot(data = ctryData, ggplot2::aes(x = value))
    
    g <- g + ggplot2::geom_histogram(
      ggplot2::aes(y = ..density..),
      bins = 30,
      colour = "black",
      fill = "white"
    ) +
      # Overlay with transparent density plot
      ggplot2::geom_density(alpha = .2, fill = "#FF6666") +
      ggplot2::facet_grid(ctryData[[admLevel]] ~ lubridate::year(variable))
    
    
  }
  else
    return(NULL)
  
  if ("scale_y_log" %in% scale)
    g <- g + ggplot2::scale_y_log10()
  
  if ("scale_x_log" %in% scale)
    g <- g + ggplot2::scale_x_log10()
  
  nlStatSig <- nlStatSignature(nlStat = nlStat)
  
  plotTitle <-
    paste0(
      "Nightlight Radiance (",
      nlStatSig,
      ifelse(grepl(pattern = "na.rm", x = nlStatSig), "*", ""),
      ")"
    )
  
  if (normArea)
    g <- g + ggplot2::labs(
      title = plotTitle,
      x = xLabel,
      y = "Avg Rad (W.Sr^-1.cm^-1/Km^2)",
      #bquote(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, " / Km" ^{2}))
      caption = ifelse(
        grepl(pattern = "na.rm", x = nlStatSig),
        paste0(nlStat, " NAs removed"),
        paste0(nlStat, " NAs not removed")
      )
    ) #y=expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2})))
  else
    g <- g + ggplot2::labs(title = plotTitle,
                           x = xLabel,
                           y = "Total Rad (W.Sr^-1.cm^-2)") #bquote(~Total~Rad~W %.% Sr^{-1})%.%cm^{-2}) #y=expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2}))

  if(plotly)
  {
    g <- plotly::ggplotly(g)
    #p$elementId <- NULL
  }
  
  g
}

######################## hCluster ###################################
#'
#' @export
hCluster <- function(ctryData, ctryCodes, admLevel, nlType, nlStat, graphType, nlPeriodStart, nlPeriodEnd, normArea)
{
  if (is.null(ctryCodes) ||
      (length(ctryCodes) == 1 && admLevel == "country"))
    return()
  
  if (length(ctryCodes) > 1)
    admLevel <- "country"
  
  #return if the country doesn't have adm levels below country
  if (is.null(admLevel) || is.na(admLevel))
    return()
  
  #for cran checks
  value <- NULL
  
  #meltCtryData <- ctryNlDataMelted()
  
  meltCtryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
  
  if (is.null(ctryCodes) ||
      is.null(meltCtryData) || nrow(meltCtryData) < 3)
    return()
  
  if (normArea)
    meltCtryData$value <-
    meltCtryData$value / meltCtryData$area_sq_km
  
  #for cran check
  mean <- NULL
  variable <- NULL
  
  #aggMeltCtryData <- stats::aggregate(mean(value), by=list(eval(admLevel)+variable), data=meltCtryData, mean)
  aggMeltCtryData <-
    stats::setNames(meltCtryData[, list(mean(value, na.rm = TRUE)), by = list(meltCtryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
  
  dcastFormula <-
    paste(paste0("`", admLevel, "`", collapse = " + "),
          "~",
          paste("variable", collapse = " + "))
  
  unmeltCtryData <-
    data.table::dcast(aggMeltCtryData,
                      dcastFormula,
                      value.var = 'value',
                      aggregate = 'mean')
  
  lbls <- unmeltCtryData[[admLevel]]
  
  unmeltCtryData <- dplyr::select(unmeltCtryData, -1)
  
  d <- stats::dist(unmeltCtryData)
  
  h <- stats::hclust(d)
  
  h$labels <- lbls
  
  h
}

######################## plotHCluster ###################################
#'
#' @export
plotHCluster <- function(ctryData, ctryCodes, admLevel, nlType, nlStat,
                         graphType, normArea, numClusters)
{
  clusts <- hCluster(ctryData = ctryData,
                     ctryCodes = ctryCodes,
                     admLevel = admLevel,
                     nlType = nlType,
                     nlStat = nlStat,
                     normArea = normArea)
  
  if (is.null(clusts))
    return("Country has no adm levels")
  
  if (length(ctryCodes) > 1)
    admLevel <- "country"
  
  cutClusts <- stats::cutree(tree = clusts, k = numClusters)
  
  dendro <- stats::as.dendrogram(object = clusts)
  
  cbPalette <-
    c(
      "#999999",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )
  
    dendro <- dendextend::color_branches(dend = dendro,
                               k = numClusters,
                               col = rev(cbPalette[1:numClusters]),
                               groupLabels = T)
    
    dendro <- dendextend::color_labels(dend = dendro,
                                       k = numClusters,
                                       col = rev(cbPalette[1:numClusters]))
    
    dendro <- graphics::plot(dendro, horiz = FALSE, main = "")
  
    dendextend::rect.dendrogram(tree = dendro,
                                k = numClusters,
                                horiz = FALSE,
                                border = rev(cbPalette[1:numClusters]))
}

######################## plotPointsCluster ###################################
#'
#' @export
plotPointsCluster <- function(ctryData, ctryCodes, admLevel, nlType, nlStat,
                              graphType, nlPeriodStart, nlPeriodEnd, normArea,
                              numClusters, plotly=F)
{
  if (length(ctryCodes) < 1)
    return()
  
  clusts <- hCluster(ctryData = ctryData,
                     ctryCodes = ctryCodes, admLevel = admLevel, nlType = nlType,
                     nlStat = nlStat, normArea = normArea)
  
  if (is.null(clusts))
    return()
  
  #return if the country doesn't have adm levels below country
  if (is.null(admLevel))
    return()
  
  
  if (length(ctryCodes) > 1)
    admLevel <- "country"
  
  #for cran checks
  value <- NULL
  
  meltCtryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
  
  if (normArea)
    meltCtryData$value <-
    meltCtryData$value / meltCtryData$area_sq_km
  
  cutClusts <- stats::cutree(clusts, k = numClusters)
  
  #ctryAvg <- aggregate(value ~ admLevel, data=meltCtryData, mean)
  ctryAvg <-
    stats::setNames(meltCtryData[, mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]])], c(admLevel, "value"))
  
  cbPalette <-
    c(
      "#999999",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )
  
  clusters <- as.factor(cutClusts)
  
  g <- ggplot2::ggplot(data = ctryAvg,
                       ggplot2::aes(x = ctryAvg[[admLevel]],
                                    y = value, col = clusters)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1,
      vjust = 0.5
    )) +
    ggplot2::scale_colour_manual(values = cbPalette) +
    ggplot2::xlab(admLevel) +
    ggplot2::ylab("Radiance")
  
  if(plotly)
  {
    g <- plotly::ggplotly(g)
    #g$elementId <- NULL #bug was fixed
  }
  
  g
}

######################## mapHCluster ###################################
#'
#' @export
mapHCluster <- function(ctryData, ctryCodes, admLevel, nlType, nlStat, graphType,
                        nlPeriodStart, nlPeriodEnd, normArea, numClusters)
{
  if (is.null(ctryCodes))
    return()
  
  if (length(ctryCodes) > 1)
    admLevel <- "country"

  #for cran checks
  value <- NULL
  variable <- NULL
  
  wgs84 <- getCRS()
  
  # if (length(ctryCodes) != 1)
  # {
  #   renderText("Please select only one country/region")
  #   return()
  # }
  
  #print("drawing leaflet cluster")
  
  clusts <- hCluster(ctryData = ctryData,
                     ctryCodes = ctryCodes, admLevel = admLevel, nlType = nlType,
                     nlStat = nlStat, normArea = normArea)
  
  if (is.null(clusts))
    return()
  
  cutClusts <- stats::cutree(clusts, k = numClusters)
  
  #admLevel <- input$radioAdmLevel #unlist(ctryAdmLevels())[2]
  
  meltCtryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
  
  
  if (normArea)
    meltCtryData$value <-
    meltCtryData$value / meltCtryData$area_sq_km
  
  
  #map <- leaflet::leaflet(data=ctryPoly0) %>%
  map <- leaflet::leaflet()
  
  map <- leaflet::addTiles(map = map, urlTemplate = "http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png")
  #leaflet::addTiles() %>%
  # leaflet::addWMSTiles(layerId="nlRaster",
  #                      baseUrl = "http://localhost/cgi-bin/mapserv?map=test.map",
  #                      layers = "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP",
  #                      options = leaflet::WMSTileOptions(format = "image/png",
  #                                                        transparent = TRUE,
  #                                                        opacity=0.8))
  
  ctryPoly0 <-
    Rnightlights::readCtryPolyAdmLayer(
      ctryCode = ctryCodes[1],
      admLevel = unlist(Rnightlights::getCtryShpLyrNames(ctryCodes[1], 0))
    )
  
  if (length(ctryCodes) > 1) {
    for (ctryCode in ctryCodes[2:length(ctryCodes)])
    {
      ctryPoly0 <- sp::rbind.SpatialPolygonsDataFrame(
        ctryPoly0,
        Rnightlights::readCtryPolyAdmLayer(
          ctryCode = ctryCode,
          admLevel = unlist(Rnightlights::getCtryShpLyrNames(ctryCode, 0))
        ),
        makeUniqueIDs = T
      )
      
    }
    
    lvlCtryData <-
      stats::setNames(meltCtryData[, mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]])], c(admLevel, "value"))
    
    lvlCtryData[["rank"]] <-
      with(lvlCtryData, rank(value, ties.method = 'first'))
    
    cbPalette <-
      c(
        "#999999",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
      )
    
    pal <- cbPalette
    
    ctryPoly0 <- sp::spTransform(ctryPoly0, wgs84)
    
    mapLabels <- sprintf(
      paste0(
        "%s:%s",
        "<br/>Cluster: %s",
        "<br/>Rad:%s",
        "<br/>Rank: %s/%s"
      ),
      admLevel,
      lvlCtryData[[1]],
      cutClusts,
      format(
        lvlCtryData[[2]],
        scientific = T,
        digits = 2
      ),
      lvlCtryData[["rank"]],
      nrow(lvlCtryData)
    )
    
    mapLabels <- lapply(X = mapLabels, FUN = htmltools::HTML)
    
    map <- leaflet::addPolygons(
      map = map,
      data = ctryPoly0,
      #layerId = "country",
      fill = TRUE,
      fillColor = pal[cutClusts],
      fillOpacity = 0.9,
      stroke = TRUE,
      weight = 1,
      smoothFactor = 0.7,
      opacity = 1,
      color = "white",
      #dashArray = "5",
      group = admLevel,
      highlight = leaflet::highlightOptions(
        weight = 5,
        #color = "#666",
        #dashArray = "",
        fillOpacity = 0,
        bringToFront = TRUE
      ),
      label = mapLabels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    )
  } else
  {
    map <- leaflet::addPolygons(
      map = map,
      data = ctryPoly0,
      layerId = ctryCodes,
      fill = FALSE,
      fillColor = "#fefe40",
      stroke = TRUE,
      weight = 4,
      smoothFactor = 0.7,
      opacity = 1,
      color = "white",
      #dashArray = "5",
      group = "country"
    )
    
    lvlCtryData <-
      stats::setNames(meltCtryData[, mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]])], c(admLevel, "value"))
    
    lvlCtryData[["rank"]] <-
      with(lvlCtryData, rank(value, ties.method = 'first'))
    
    cbPalette <-
      c(
        "#999999",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
      )
    
    pal <- cbPalette
    
    #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
    ctryPoly <- Rnightlights::readCtryPolyAdmLayer(ctryCodes,
                                                   unlist(Rnightlights::getCtryShpLyrNames(ctryCodes, 1)))
    
    ctryPoly <- sp::spTransform(ctryPoly, wgs84)
    
    mapLabels <- sprintf(
      paste0(
        "%s:%s",
        "<br/>Cluster: %s",
        "<br/>Rad:%s",
        "<br/>Rank: %s/%s"
      ),
      admLevel,
      lvlCtryData[[1]],
      cutClusts,
      format(
        lvlCtryData[[2]],
        scientific = T,
        digits = 2
      ),
      lvlCtryData[["rank"]],
      nrow(lvlCtryData)
    )
    
    mapLabels <- lapply(X = mapLabels, FUN = htmltools::HTML)
    
    map <- leaflet::addPolygons(
      map = map,
      data = ctryPoly,
      layerId = as.character(ctryPoly@data[, 'NAME_1']),
      fill = TRUE,
      fillColor = pal[cutClusts],
      fillOpacity = 0.9,
      stroke = TRUE,
      weight = 1,
      smoothFactor = 0.7,
      opacity = 1,
      color = "white",
      #dashArray = "5",
      group = admLevel,
      highlight = leaflet::highlightOptions(
        weight = 5,
        #color = "#666",
        #dashArray = "",
        fillOpacity = 0,
        bringToFront = TRUE
      ),
      label = mapLabels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    )
  }
  
  map <- leaflet::addLayersControl(map = map, overlayGroups = admLevel)
  
  map <- leaflet::addLegend(
    map = map,
    position = "bottomright",
    colors = pal[unique(cutClusts)],
    labels = unique(cutClusts),
    #title = "Nightlight percentiles",
    title = "clusters",
    opacity = 1
  )
  
  map
}

######################## renderPlot plotTSDecomposed ###################################
#'
#' @export
plotTSDecomposed <- function(ctryData, ctryCodes, admLevel, nlType, nlStat, normArea)
{
  if (length(ctryCodes) < 1)
    return()
  
  #return if the country doesn't have adm levels below country
  if (is.null(admLevel) || admLevel == "")
    return()
  
  if (length(ctryCodes) > 1)
    admLevel <- "country"
  
  #meltCtryData <- ctryNlDataMeltedLvl2()
  meltCtryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
  
  if (nrow(meltCtryData) < 2)
    return()
  
  if (normArea)
    meltCtryData$value <-
    meltCtryData$value / meltCtryData$area_sq_km
  
  ctryAvg <- meltCtryData
  #ctryAvg <- aggregate(value ~ country, data=meltCtryData, mean)
  #ctryAvg <- stats::setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[make.names(admLevel)]], variable)], c(admLevel, "variable", "value"))
  
  if (stringr::str_detect(nlType, "\\.D"))
    fmt <- "%Y-%M-%d"
  else if (stringr::str_detect(nlType, "\\.M"))
    fmt <- "%Y-%M-%d"
  else if (stringr::str_detect(nlType, "\\.Y"))
    fmt <- "%Y"
  
  minDate <- as.character(min(ctryAvg$variable))
  maxDate <- as.character(max(ctryAvg$variable))
  
  if (stringr::str_detect(nlType, "\\.Y") &&
      stringr::str_detect(minDate, "\\d+$"))
  {
    minDate <- paste0(minDate, "-01-01")
    maxDate <- paste0(maxDate, "-01-01")
    freq <- 2
  }
  
  startYear <- lubridate::year(as.Date(minDate, fmt))
  endYear <- lubridate::year(as.Date(maxDate, fmt))
  
  if (startYear == endYear)
    stop(Sys.time(), ": Only 1 data point (year) in the dataset")
  
  tsStart <- c(startYear)
  tsEnd <- c(endYear)
  
  if (stringr::str_detect(nlType, "\\.D|\\.M"))
  {
    startMonth <-
      lubridate::month(lubridate::ymd(min(ctryAvg$variable)))
    endMonth <-
      lubridate::month(lubridate::ymd(max(ctryAvg$variable)))
    
    tsStart <- c(tsStart, startMonth)
    tsEnd <- c(tsEnd, endMonth)
    freq <- 12
  }
  
  if (stringr::str_detect(nlType, "\\.D"))
  {
    startDay <- lubridate::day(lubridate::ymd(max(ctryAvg$variable)))
    endDay <-
      lubridate::month(lubridate::ymd(max(ctryAvg$variable)))
    
    tsStart <- c(tsStart, startDay)
    tsEnd <- c(tsEnd, endDay)
    freq <- 7
  }
  
  ctryDataTS <-
    stats::ts(
      ctryAvg$value,
      start = tsStart,
      end = tsEnd,
      frequency = freq
    )
  
  ctryDataTScomponents <- stats::decompose(ctryDataTS)
  #g <- ggplot2::autoplot(ctryDataTScomponents)
  
  graphics::plot(ctryDataTScomponents)
}

######################## plotYearly ###################################
#'
#' @export
plotYearly <- function(ctryData, ctryCodes, admLevel, nlType, nlStat, normArea, scale)
{
  if (is.null(ctryCodes))
    return(NULL)
  
  ctryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
  
  if (is.null(ctryCodes) || is.null(ctryData))
    return()
  
  #admLevel <- unlist(Rnightlights:::searchAdmLevel(ctryCodes))[1]
  
  #print(paste0("admLevel:", admLevel))
  
  if (!exists("admLevel") ||
      is.null(admLevel) || length(admLevel) == 0)
    admLevel <- "country"
  
  #for cran checks
  country <- NULL
  value <- NULL
  
  ctryData$year <-
    lubridate::year(as.Date(as.character(ctryData$variable), "%Y"))
  
  lstAggBy <- paste0("list(", admLevel, ", variable")
  
  lstAggBy <- paste0(lstAggBy, ", as.factor(year)")
  aggNames <- "year"
  
  if (stringr::str_detect(nlType, "\\.M"))
  {
    ctryData$month <-
      lubridate::month(as.Date(as.character(ctryData$variable)))
    lstAggBy <- paste0(lstAggBy, ", as.factor(month)")
    aggNames <- c(aggNames, "month")
  }
  
  lstAggBy <- paste0(lstAggBy, ")")
  
  if (stringr::str_detect(nlType, "\\.D"))
  {
    ctryData$day <-
      lubridate::day(as.Date(as.character(ctryData$variable)))
    lstAggBy <- paste0(lstAggBy, ", as.factor(day)")
    aggNames <- c(aggNames, "day")
  }
  
  #print(paste0("ctrydata nrow:", nrow(ctryData)))
  
  if (normArea)
    ctryData$value <- (ctryData$value) / ctryData$area_sq_km
  
  if (length(ctryCodes) == 1)
  {
    #switched to data.table aggregation
    #ctryData <- stats::setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
    ctryData <- stats::setNames(ctryData[, list(mean(value, na.rm = TRUE)),
                                         by = eval(parse(text = lstAggBy))],
                                c(admLevel, "variable", aggNames, "value"))
    g <- ggplot2::ggplot(ctryData,
                         ggplot2::aes(
                           x = eval(parse(text = aggNames[length(aggNames)])),
                           y = value,
                           col = year,
                           group = eval(parse(text=admLevel))
                         )) +
      ggplot2::geom_line(alpha = 0.3) + ggplot2::geom_point() +
      ggplot2::geom_smooth(
        ggplot2::aes(group = 1),
        method = "loess",
        weight = 1,
        alpha = 0.2,
        lty = 'twodash'
      ) #+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #+ labs(col=year)
  }
  else
  {
    #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
    #switched to data.table aggregation
    ctryData <-
      stats::setNames(ctryData[, mean(value, na.rm = TRUE), by = eval(parse(text =
                                                                              lstAggBy))],
                      c(admLevel, "variable", aggNames, "value"))
    #g <- ggplot2::ggplot(data=ctryData, ggplot2::aes(x=variable, y=value, col=country, group=year))
    g <-
      ggplot2::ggplot(
        ctryData,
        ggplot2::aes(
          x = eval(parse(text = aggNames[length(aggNames)])),
          y = value,
          col = country,
          shape = year,
          group = interaction(country, year)
        )
      ) +
      ggplot2::geom_line(lwd = .5, alpha = 0.3) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(
        ggplot2::aes(group = country),
        method = "loess",
        weight = 1,
        alpha = 0.2,
        lty = 'twodash'
      )
  }
  
  if ("scale_y_log" %in% scale)
    g <- g + ggplot2::scale_y_log10()
  
  if ("scale_x_log" %in% scale)
    g <- g + ggplot2::scale_x_log10()
  
  if (normArea)
    g <-
    g + ggplot2::labs(title = "Radiance", x = "Time", y = "Rad (W.Sr^-1.cm^-2/Km^2)")
    #y=expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2}))
  else
    g <-
    g + ggplot2::labs(title = "Radiance", x = "Time", y = "Rad (W.Sr^-1.cm^-2")
    #y=expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2})
  
  #plotly::ggplotly(g)
  g
}

######################## dtDataset ###################################
#'
#' @export
dtDataset <- function(ctryData, nlStat, nlType, nlPeriodStart, nlPeriodEnd, 
                      polySrc, polyVer, polyType, configName, extension,
                      multiTileMergeStrategy, multiTileMergeFun, removeGasFlaresMethod)
{
  DT::datatable({
    if (is.null(ctryData))
      return()
    
    startDate <-
      Rnightlights::dateToNlPeriod(nlPeriodStart, nlType)
    
    endDate <-
      Rnightlights::dateToNlPeriod(nlPeriodEnd, nlType)
    
    allCols <- names(ctryData)
    
    admCols <- grep("NL_", allCols, invert = TRUE, value = TRUE)
    
    dataCols <- grep("NL_", allCols, value = TRUE)
    
    #the cols with the stats we want
    if(!missing(nlStat) && length(nlStat) > 0)
      dataCols <-
      grep(
        pattern = nlStat,
        x = allCols,
        value = TRUE,
        fixed = T
      )
    
    if(!missing(configName) && length(configName) > 0)
      dataCols <-
      grep(pattern = configName,
           x = dataCols,
           value = TRUE)
    
    if(!missing(extension) && length(extension) > 0)
      dataCols <-
      grep(pattern = extension,
           x = dataCols,
           value = TRUE)
    
    if(!missing(removeGasFlaresMethod) && length(removeGasFlaresMethod) > 0)
      dataCols <-
      grep(pattern = paste0("GF", toupper(removeGasFlaresMethod)),
           x = dataCols,
           value = TRUE)
    
    if(!missing(multiTileMergeStrategy) && length(multiTileMergeStrategy) > 0)
      dataCols <-
      grep(
        pattern = paste0("MTS", multiTileMergeStrategy),
        x = dataCols,
        value = TRUE
      )
    
    if(!missing(multiTileMergeFun) && length(multiTileMergeFun) > 0)
      dataCols <-
      grep(pattern = multiTileMergeFun,
           x = dataCols,
           value = TRUE)
    
    dataColDates <- gsub(".*_(\\d{4,8})_.*", "\\1", dataCols)
    
    dataCols <-
      dataCols[dataColDates >= startDate & dataColDates <= endDate]
    
    ctryData <- ctryData[, c(admCols, dataCols), with = F]
  },
  
  options = list(scrollX = TRUE, scrolly = TRUE))
}

######################## dtAvailableData ###################################
#'
#' @export
dtAvailableData <- function(lstCtryNlData)
{
  DT::datatable({
    
    if(missing(lstCtryNlData) || is.null(lstCtryNlData))
    {
      #dt <- ctryNlDataList()
      
      dt <- listCtryNlData()
    } else
    {
      dt <- lstCtryNlData
    }
    
    if (is.null(dt))
      return()
    
    names(dt) <-
      c(
        "DT",
        "CCode",
        "ADM",
        "NlType",
        "CfgName",
        "MTS",
        "MTF",
        "RGF",
        "NlPeriod",
        "PolySrc",
        "PolyVer",
        "PolyType",
        "NlStats"
      )
    
    dt
  },
  
  options = list(
    scrollX = TRUE,
    scrollY = FALSE,
    autoWidth = TRUE,
    columnDefs = list(list(width = "100px"))
  )
  )
}

######################## mapData ###################################
#'
#' @export
mapData <- function(ctryData, ctryCodes, admLevel, admLevelFilters, nlPeriod, nlStat,
                    scale, nlType,normArea, gadmPolySrc, gadmPolyVer, gadmPolyType, custPolyPath)
{
  #for cran checks
  area_sq_km <- NULL
  value <- NULL
  variable <- NULL
  
  wgs84 <- getCRS()
  
  allCols <- names(ctryData)
  
  statCols <- grep(pattern = "^NL_", x = allCols, value = TRUE)
  
  nonStatCols <- setdiff(x = allCols, y = statCols)
  
  if(missing(admLevel) || !grepl(pattern = admLevel, x = names(ctryData), fixed = TRUE))
    admLevel <- nonStatCols[length(nonStatCols)-1]
  
  if(missing(nlPeriod))
  {
    tileDets <- regmatches(x = statCols,
                         m = regexpr(pattern = "[VIIRSOL]{3,5}\\.[D|M|Y]_[A-Z]+_([a-zA-Z]+_?)+[^-]",
                                     text = statCols))
    
    nlType <- unlist(strsplit(tileDets, "_"))[1]
    
    configName <- unlist(strsplit(tileDets, "_"))[2]
    
    extension <- 
    
    nlPeriod <- regmatches(regexpr("\\d{4,}"))
  }
  
  map <- leaflet::leaflet()
  
  map <- leaflet::addTiles(map = map, 
                            urlTemplate = "http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png")
  map <- leaflet::addWMSTiles(
    map = map,
      layerId = "nlRaster",
      baseUrl = paste0(
        "http://localhost/cgi-bin/mapserv?map=",
        file.path(path.expand(
          Rnightlights::getNlDir("dirRasterOutput")
        ),
        "nightlights.map")
      ),
      layers = "NL_CTRYCODE_VIIRS.M_NLPERIOD_VCMCFG-MTSALL-MEAN-RGFT_GADM-3.6-SHPZIP",
      group = "ctryRaster",
      options = leaflet::WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        opacity = 0.8,
        TIME = Rnightlights::dateToNlPeriod(nlPeriod, nlType)
      )
    )
  
  if (is.null(gadmPolySrc) ||
      is.null(gadmPolyVer) || gadmPolySrc == "" || gadmPolyVer == "")
    return()
  
  if (is.null(admLevel))
    admLevel <- "country"
  
  mapExtent <- NULL
  selected <- NULL
  
  if (length(ctryCodes) > 1) {
    admLevel <- "country"
    
    ctryPoly0 <-
      Rnightlights::readCtryPolyAdmLayer(
        ctryCode = ctryCodes[1],
        admLevel = unlist(
          Rnightlights::getCtryShpLyrNames(ctryCodes = ctryCodes[1],
                                           lyrNums = 0)
        )
      )
    
    for (ctryCode in ctryCodes[2:length(ctryCodes)])
    {
      ctryPoly0 <- sp::rbind.SpatialPolygonsDataFrame(
        ctryPoly0,
        Rnightlights::readCtryPolyAdmLayer(
          ctryCode = ctryCode,
          admLevel = unlist(Rnightlights::getCtryShpLyrNames(ctryCode, 0))
        ),
        makeUniqueIDs = T
      )
      
    }
    
    ctryPoly0 <- sp::spTransform(ctryPoly0, wgs84)
    
    mapExtent <- raster::extent(ctryPoly0)
    
    ctryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
    
    if (is.null(ctryData))
      return()
    
    #data already in data.table form
    lvlCtryData <-
      stats::setNames(ctryData[, list(mean(value, na.rm = T),
                                      sum(area_sq_km, na.rm =T)),
                               by = list(ctryData[["country"]], ctryData[["variable"]])],
                      c("country", "variable", "value", "area_sq_km"))
    
    #rank the data
    varname <- paste0('rankcountry')
    lvlCtryData[[varname]] <-
      with(lvlCtryData, rank(-value, ties.method = 'first'))
    
    #palette deciles for the layer
    bins <-
      rev(stats::quantile(lvlCtryData$value, seq(0, 1, 0.1), na.rm = T))
    
    brewerPal <-
      rev(RColorBrewer::brewer.pal(n = 10, name = "YlOrRd"))
    
    pal <-
      leaflet::colorBin(
        palette = brewerPal,
        domain = lvlCtryData$value,
        na.color = "grey",
        bins = bins
      )
    
    mapLabels <- sprintf(
      paste0(
        "<strong>%s:%s</strong>",
        "<br/>Area: %s km<superscript>2</superscript>",
        "<br/>Date: %s",
        ifelse(
          normArea,
          paste0("<br/>Rad (", nlStat, "): %s /sq.km"),
          paste0("<br/>Rad (", nlStat, "): %s")
        ),
        "<br/>Rank: %s/%s"
      ),
      "country",
      lvlCtryData[["country"]],
      format(lvlCtryData[["area_sq_km"]], scientific = F, digits = 2),
      lvlCtryData[["variable"]],
      format(lvlCtryData[["value"]], scientific = F, digits = 2),
      lvlCtryData[[paste0("rankcountry")]],
      nrow(lvlCtryData)
    )
    
    mapLabels <- lapply(X = mapLabels, FUN = htmltools::HTML)
    
    map <- leaflet::addPolygons(
      map = map,
      data = ctryPoly0,
      #layerId = "country",
      fill = TRUE,
      fillColor = ~ pal(lvlCtryData[["value"]]),
      fillOpacity = 0.9,
      stroke = TRUE,
      weight = 1,
      #color=lineCol[iterAdmLevel],
      color = "white",
      smoothFactor = 0.7,
      opacity = 1,
      #dashArray = "5",
      group = "country",
      popup = mapLabels,
      popupOptions = leaflet::popupOptions(
        keepInView = T,
        closeOnClick = T,
        closeButton = T
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 5,
        #color = "yellow",
        #dashArray = "4",
        fillOpacity = 0,
        bringToFront = FALSE
      )
    )
  } else 
  {
    #if 1 country
    
    for (ctryCode in ctryCodes)
    {
      #print(paste0("admlvlNums:", admLvlNums))
      
      #get the selected admLevel and convert to lyrnum
      ctryAdmLevels <-
        unlist(searchAdmLevelName(ctryCode))
      
      #ctryAdmLevels <- unlist(ctryAdmLevels[which(ctryCodes == ctryCode)])
      if (!is.null(admLevel) || admLevel == "country")
        lyrNum <- which(ctryAdmLevels == admLevel)
      else
        lyrNum <- 0
      
      #line weight increases. max=4 min=1
      deltaLineWt <- (4 - 1) / as.numeric(lyrNum)
      
      #line color darkens
      
      if (stringr::str_detect(nlType, "OLS"))
        nlYm <- nlPeriod
      else if (stringr::str_detect(nlType, "VIIRS"))
        nlYm <- as.Date(nlPeriod, "%Y%m%d")
      
      ctryData <- ctryNlDataMelted(ctryData = ctryData, nlType = nlType, nlStat = nlStat)
      
      if (is.null(ctryData))
        return()
      
      #get our data ready to match with polygons
      #subset data based on level selections
      if (stringr::str_detect(nlType, "OLS"))
        ctryData <- subset(x = ctryData, variable == nlPeriodToDate(nlPeriod = nlYm, nlType = "OLS.Y"))
      else if (stringr::str_detect(string = nlType, pattern = "VIIRS"))
        ctryData <-
        subset(x = ctryData, 
               subset = lubridate::year(variable) == lubridate::year(nlYm) &
                 lubridate::month(variable) == lubridate::month(nlYm)
        )
      
      if (normArea)
        ctryData$value <- ctryData$value / ctryData$area_sq_km
      
      ctryPoly0 <-
        Rnightlights::readCtryPolyAdmLayer(
          ctryCode = ctryCode,
          admLevel = unlist(Rnightlights::getCtryShpLyrNames(ctryCode, 0))
        )
      
      ctryPoly0 <- sp::spTransform(ctryPoly0, wgs84)
      
      e <- raster::extent(ctryPoly0)
      if (exists("mapExtent") && !is.null(mapExtent))
      {
        mapExtent@xmin <- min(mapExtent@xmin, e@xmin)
        mapExtent@ymin <- min(mapExtent@ymin, e@ymin)
        mapExtent@xmax <- max(mapExtent@xmax, e@xmax)
        mapExtent@ymax <- max(mapExtent@ymax, e@ymax)
      }
      else
      {
        mapExtent <- e
      }
      
      if (stringr::str_detect(nlType, "OLS"))
        nlPeriod <- substr(gsub("-", "", nlYm), 1, 4)
      else if (stringr::str_detect(nlType, "VIIRS"))
        nlPeriod <- substr(gsub("-", "", nlYm), 1, 6)
      
      map <- leaflet::clearShapes(map = map)
      map <- leaflet::clearControls(map = map)
      map <- leaflet::addPolygons(
        map = map,
          layerId = ctryCode,
          fill = FALSE,
          fillColor = "#fefe40",
          stroke = TRUE,
          weight = 4,
          smoothFactor = 0.7,
          opacity = 1,
          color = "white",
          #dashArray = "5",
          group = "country",
          data = ctryPoly0
        )
      
      selected <- NULL
      
      lineCol <- rev(hexbin::BTC(lyrNum + 1, 200, 250))
      
      if (lyrNum > 1)
      {
        #skip drawing the country level. avoid reverse seq (2:1)
        for (iterAdmLevel in 2:lyrNum)
        {
          #aggregate the data to the current level
          iterAdmLevelName <- ctryAdmLevels[iterAdmLevel]
          
          #data already in data.table form
          lvlCtryData <-
            stats::setNames(ctryData[, list(mean(value, na.rm = T),
                                            sum(area_sq_km, na.rm =T)),
                                     by = list(ctryData[[iterAdmLevelName]],
                                               ctryData[["variable"]])],
                            c(iterAdmLevelName, "variable", "value", "area_sq_km"))
          
          #rank the data
          varname <- paste0('rank', iterAdmLevel)
          lvlCtryData[[varname]] <-
            with(lvlCtryData, rank(-value, ties.method = 'first'))
          
          #palette deciles for the layer
          bins <-
            rev(stats::quantile(lvlCtryData$value, seq(0, 1, 0.1), na.rm = T))
          brewerPal <-
            rev(RColorBrewer::brewer.pal(n = 10, name = "YlOrRd"))
          pal <-
            leaflet::colorBin(
              palette = brewerPal,
              domain = lvlCtryData$value,
              na.color = "grey",
              bins = bins
            )
          
          #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
          ctryPoly <-
            Rnightlights::readCtryPolyAdmLayer(ctryCode = ctryCode,
                                               unlist(
                                                 Rnightlights::getCtryShpLyrNames(ctryCode, iterAdmLevel - 1)
                                               ))
          
          ctryPoly <- sp::spTransform(ctryPoly, wgs84)
          
          e <- raster::extent(ctryPoly)
          
          if (exists("mapExtent") && !is.null(mapExtent))
          {
            mapExtent@xmin <- min(mapExtent@xmin, e@xmin)
            mapExtent@ymin <- min(mapExtent@ymin, e@ymin)
            mapExtent@xmax <- max(mapExtent@xmax, e@xmax)
            mapExtent@ymax <- max(mapExtent@ymax, e@ymax)
          }
          else
          {
            mapExtent <- e
          }
          
          if (length(admLevelFilters) > 0)
          {
            if (ctryAdmLevels[[iterAdmLevel]] == ctryAdmLevels[which(ctryAdmLevels == names(admLevelFilters))])
            {
              #iterAdmLevel+1 %in% admLvlNums)
              selected <-
                which(ctryPoly@data[[paste0("NAME_", iterAdmLevel - 1)]] %in% admLevelFilters[[ctryAdmLevels[[iterAdmLevel]]]])
            }
          }else
          {
            selected <- c()
          }
          
          mapLabels <- sprintf(
            paste0(
              "<strong>%s:%s</strong>",
              "<br/>Area: %s km<superscript>2</superscript>",
              "<br/>Date: %s",
              ifelse(
                normArea,
                paste0("<br/>Rad (", nlStat, "): %s /sq.km"),
                paste0("<br/>Rad (", nlStat, "): %s")
              ),
              "<br/>Rank: %s/%s"
            ),
            ctryAdmLevels[iterAdmLevel],
            lvlCtryData[[ctryAdmLevels[iterAdmLevel]]],
            format(
              lvlCtryData[["area_sq_km"]],
              scientific = F,
              digits = 2
            ),
            lvlCtryData[["variable"]],
            format(
              lvlCtryData[["value"]],
              scientific = F,
              digits = 2
            ),
            lvlCtryData[[paste0("rank", iterAdmLevel)]],
            nrow(lvlCtryData)
          )
          
          mapLabels <- lapply(X = mapLabels, FUN = htmltools::HTML)
          
          #only the lowest layer will have fill, label, etc
          if (iterAdmLevel == lyrNum)
          {
            map <- leaflet::addPolygons(
              map = map,
              data = ctryPoly,
              layerId = as.character(ctryPoly@data[, paste0('NAME_', iterAdmLevel -
                                                              1)]),
              fill = TRUE,
              fillColor = ~ pal(lvlCtryData[["value"]]),
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 4 - (iterAdmLevel - 1) * deltaLineWt,
              #color=lineCol[iterAdmLevel],
              color = "white",
              smoothFactor = 0.7,
              opacity = 1,
              #dashArray = "5",
              group = ctryAdmLevels[iterAdmLevel],
              popup = mapLabels,
              popupOptions = leaflet::popupOptions(
                keepInView = T,
                closeOnClick = T,
                closeButton = T
              ),
              highlightOptions = leaflet::highlightOptions(
                weight = 5,
                #color = "yellow",
                #dashArray = "4",
                fillOpacity = 0,
                bringToFront = FALSE
              )
            )
          } else
          {
            map <- leaflet::addPolygons(
              map = map,
              data = ctryPoly,
              layerId = as.character(ctryPoly@data[, paste0('NAME_', iterAdmLevel -
                                                              1)]),
              stroke = TRUE,
              weight = 4 - (iterAdmLevel - 1) * deltaLineWt,
              smoothFactor = 0.7,
              opacity = 1,
              #color=lineCol[iterAdmLevel],
              color = "white",
              #dashArray = "5",
              group = ctryAdmLevels[iterAdmLevel]
            )
          }
          
          if (length(selected) > 0)
            mapExtent <- NULL
          
          for (iterPoly in selected)
          {
            map <- leaflet::addPolygons(
              map = map,
              data = ctryPoly[iterPoly, ],
              layerId = paste0(as.character(ctryPoly@data[iterPoly,
                                                          paste0('NAME_', 
                                                                 iterAdmLevel -
                                                                             1)]),
                               "_selected"),
              fill = FALSE,
              stroke = TRUE,
              weight = 4 - (iterAdmLevel - 1) * deltaLineWt + 0.5,
              opacity = 1,
              color = "blue",
              # dashArray = "5",
              group = "selected",
              highlightOptions = leaflet::highlightOptions(
                stroke = TRUE,
                weight = 4 - (iterAdmLevel - 1) * deltaLineWt + 0.5,
                opacity = 1,
                color = "blue",
                bringToFront = TRUE
              )
            )
            
            e <- raster::extent(ctryPoly[iterPoly, ])
            
            if (exists("mapExtent") && !is.null(mapExtent))
            {
              mapExtent@xmin <- min(mapExtent@xmin, e@xmin)
              mapExtent@ymin <- min(mapExtent@ymin, e@ymin)
              mapExtent@xmax <- max(mapExtent@xmax, e@xmax)
              mapExtent@ymax <- max(mapExtent@ymax, e@ymax)
            }
            else
            {
              mapExtent <- e
            }
          }
        }
      }
    }
    
    
    if (length(selected) > 0)
    {
      map <- leaflet::addLayersControl(map = map,
                                       overlayGroups = c("ctryRaster",
                                                            ctryAdmLevels[2:lyrNum],
                                                            "selected"))
      
      map <-
        leaflet::flyToBounds(
          map = map,
          lng1 = mapExtent@xmin,
          lat1 = mapExtent@ymin,
          lng2 = mapExtent@xmax,
          lat2 = mapExtent@ymax
        )
    }
    else
      map <- leaflet::addLayersControl(map = map,
                                       overlayGroups = c("ctryRaster",
                                                         ctryAdmLevels[2:lyrNum]))
    
    if (admLevel != "country")
      map <- 
        leaflet::addLegend(
          map = map,
          position = "bottomright",
          pal = pal,
          values = format(ctryData$value, scientific = T),
          labels = stats::quantile(ctryData$value, seq(1, 0, -0.1), na.rm =
                                     T),
          #title = "Nightlight percentiles",
          title = ifelse(normArea, "Rad/sq. Km.", "Rad"),
          opacity = 1
        )
  }
  
  #Zoom
  if (exists("mapExtent"))
    map <- leaflet::flyToBounds(map = map,
                                mapExtent@xmin,
                                mapExtent@ymin,
                                mapExtent@xmax,
                                mapExtent@ymax)
    
  
  
  map
}