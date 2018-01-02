
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# 
# if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')
# 
# pacman::p_load(shiny, ggplot2, plotly, reshape2, rgdal, RColorBrewer, ggdendro, dendextend)
# 
# pacman::p_load_gh("rstudio/leaflet", "cloudyr/aws.s3")
# 
# library(shiny)
# library(ggplot2)
# suppressMessages(library(plotly))
# library(leaflet)
# library(reshape2)
# library(rgdal)
# library(RColorBrewer)
# library(ggdendro)
# library(dendextend)
# library(Rnightlights)
# library(aws.s3)

missingPkgs <- NULL

wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
 
if (!requireNamespace("Rnightlights", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "Rnightlights")
}

if (!requireNamespace("shiny", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "shiny")
  
}

if (!requireNamespace("dendextend", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "dendextend")
}

if (!requireNamespace("ggdendro", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "ggdendro")
}

if (!requireNamespace("leaflet", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "rstudio/leaflet")
}

if (!requireNamespace("plotly", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "plotly")
}

if (!requireNamespace("RColorBrewer", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "RColorBrewer")
}

if (!requireNamespace("reshape2", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "reshape2")
}

if (!requireNamespace("rgdal", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "rgdal")
}

if (!requireNamespace("Rnightlights", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "Rnightlights")
}

if(!is.null(missingPkgs))
 stop("Missing packages needed for this function to work. 
      Please install missing packages: '", paste0(missingPkgs, collapse = ", "), "'", call. = FALSE)
 
 
options(shiny.trace=F)

shiny::shinyServer(function(input, output, session){
  
  #Since renderUI does not like intraCountry returning NULL we init with an empty renderUI, set suspendWhenHidden = FALSE to force it to recheck intraCountry even if null
  output$intraCountry <- shiny::renderUI({})
  shiny::outputOptions(output, "intraCountry", suspendWhenHidden = FALSE)
  

  #yrs <- getAllNlYears("VIIRS")
  
  #isolate({updateTabItems(session, "inputs", "plotNightLights")})

  ######################## reactive ctryAdmLevels ###################################
  
    ctryAdmLevels <- shiny::reactive({
      #print(paste0("here: ctryAdmLevels"))
      
      if (length(input$countries) != 1)
        return()

      temp <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
      
      cols <- names(temp)
      
      cols <- cols[-grep("area_sq_km|NL_", cols)]
    })
    
  ######################## reactive ctryAdmLevelNames ###################################
  
    ctryAdmLevelNames <- shiny::reactive({
      #print(paste0("here: ctryAdmLevelNames"))
      
      countries <- input$countries
      
      if (length(countries) != 1)
        return()

      hdr <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(countries), nrows = 1, header = T)
      
      colClasses <- names(hdr)
      
      colClasses[-grep("area_sq_km|NL_", colClasses)] <- "character"
      colClasses[grep("area_sq_km|NL_", colClasses)] <- "NULL"
      
      data <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(countries), colClasses = colClasses, header = T)
    })
  
  ######################## reactive ctryNlTypes ###################################
  
  ctryNlTypes <- shiny::reactive({
    #print(paste0("here: ctryNlTypes"))
    
    countries <- input$countries
    
    if (length(countries) < 1)
     return()
    
    #temp <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
    
    #temp <- ctryNlData()
    
    nlTypes <- NULL
    
    if (length(countries) == 1)
    {
      hdrs <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(countries), nrows = 1, header = T)
      
      cols <- grep(pattern = "NL_", x = names(hdrs), value = T)
      
      nlTypes <- list(unique(sapply(cols, function(x)unlist(strsplit(x, "_"))[2])))
    }
    else if(length(countries) > 1) #remove subcountry admin levels
    {
      for (ctryCode in countries)
      {
        hdrs <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(ctryCode), nrows = 1, header = T)
        
        cols <- grep(pattern = "NL_", x = names(hdrs), value = T)
        
        temp <- list(unique(sapply(cols, function(x)unlist(strsplit(x, "_"))[2])))
        
        nlTypes <- c(nlTypes, temp)
      }
    }
    
    if(is.null(nlTypes))
      return(NULL)
    
    if(length(nlTypes) == 1)
      nlTypes <- unlist(nlTypes)
    else
      nlTypes <- unlist(Reduce(intersect, nlTypes))

    return(nlTypes)
  })
  
  ######################## reactive ctryDataStats ###################################
  
  ctryDataStats <- shiny::reactive({
    #print(paste0("here: ctryDataStats"))
    #print(paste0("here: ctryNlTypes"))
    
    countries <- input$countries
    
    if (length(countries) < 1)
      return()
    
    nlStats <- NULL
    
    if (length(countries) == 1)
    {
      hdrs <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(countries), nrows = 1, header = T)
      
      cols <- grep(pattern = "NL_", x = names(hdrs), value = T)
      
      nlStats <- list(unique(gsub(".*._.*._.*._", "", cols)))
    }
    else if(length(countries) > 1) #remove subcountry admin levels
    {
      for (ctryCode in countries)
      {
        hdrs <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(ctryCode), nrows = 1, header = T)
        
        cols <- grep(pattern = "NL_", x = names(hdrs), value = T)
        
        temp <- list(unique(gsub(".*._.*._.*._", "", cols)))
        
        nlStats <- c(nlStats, temp)
      }
    }
    
    if(is.null(nlStats))
      return(NULL)
    
    if(length(nlStats) == 1)
      nlStats <- unlist(nlStats)
    else
      nlStats <- unlist(Reduce(intersect, nlStats))

    return(nlStats)
  })
  
  
  ######################## reactive ctryNlData ###################################
  
    ctryNlData <- shiny::reactive({
      #print(paste0("here: ctryNlData"))
      input$btnGo
      
      countries <- shiny::isolate(input$countries)
      nlType <- shiny::isolate(input$nlType)
      
      if (length(countries) < 1)
        return(NULL)
      
      ctryData <- NULL
      
      if (length(countries) == 1)
      {
        ctryData <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(countries))
      }
      else if(length(countries) > 1) #remove subcountry admin levels
      {
        for (ctryCode in countries)
        {
          #print(ctryCode)
          temp <- data.table::fread(Rnightlights::getCtryNlDataFnamePath(ctryCode))
          
          ctryCols <- grep(paste0("country|area|NL_", nlType), names(temp))
          
          temp <- temp[, ctryCols, with=F]
          
          if (is.null(ctryData))
          {
            ctryData <- temp
          }else
          {
            ctryData <- merge(ctryData, temp, all=TRUE)
          }
        }
      }
      
      #get the nlType columns
      ctryCols <- names(ctryData)
      
      ctryNonNLCols <- grep("NL_", ctryCols, invert = T, value = T)
      ctryNLCols <- grep("NL_", ctryCols, value = T)
      
      ctryNLColsNlType <- grep(nlType, ctryNLCols, value = T)
      
      ctryData <- ctryData[, c(ctryNonNLCols, ctryNLColsNlType), with=F]
      
      return(ctryData)
    })  
  
  ######################## reactive ctryNlDataMelted ###################################
  
    ctryNlDataMelted <- shiny::reactive({
      #print(paste0("here: ctryNlDataMelted"))
      
      if(is.null(ctryNlData()))
        return()
      
      ctryData <- ctryNlData()
    
      #the nightlight cols
      nlCols <- names(ctryData)[grep("NL_", names(ctryData))]
      
      #the cols with the stats we want
      statCols <- names(ctryData)[grep(paste0("NL_.*.", input$ctryStat), names(ctryData))]
      
      #the non nightlight cols
      ctryDataCols <- setdiff(names(ctryData), nlCols)

      #the cols to melt by
      meltMeasureVars <- statCols
            
      #combine the non-nightlight cols and the cols with the stats we want
      ctryData <- subset(ctryData, select=c(ctryDataCols, meltMeasureVars))

      #remove non-digits to get only stat cols
      meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
      
      ctryData <- data.table::data.table(reshape2::melt(ctryData, measure.vars=meltMeasureVars))

      if(input$nlType == "OLS")
      {
        ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable))
        
        ctryData$variable <- as.numeric(ctryData$variable)
      }
      else if(input$nlType == "VIIRS")
      {
        ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable),"01")
      
        ctryData$variable <- as.Date(ctryData$variable, format="%Y%m%d")
      }
      
      return(ctryData)
    })

  ######################## renderUI ctryStats ###################################
    
    output$ctryStats <- shiny::renderUI({
      # if(length(input$countries) != 1)
      #   return()
      
      ctryDtStats <- ctryDataStats()
      
      if(is.null(ctryDtStats))
        return(NULL)
      
      if(!is.null(input$ctryStats))
        chosenStat <- input$ctryStats
      else
        chosenStat <- NULL
      
      shiny::radioButtons(inputId = "ctryStat",
                          label = "Stats",
                          choices = ctryDtStats,
                          inline = TRUE,
                          selected = chosenStat
      )
    })
  
  
  ######################## renderUI nlType ###################################
  
  output$nlType <- shiny::renderUI({
    if(length(input$countries) != 1)
      return()
    
    nlTypes <- ctryNlTypes()
    
    if(is.null(nlTypes))
      return(NULL)
    
    shiny::radioButtons(inputId = "nlType",
                        label = "NL Type",
                        choices = nlTypes,
                        inline = TRUE
    )
  })
  
  ######################## reactiveValues values ###################################
  
    values <- shiny::reactiveValues(
      lastUpdated = NULL
    )
    

    ######################## observe lastUpdated ###################################
    
    observe({
      lapply(names(input), function(x) {
        shiny::observe({
          input[[x]]
          values$lastUpdated <- x
        })
      })
    })
    
    output$intraCountry1 <- shiny::renderUI({
      if(length(input$countries) != 1)
        return()
      
      shiny::radioButtons(inputId = "admLevel", 
                     label = "Admin Level", 
                     choices = ctryAdmLevels()
                   )
    })

    ######################## render UI: intraCountry ###################################
    
    output$intraCountry <- shiny::renderUI({
      
      #print("here: renderUI intracountry")
      countries <- input$countries
      
      if(length(countries) != 1)
        return()
      
      ctryAdmLevels <- ctryAdmLevels()
      
      ctryAdmLevelNames <- ctryAdmLevelNames()
      
      if (length(ctryAdmLevelNames)>1)
        elems <- lapply(2:length(ctryAdmLevels), function(lvlIdx){
          
          lvl <- ctryAdmLevels[lvlIdx]
          
          lvlSelect <- unique(ctryAdmLevelNames[[ctryAdmLevels[lvlIdx]]])
          
          #         a <- checkboxInput(inputId = paste0("radioAdm", lvlIdx),
          #                      label = ctryAdmLevels[lvlIdx], 
          #                      value = FALSE
          #         )
          
          b <- shiny::selectizeInput(inputId = paste0("selectAdm", lvlIdx),
                              label = ctryAdmLevels[lvlIdx],
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE
          )
          
          shiny::updateSelectizeInput(session = session,
                               inputId = paste0("selectAdm", lvlIdx),
                               choices = lvlSelect,
                               server = TRUE
                               )
          
          b
          #list(a,b)
        })
    })
    
    ######################## observe selectAdms (intraCountry) ###################################
    
    shiny::observe({
      #print(paste0("here: observe selectAdms"))

      admLvlCtrlsNames <- names(input)
      
      x <- admLvlCtrlsNames[grep("selectAdm", admLvlCtrlsNames)]
      
      if(length(x)==0)
        return()

      admSelected <- FALSE
      lowestSelected <- ""
      for (i in x)
      {
        if (length(input[[i]]) > 0)
        {
          admSelected <- TRUE
          lowestSelected <- gsub("[^[:digit:]]","",i)
        }
      }
      
      if (!admSelected)
        return()
      
      ctryAdmLevelNames <- ctryAdmLevelNames()
      
      ctryAdmLevelNamesFilter <- ctryAdmLevelNames
      
      ctryAdmLevels <- ctryAdmLevels()
      
      lvlNum <- gsub("[^[:digit:]]", "",values$lastUpdated) #gsub("[^[:digit:]]", "", x)
      
      if(lvlNum=="")
        return()
      
      #print(paste0("lastupdated:", values$lastUpdated))
      
      #print(paste0("x:", x))
      #print(paste0("lvlnum:", lvlNum))
      
      #set admLevel to match the selectizeInput level
      #if (length(input[[paste0("selectAdm", lvlNum)]]) > 0)
        updateRadioButtons(session = session, inputId = "admLevel", selected = ctryAdmLevels[as.numeric(lowestSelected)])
      
      multipleSelected <- FALSE
      
      for (lvlIdx in 2:length(ctryAdmLevels))
      {
        lvlSelect <- ""
        top10 <- ""
        
        if (length(input[[paste0("selectAdm", lvlIdx)]]) > 1)
          multipleSelected <- TRUE
        
        if (lvlIdx < lvlNum)
        {
          #print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if (length(input[[paste0("selectAdm", lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
          }
          else
          {
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
          }
          #print(paste0("lvlSelect:",lvlSelect))
          
          #print(paste0("lvlselect: ", lvlSelect))
          #print(paste0("top10: ", top10))
          
          #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = TRUE)
          
          shiny::updateSelectInput(session, paste0("selectAdm",lvlIdx), choices = lvlSelect, selected = input[[paste0("selectAdm",lvlIdx)]])
        }
        else if(lvlIdx == lvlNum)
        {
          #print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if (length(input[[paste0("selectAdm", lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            #top10 <- if(length(lvlSelect) > 1) lvlSelect[1] else no = lvlSelect
            
            #print(paste0("lvlselect: ", lvlSelect))
            #print(paste0("top10: ", top10))
            
            #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = TRUE)
            
            # if (length(input[[paste0("radioAdm", lvlIdx)]])==0)
              shiny::updateSelectizeInput(session, paste0("selectAdm",lvlIdx), choices = lvlSelect, selected = input[[paste0("selectAdm",lvlIdx)]])
          }else
          {
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = NULL, selected = NULL)
          }
        }
        else
        {
          #print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if (multipleSelected)
          {
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = "", selected = NULL)
            next()
          }
          
          if(length(input[[paste0("selectAdm",lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = FALSE)

            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect, selected = NULL)
          }
          else if(length(input[[paste0("selectAdm",lvlIdx-1)]]) == 0 && length(input[[paste0("selectAdm", lvlNum)]])==1)
          {
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect, selected = NULL)
          }
          else
          {
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect, selected = NULL)
          }
        }
      }
      #})
    })
  #})


    ######################## sliderNlYearMonthRange ###################################
    
    output$sliderNlYearMonthRange <- shiny::renderUI({
      #print(paste0("here: sliderNlYearMonthRange"))
      ctryData <- ctryNlDataMelted()
      
      if (is.null(ctryData))
      {
        shiny::sliderInput(inputId = "nlYearMonthRange",
                    label = "Time",
                    min = as.Date("2014-01-01", "%Y-%m-%d"),
                    max = as.Date("2017-10-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 31,
                    value = c(as.Date("2014-01-01","%Y-%m-%d"),as.Date("2017-10-31","%Y-%m-%d"))
        )
      }
      else
      {
        minDate <- min(ctryData$variable)
        maxDate <- max(ctryData$variable)
                           
        
        
        shiny::sliderInput(inputId = "nlYearMonthRange",
                    label = "Time",
                    min = minDate,
                    max = maxDate,
                    timeFormat = "%Y-%m",
                    step = 31,
                    value = c(minDate, maxDate),
                    animate = animationOptions(interval = 2000, loop = FALSE, playButton = NULL, pauseButton = NULL)
        )
      }
      
    })
    
    ######################## sliderNlYearMonth ###################################
    
    output$sliderNlYearMonth <- shiny::renderUI({
      #print(paste0("here: sliderNlYearMonth"))
      
      ctryData <- ctryNlDataMelted()
      
      if (is.null(ctryData))
      {
        shiny::sliderInput(inputId = "nlYearMonth",
                    label = "Time",
                    min = as.Date("2014-01-01", "%Y-%m-%d"),
                    max = as.Date("2017-10-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 31,
                    value = as.Date("2014-01-01", "%Y-%m-%d")
        )
      }
      else
      {
        minDate <- min(ctryData$variable)
        maxDate <- max(ctryData$variable)
       
        if(input$nlType == "OLS")  
          shiny::sliderInput(inputId = "nlYearMonth",
                    label = "Time",
                    min = minDate,
                    max = maxDate,
                    #timeFormat = "%Y",
                    sep = "",
                    step = 1,
                    value = minDate,
                    animate = animationOptions(interval = 10000, loop = FALSE, playButton = "Play", pauseButton = NULL)
        )
        else if(input$nlType == "VIIRS")  
          shiny::sliderInput(inputId = "nlYearMonth",
                             label = "Time",
                             min = minDate,
                             max = maxDate,
                             timeFormat = "%Y-%m",
                             step = 31,
                             value = minDate,
                             animate = animationOptions(interval = 10000, loop = FALSE, playButton = "Play", pauseButton = NULL)
          )
      }
    })
    
    ######################## hCluster ###################################
    
    hCluster <- shiny::reactive({
      #print(paste0("here: reactive hCluster"))
      input$btnGo
      
      countries <- shiny::isolate(input$countries)
      
      if (is.null(countries))
        return()
      
      scale <- input$scale
      
      shiny::isolate({
      nlYearMonthRange <- input$nlYearMonthRange
      graphType <- input$graphType
      admLevel <- ctryAdmLevels()[2]

      #return if the country doesn't have adm levels below country
      if (is.na(admLevel))
        return()
      
      meltCtryData <- ctryNlDataMelted()
      
      if (is.null(countries) || is.null(meltCtryData))
        return()
      
      if ("norm_area" %in% scale)
        meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
      
      #aggMeltCtryData <- stats::aggregate(mean(value), by=list(eval(admLevel)+variable), data=meltCtryData, mean)
      aggMeltCtryData <- stats::setNames(meltCtryData[,list(mean(value, na.rm = TRUE)), by = list(meltCtryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
      
      dcastFormula <- paste(paste(admLevel, collapse = " + "), "~", paste("variable", collapse = " + "))
      
      unmeltCtryData <- data.table::dcast(aggMeltCtryData, dcastFormula, value.var='value', aggregate='mean')
      
      d <- stats::dist(unmeltCtryData)
      
      h <- stats::hclust(d)
      
      h$labels <- unmeltCtryData[[admLevel]]
      
      h
      })
    })
    
    ######################## plotHCluster ###################################
    
    output$plotHCluster <- shiny::renderPlot({
      #print(paste0("here: plotHCluster"))
      
      clusts <- hCluster()
      numClusters <- input$kClusters
      
      if (is.null(clusts))
        return("Country has no adm levels")
      
      shiny::isolate({
      dendro <- stats::as.dendrogram(clusts)
      
      cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

      dendro %>% dendextend::color_branches(k=numClusters, col = cbPalette) %>% 
        dendextend::color_labels(k=numClusters, col = cbPalette) %>%
        graphics::plot(horiz=FALSE, main = "")
      
      dendro %>% dendextend::rect.dendrogram(k=numClusters,horiz=FALSE,border = cbPalette)
      
      })
      
    })
    
    
    ######################## plotPointsCluster ###################################
    
    output$plotPointsCluster <- plotly::renderPlotly({
      #print(paste0("here: plotPointsCluster"))
      
      input$btnGo
      
      countries <- shiny::isolate(input$countries)
      
      if(length(countries) < 1)
        return()
      
      clusts <- hCluster()

      if(is.null(clusts))
        return()
            
      admLevel <- ctryAdmLevels()[2]

      
      #return if the country doesn't have adm levels below country
      if (admLevel == "")
        return()
      
      numClusters <- input$kClusters
      scale <- input$scale
      
      isolate({
        meltCtryData <- ctryNlDataMelted()
        
        if ("norm_area" %in% scale)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
        
        cutClusts <- stats::cutree(clusts, k=numClusters)
        
        #ctryAvg <- aggregate(value ~ admLevel, data=meltCtryData, mean)
        ctryAvg <- stats::setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]])], c(admLevel, "value"))
  
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        g <- ggplot2::ggplot(data=ctryAvg, aes(x=ctryAvg[[admLevel]], y=value, col=as.factor(cutClusts)))+geom_point(size=2)+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::scale_colour_manual(values=cbPalette)
        
        plotly::ggplotly(g)
      })
    })
    
    ######################## mapHCluster ###################################
    
    output$mapHCluster <- leaflet::renderLeaflet({
      #print(paste0("here: draw mapHCluster"))
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      input$btnGo
      
      
      countries <- shiny::isolate(input$countries)
      scale <- input$scale
      numClusters <- input$kClusters
      
      shiny::isolate({      
        if (is.null(countries))
          return()
        
        if (length(countries) != 1)
        {
          renderText("Please select only one country/region")
          return()
        }

        #print("drawing leaflet cluster")
        
        clusts <- hCluster()
        
        cutClusts <- stats::cutree(clusts, k=numClusters)
        
        admLevel <- ctryAdmLevels()[2]
        
        meltCtryData <- ctryNlDataMelted()
        
        if ("norm_area" %in% scale)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
        
        ctryPoly0 <- rgdal::readOGR(Rnightlights::getPolyFnamePath(countries), Rnightlights::getCtryShpLyrName(countries,0))
        
        map <- leaflet::leaflet(data=ctryPoly0) %>%
          #addTiles("http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>%
          leaflet::addTiles() %>%
          leaflet::addWMSTiles(layerId="nlRaster", baseUrl = "http://localhost/cgi-bin/mapserv?map=nightlights_wms.map", layers = "nightlights_201401", options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE, opacity=1)) %>%
          leaflet::addPolygons(layerId = countries, fill = FALSE, fillColor = "#fefe40", stroke = TRUE, weight=4, smoothFactor = 0.7, opacity = 1, color="white", dashArray = "5", group = "country")
        
        
        lvlCtryData <- stats::setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]])], c(admLevel, "value"))
        
        lvlCtryData[["rank"]] <- with(lvlCtryData, rank(-value, ties.method = 'first'))
        
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        pal <- cbPalette
        
        #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
        ctryPoly <- rgdal::readOGR(Rnightlights::getPolyFnamePath(countries), Rnightlights::getCtryShpLyrName(countries, 1)) 
        
        ctryPoly <- sp::spTransform(ctryPoly, wgs84)
        
        mapLabels <- sprintf(
          paste0("%s:%s", "<br/>Cluster: %s", "<br/>Rad:%s", "<br/>Rank: %s/%s"),
          admLevel, lvlCtryData[[1]], cutClusts, format(lvlCtryData[[2]],scientific = T,digits = 2), lvlCtryData[["rank"]], nrow(lvlCtryData)
        ) %>% lapply(htmltools::HTML)
        
        map <- map %>% leaflet::addPolygons(
          data = ctryPoly,
          layerId = as.character(ctryPoly@data[,'NAME_1']),
          fill = TRUE,
          fillColor = pal[cutClusts],
          fillOpacity = 0.9,
          stroke = TRUE, 
          weight=1,
          smoothFactor = 0.7,
          opacity = 1,
          color="white",
          dashArray = "5",
          group = admLevel,
          highlight = leaflet::highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0,
            bringToFront = TRUE),
          label = mapLabels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
        
        map <- map %>% leaflet::addLayersControl(overlayGroups = admLevel)
        
        map <- map %>% leaflet::addLegend(position = "bottomright", 
                                 colors = pal[unique(cutClusts)], 
                                 labels = unique(cutClusts),
                                 #title = "Nightlight percentiles",
                                 title = "clusters",
                                 opacity = 1 )
        
        map
      })
    })
    
    ######################## renderPlotly plotTSDecomposed ###################################

    output$plotTSDecomposed <- shiny::renderPlot({
      #print(paste0("here: plotTSDecomposed"))
      input$btnGo
      
      countries <- shiny::isolate(input$countries)
      
      if(length(countries) < 1)
        return()
      
      admLevel <- ctryAdmLevels()[1]
      
      
      #return if the country doesn't have adm levels below country
      if (admLevel == "")
        return()
      
      scale <- input$scale
      
      shiny::isolate({
        meltCtryData <- ctryNlDataMelted()
        
        if ("norm_area" %in% scale)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km

        #ctryAvg <- aggregate(value ~ admLevel, data=meltCtryData, mean)
        ctryAvg <- stats::setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
        
        startYear <- lubridate::year(min(ctryAvg$variable))
        startMonth <- lubridate::month(min(ctryAvg$variable))
        endYear <- lubridate::year(max(ctryAvg$variable))
        endMonth <- lubridate::month(max(ctryAvg$variable))
        
        ctryDataTS <- stats::ts(ctryAvg$value, start = c(startYear,startMonth), end = c(endYear,endMonth), frequency = 12)

        ctryDataTScomponents <- stats::decompose(ctryDataTS)
        #g <- ggplot2::autoplot(ctryDataTScomponents)
        
        graphics::plot(ctryDataTScomponents)
      })
    })
    
    ######################## plotYearly ###################################
    
    output$plotYearly <- plotly::renderPlotly({
      #print(paste0("here: renderPlotYearly"))
      input$btnGo
      
      countries <- shiny::isolate(input$countries)
      
      if (is.null(countries))
        return()
      
      scale <- input$scale
      nlYearMonthRange <- input$nlYearMonthRange
      graphType <- input$graphType

      shiny::isolate({      
        ctryData <- ctryNlDataMelted()
        
        if (is.null(countries) || is.null(ctryData))
          return()
        
        admLvlCtrlNames <- names(input)
        
        x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]

        admLvlNums <- NULL
        for (i in x)
          if(length(input[[i]])>0)
            admLvlNums <- c(admLvlNums, i)
          
          
          #print(paste0("x", x))
          #print(paste0("admlvlnums:", admLvlNums))
          
          #if (admLvlNum=="" && length(countries)>0)
          #  return()
          
          admLvlNums <- as.numeric(gsub("[^[:digit:]]","",admLvlNums))
          
          if (length(admLvlNums)==0)
            admLvlNums <- 1
          
          ctryAdmLevels <- ctryAdmLevels()
          admLevel <- ctryAdmLevels[as.numeric(data.table::last(admLvlNums))]
          
          #print(paste0("admLevel:", admLevel))
          
          if (!exists("admLevel") || is.null(admLevel) || length(admLevel)==0)
            admLevel <- "country"
          
          ctryData <- subset(ctryData, variable >= nlYearMonthRange[1] & variable <= nlYearMonthRange[2])
          
          for (lvl in admLvlNums)
          {
            if (lvl == 1)
              next()
            
            #print(paste0("lvl:",lvl))
            
            if (length(input[[x[lvl-1]]])>0)
            {
              ctryData <- subset(ctryData, ctryData[[ctryAdmLevels[lvl]]] %in% input[[x[lvl-1]]])
            }
          }
          
          ctryData$year <- lubridate::year(ctryData$variable)
          
          ctryData$month <- lubridate::month(ctryData$variable)
          
          #print(paste0("ctrydata nrow:", nrow(ctryData)))
          
          if ("norm_area" %in% scale)
            ctryData$value <- (ctryData$value)/ctryData$area_sq_km
          
          if (graphType == "boxplot")
          {
            if (length(countries)==1)
            {
              g <- ggplot2::ggplot(data=ctryData, aes(x=factor(variable), y=value, col=ctryData[[admLevel]])) + ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::labs(col=admLevel)
            }
            else
            {
              g <- ggplot2::ggplot(data=ctryData, aes(x=factor(variable), y=value, col=country)) + ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::facet_grid(facets = year(variable) ~ month(variable)) + ggplot2::labs(col=admLevel)
            }
            
            g <- g + ggplot2::geom_boxplot()# +facet_grid(.~variable)
          }
          else if (graphType == "line")
          {
            if (length(countries)==1)
            {
              #switched to data.table aggregation
              #ctryData <- stats::setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
              ctryData <- stats::setNames(ctryData[,list(mean(value, na.rm = TRUE)), by = list(ctryData[[admLevel]], variable, as.factor(year), as.factor(month))], c(admLevel, "variable", "year", "month", "value"))
              g <- ggplot2::ggplot(ctryData, aes(x=month, y=value, col=year, group=year))
            }
            else
            {
              #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
              #switched to data.table aggregation
              ctryData <- stats::setNames(ctryData[,mean(value, na.rm = TRUE),by = list(country, variable)], c("country", "variable", "value"))
              g <- ggplot2::ggplot(data=ctryData, aes(x=variable, y=value, col=country))
            }
            
            g <- g + ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::geom_smooth(aes(group=1),method = "loess", weight=3) #+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #+ labs(col=year)
          }
          else if (graphType == "histogram")
          {
            #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
            
            g <- ggplot(data=ctryData, aes(x=value))
            
            g <- g + ggplot2::geom_histogram(aes(y=..density..), bins = 30, colour="black", fill="white") + ggplot2::geom_density(alpha=.2, fill="#FF6666") + ggplot2::facet_wrap(~ variable+country, ncol = length(countries)) # Overlay with transparent density plot
            
          }
          
          if ("scale_y_log" %in% scale)
            g <- g + ggplot2::scale_y_log10()
          
          if ("scale_x_log" %in% scale)
            g <- g + ggplot2::scale_x_log10()
          
          if ("norm_area" %in% scale)
            g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Avg Rad (W.Sr^-1.cm^-2/Km2)") #y=expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2})))
          else
            g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Total Rad (W.Sr^-1.cm^-2)") #y=expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2}))
          
          plotly::ggplotly(g)
          
      })
    })
    
    ######################## plotNightLights ###################################
    
    output$plotNightLights <- plotly::renderPlotly({
      #print(paste0("here: renderPlot"))
      input$btnGo
      
      countries <- shiny::isolate(input$countries)
      
      if (is.null(countries))
        return()
      
      scale <- input$scale
      nlYearMonthRange <- input$nlYearMonthRange
      graphType <- input$graphType
      nlType <- shiny::isolate(input$nlType)
      
      ctryData <- ctryNlDataMelted()

      if (is.null(countries) || is.null(ctryData))
        return()
            
      admLvlCtrlNames <- names(input)
      
      x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]
      
      shiny::isolate({
        admLvlNums <- NULL
        for (i in x)
          if(length(input[[i]])>0)
            admLvlNums <- c(admLvlNums, i)
          
          
        #print(paste0("x", x))
        #print(paste0("admlvlnums:", admLvlNums))
        
        #if (admLvlNum=="" && length(countries)>0)
        #  return()
        
        admLvlNums <- as.numeric(gsub("[^[:digit:]]","",admLvlNums))
        
        if (length(admLvlNums)==0)
          admLvlNums <- 1
        
        ctryAdmLevels <- ctryAdmLevels()
        admLevel <- ctryAdmLevels[as.numeric(data.table::last(admLvlNums))]
        
        #print(paste0("admLevel:", admLevel))
        
        if (!exists("admLevel") || is.null(admLevel) || length(admLevel)==0)
          admLevel <- "country"
          
      ctryData <- subset(ctryData, variable >= nlYearMonthRange[1] & variable <= nlYearMonthRange[2])
      
      for (lvl in admLvlNums)
      {
        if (lvl == 1)
          next()
        
        #print(paste0("lvl:",lvl))
        
        if (length(input[[x[lvl-1]]])>0)
        {
          ctryData <- subset(ctryData, ctryData[[ctryAdmLevels[lvl]]] %in% input[[x[lvl-1]]])
        }
      }
      
      #print(paste0("ctrydata nrow:", nrow(ctryData)))
      
      if ("norm_area" %in% scale)
        ctryData$value <- (ctryData$value)/ctryData$area_sq_km

      if (graphType == "boxplot")
      {
        if (length(countries)==1)
        {
          g <- ggplot2::ggplot(data=ctryData, ggplot2::aes(x=factor(variable), y=value, col=ctryData[[admLevel]])) + ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::labs(col=admLevel)
        }
        else
        {
          g <- ggplot2::ggplot(data=ctryData, ggplot2::aes(x=factor(variable), y=value, col=country)) + ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::labs(col=admLevel)
        }
        
        g <- g + ggplot2::geom_boxplot()# +facet_grid(.~variable)
      }
      else if (graphType == "line")
      {
        if (length(countries)==1)
        {
          #switched to data.table aggregation
          #ctryData <- stats::setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
          ctryData <- stats::setNames(ctryData[,mean(value, na.rm = TRUE),by = list(ctryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
          g <- ggplot2::ggplot(data=ctryData, aes(x=variable, y=value, col=ctryData[[admLevel]]))
          
          if(nlType == "VIIRS")
            g <- g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
        }
        else
        {
          #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
          #switched to data.table aggregation
          ctryData <- stats::setNames(ctryData[,mean(value, na.rm = TRUE),by = list(country, variable)], c("country", "variable", "value"))
          g <- ggplot2::ggplot(data=ctryData, aes(x=variable, y=value, col=country))
          
          if(nlType == "VIIRS")
            g <- g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
        }

        g <- g+ ggplot2::geom_line() + ggplot2::geom_point()+ ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::labs(col=admLevel)
      }
      else if (graphType == "histogram")
      {
        #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
        
        g <- ggplot2::ggplot(data=ctryData, aes(x=value))
        
        g <- g + ggplot2::geom_histogram(aes(y=..density..), bins = 30, colour="black", fill="white") + ggplot2::geom_density(alpha=.2, fill="#FF6666") + ggplot2::facet_wrap(~ variable+country, ncol = length(countries)) # Overlay with transparent density plot

      }
      else
        return(NULL)

      if ("scale_y_log" %in% scale)
        g <- g + ggplot2::scale_y_log10()
      
      if ("scale_x_log" %in% scale)
        g <- g + ggplot2::scale_x_log10()
      
      if ("norm_area" %in% scale)
        g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Avg Rad (W.Sr^-1.cm^-2/Km2)") #y=expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2})))
      else
        g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Total Rad (W.Sr^-1.cm^-2)") #y=expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2}))
      
      plotly::ggplotly(g)
      
      })
    })
    
    ######################## renderDataTable dataset ###################################
    
    output$dataset <- DT::renderDataTable({
      if(is.null(ctryNlData()))
        return("NO DATA")
      
      ctryNlData()
    },
      
      options = list(scrollX = TRUE, scrolly = TRUE)
    )
    
    ######################## observe map ###################################
#     observe({
#       if(!exists("nlYearMonth"))
#         return()
#       
#       nlYm <- substr(gsub("-", "", nlYearMonth[1]), 1, 6)
#       ctryYearMonth <- paste0(countries, "_", nlYm)
#       
#       leafletProxy("map") %>%
#         clearTiles("nlRaster") %>%
#         addWMSTiles(baseUrl = "http://localhost/cgi-bin/mapserv?map=test.map", layers = ctryYearMonth, options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=0.5), layerId="nlRaster")
#     })
    
#     observeEvent(input$admLevel, {
#       print(paste0("here: observe admLevel 2 update map"))
#       admLevel <- input$admLevel
#       countries <- input$countries
#       
#       if (input$drawMap == 0)
#         return()
#       
#       lyrs <- ctryAdmLevels()
#       
#       lyrNum <- which(lyrs == admLevel) - 1
#       
#       ctryPoly <- readOGR(Rnightlights::getPolyFnamePath(countries), ifelse(is.null(admLevel),  yes = Rnightlights::getCtryShpLyrName(countries,0), no = Rnightlights::getCtryShpLyrName(countries,lyrNum)))
#       
#       proxy <- leafletProxy("map", data=ctryPoly)
#       
#       print("drawing leaflet proxy")
#       proxy %>% 
#         clearShapes() %>% 
#         addPolygons(fill = FALSE, stroke = TRUE, weight=3, smoothFactor = 0.7, opacity = 0.5, color="green")
#     })
    
    ######################## map ###################################

    output$map <- leaflet::renderLeaflet({
      #print(paste0("here: draw leaflet map"))
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      input$btnGo


      countries <- shiny::isolate(input$countries)
      nlYearMonth <- input$nlYearMonth
      admLevel <- shiny::isolate(input$admLevel)
      scale <- input$scale
      nlType <- input$nlType
      
      shiny::isolate({      
      if (is.null(countries) || is.null(nlYearMonth) || is.null(admLevel))
        return()
      
      if (length(countries) != 1)
      {
        shiny::renderText("Please select only one country/region")
        return()
      }
      
      admLvlCtrlNames <- names(input)
      
      x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]
      
      admLvlNums <- NULL
      for (i in x)
        if(length(input[[i]])>0)
          admLvlNums <- c(admLvlNums, i)
        
        
      #print(paste0("x", x))
      #print(paste0("admlvlnums:", admLvlNums))

      admLvlNums <- as.numeric(gsub("[^[:digit:]]","",admLvlNums))
      
      #print(paste0("admlvlNums:", admLvlNums))

      #get the selected admLevel and convert to lyrnum
      ctryAdmLevels <- ctryAdmLevels()

      lyrNum <- which(ctryAdmLevels == admLevel)
      
      #line weight increases. max=4 min=1
      deltaLineWt <- (4 - 1) / as.numeric(lyrNum)

      if(nlType == "OLS")
        nlYm <- nlYearMonth[1]
      else if (nlType=="VIIRS")
        nlYm <- as.Date(nlYearMonth[1], "%Y%m%d")

      ctryData <- ctryNlDataMelted()
      
      if (is.null(ctryData))
        return()
      
      #get our data ready to match with polygons
      #subset data based on level selections
      if(nlType == "OLS") 
        ctryData <- subset(ctryData, variable == nlYm)
      else if(nlType == "VIIRS")
        ctryData <- subset(ctryData, lubridate::year(variable) == lubridate::year(nlYm) & lubridate::month(variable) == lubridate::month(nlYm))
      #only used when we want to show only the selected features
      #for now we want all features shown and then highlight the selected features
#       for (lvl in admLvlNums)
#       {
#         if (lvl == 1)
#           next()
#         
#         print(paste0("lvl:",lvl))
#         
#         if (length(input[[x[lvl-1]]])>0)
#         {
#           ctryData <- subset(ctryData, ctryData[[ctryAdmLevels[lvl]]] %in% input[[x[lvl-1]]])
#         }
#       }

      if ("norm_area" %in% scale)
        ctryData$value <- (ctryData$value)/ctryData$area_sq_km

      #print(paste0("ctrydata nrow:", nrow(ctryData)))
      
      #print("drawing leaflet")
      
      ctryYearMonth <- paste0(countries, "_", nlYm)
      
      #message(ctryYearMonth)
      
      ctryPoly0 <- rgdal::readOGR(Rnightlights::getPolyFnamePath(countries), Rnightlights::getCtryShpLyrName(countries,0))

      if(nlType == "OLS")
        nlPeriod <- substr(gsub("-","",nlYm),1,4)
      else if(nlType=="VIIRS")
        nlPeriod <- substr(gsub("-","",nlYm),1,6)
      
      ctryRastFilename <- Rnightlights::getCtryRasterOutputFname(countries, nlType, nlPeriod)
      
      if(file.exists(ctryRastFilename))
      {
        ctryRast <- raster::raster(ctryRastFilename)
        
        #raster::projection(ctryRast) <- wgs84
      }
      else
        ctryRast <- NULL
      
      map <- leaflet::leaflet(data=ctryPoly0) %>%
        #leaflet::addTiles("http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>%
        leaflet::addTiles()
        
      if(inherits(ctryRast, "RasterLayer"))
      {
        map <- map %>% leaflet::addRasterImage(x = ctryRast,layerId = "ctryRasterLocal", group = "ctryRaster", project = T)

        leaflet::projectRasterForLeaflet(ctryRast)
      }
              
      map <-  map %>% leaflet::addWMSTiles(layerId="nlRaster",
                             baseUrl = "http://localhost/cgi-bin/mapserv?map=nightlights_wms.map", 
                             layers = "ctryRasterWMS",
                             group = "ctryRaster",
                             options = leaflet::WMSTileOptions(format = "image/png",
                                                      transparent = TRUE, opacity=1)
                             ) %>%
        
        leaflet::addPolygons(layerId = countries,
                             fill = FALSE,
                             fillColor = "#fefe40",
                             stroke = TRUE,
                             weight=4,
                             smoothFactor = 0.7,
                             opacity = 1,
                             color="white",
                             dashArray = "5",
                             group = "country"
                             )

        selected <- NULL
        
        if (lyrNum > 1) #skip drawing the country level. avoid reverse seq (2:1)
        for (iterAdmLevel in 2:lyrNum)
        {
          #aggregate the data to the current level
          iterAdmLevelName <- ctryAdmLevels[iterAdmLevel]
          #lvlCtryData <- stats::setNames(aggregate(ctryData$value, by=list(ctryData[[iterAdmLevelName]], ctryData[,"variable"]), mean, na.rm=T), c(iterAdmLevelName, "variable", "value"))
          
          #temp <- as.data.table(ctryData)
          #data already in data.table form
          lvlCtryData <- stats::setNames(ctryData[,list(mean(value,na.rm=T), sum(area_sq_km, na.rm=T)), by=list(ctryData[[iterAdmLevelName]], ctryData[["variable"]])], c(iterAdmLevelName, "variable", "value", "area_sq_km"))
          #lvlCtryData <- as.data.frame(lvlCtryData)
          
          #rank the data
          varname <- paste0('rank',iterAdmLevel)
          lvlCtryData[[varname]] <- with(lvlCtryData, rank(-value, ties.method = 'first'))
          
          #palette deciles for the layer
          bins <- unique(stats::quantile(lvlCtryData$value, seq(0,1,0.1), na.rm=T))
          brewerPal <- rev(RColorBrewer::brewer.pal(10, "YlOrRd"))
          pal <- leaflet::colorBin(brewerPal, domain = lvlCtryData$value, na.color = "grey", bins=bins)
          
          #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
          ctryPoly <- rgdal::readOGR(Rnightlights::getPolyFnamePath(countries), Rnightlights::getCtryShpLyrName(countries, iterAdmLevel-1)) 
          
          ctryPoly <- sp::spTransform(ctryPoly, wgs84)
          
          if (length(admLvlNums) > 0)
          if((iterAdmLevel) == data.table::last(admLvlNums)) #iterAdmLevel+1 %in% admLvlNums)
            selected <- which(ctryPoly@data[[paste0("NAME_",iterAdmLevel-1)]] %in% input[[paste0("selectAdm", iterAdmLevel)]])
          else
            selected <- c()
          

          mapLabels <- sprintf(
            paste0("<strong>%s:%s</strong>", "<br/>Area: %s km<superscript>2</superscript>","<br/>Date: %s", ifelse("norm_area" %in% scale, "<br/>Rad: %s /sq.km", "<br/>Rad: %s"), "<br/>Rank: %s/%s"),
            ctryAdmLevels[iterAdmLevel], lvlCtryData[[ctryAdmLevels[iterAdmLevel]]], format(lvlCtryData[["area_sq_km"]],scientific = T,digits = 2), lvlCtryData[["variable"]], format(lvlCtryData[["value"]],scientific = T,digits = 2),  lvlCtryData[[paste0("rank",iterAdmLevel)]], nrow(lvlCtryData)
          ) %>% lapply(htmltools::HTML)

          map <- map %>% leaflet::addPolygons(
            data = ctryPoly,
            layerId = as.character(ctryPoly@data[,paste0('NAME_',iterAdmLevel-1)]),
            fill = TRUE,
            fillColor = ~pal(lvlCtryData[["value"]]),
            fillOpacity = 0.9,
            stroke = TRUE, weight=4-(iterAdmLevel-1)*deltaLineWt,
            smoothFactor = 0.7,
            opacity = 1,
            color="white",
            dashArray = "5",
            group = ctryAdmLevels[iterAdmLevel],
            highlight = leaflet::highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0,
              bringToFront = TRUE),
              label = mapLabels,
              labelOptions = leaflet::labelOptions(
                style = list("font-weight" = "normal",
                             padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
                )
            )

          for (iterPoly in selected)
          {
              map <- map %>% leaflet::addPolygons(
                data = ctryPoly[iterPoly,],
                layerId = paste0(as.character(ctryPoly@data[iterPoly, paste0('NAME_',iterAdmLevel-1)]),"_selected"),
                fill = TRUE,
                fillColor = ~pal(lvlCtryData[["value"]][iterPoly]),
                fillOpacity = 0.9,
                stroke = TRUE,
                weight=4-(iterAdmLevel-1)*deltaLineWt+0.5,
                smoothFactor = 0.7,
                opacity = 1,
                color="blue",
                # dashArray = "5",
                group = "selected",
                highlight = leaflet::highlightOptions(
                  weight = 5,
                  color = "blue",
                  dashArray = "",
                  fillOpacity = 0,
                  bringToFront = TRUE),
                  label = mapLabels[iterPoly],
                  labelOptions = leaflet::labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                    )
                )
              
              e <- raster::extent(ctryPoly[iterPoly,])
              if (exists("mapExtent"))
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
      map <- map %>% leaflet::addLayersControl(overlayGroups = c("ctryRaster", ctryAdmLevels[2:lyrNum], "selected"))
      
      if (admLevel != "country")
        map <- map %>% leaflet::addLegend(position = "bottomright", 
                                 pal = pal, 
                                 values = format(ctryData$value, scientific = T),
                                 labels = stats::quantile(ctryData$value, seq(0,1,0.1), na.rm=T),
                                 #title = "Nightlight percentiles",
                                 title = ifelse("norm_area" %in% scale, "Rad/sq. Km.", "Total Rad"),
                                 opacity = 1 )
#       #Zoom in disabled
#       if (exists("mapExtent"))
#         map <- map %>% fitBounds(mapExtent@xmin, mapExtent@ymin, mapExtent@xmax, mapExtent@ymax)
      
      map
      })
    })
    
})
