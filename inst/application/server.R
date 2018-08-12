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

if (!requireNamespace("shinyjs", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "shinyjs")
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

if(!is.null(missingPkgs))
 stop(Sys.time(), ": Missing packages needed for this function to work. 
      Please install missing packages: '", paste0(missingPkgs, collapse = ", "), "'", call. = FALSE)
 
 
#options(shiny.trace=T)

shiny::shinyServer(function(input, output, session){
  
  #Since renderUI does not like intraCountry returning NULL we init with an empty renderUI, set suspendWhenHidden = FALSE to force it to recheck intraCountry even if null
  output$intraCountry <- shiny::renderUI({})
  shiny::outputOptions(output, "intraCountry", suspendWhenHidden = FALSE)
  #shinyjs::useShinyjs()

  #yrs <- getAllNlYears("VIIRS")
  
  #isolate({updateTabItems(session, "inputs", "plotNightLights")})

  ######################## actionButton btnGo ###################################
  
  output$btnGo <- shiny::renderUI({
    if(values$needsDataUpdate)
      shiny::actionButton("btnGo", "LOAD", style="background-color:orange")
    else
      shiny::actionButton("btnGo", "LOAD", style="background-color:lightblue")
  })
  
  ######################## renderUI countries ###################################
  
  output$countries <- shiny::renderUI({
    shiny::selectizeInput(inputId = "countries",
                        label = "Select Country(ies)",
                        choices = ctryCodesWithData(),
                        multiple = TRUE
    )
  })
  
  ######################## renderUI polySrc ###################################
  
  output$polySrc <- shiny::renderUI({
    if(is.null(input$countries))
      return()
    
    polySrcs <- Rnightlights::listCtryNlData(ctryCode=input$countries)$polySrc
    
    shiny::selectInput(inputId = "polySrc", label = "polySrc", choices = polySrcs)
  })
  
  ######################## renderUI polyVer ###################################
  
  output$polyVer <- shiny::renderUI({
    if(is.null(input$countries) || is.null(input$polySrc))
      return()
    
    polyVers <- Rnightlights::listCtryNlData(ctryCode=input$countries, polySrcs = input$polySrc)$polyVer
    
    shiny::selectInput(inputId = "polyVer", label = "polyVer", choices = polyVers)
  })
  
  ######################## reactive ctryCodesWithData ###################################
  
  ctryCodesWithData <- shiny::reactive({
    existingData <- Rnightlights::listCtryNlData()
    
    ctryCodesWithData <- unique(existingData$ctryCode)
    
    ctryCodeNames <- lapply(ctryCodesWithData, function(x) Rnightlights::ctryCodeToName(x))
    
    ctryCodeNames[is.na(ctryCodeNames)] <- "---"
    
    ctryCodesWithData <- stats::setNames(ctryCodesWithData, ctryCodeNames)
    
    ctryCodesWithData
  })
  
  ######################## reactive getInputCountries ###########################
  
  getInputCountries <- reactive({
    countries <- input$countries
    
    countries[countries=="---" | countries==" "] <- ""
    
    countries
  })
  
  ######################## reactive ctryAdmLevels ###################################
  
    ctryAdmLevels <- shiny::reactive({
      #print(paste0("here: ctryAdmLevels"))
      
      countries <- getInputCountries()
      
      polySrc <- input$polySrc
      
      polyVer <- input$polyVer
      
      if((length(countries) == 0 || countries == ""))
        if(is.null(polySrc) || polySrc == "" || polySrc == "GADM" || is.null(polyVer) || polyVer == "")
          return()
      
      if(!(length(countries) == 0 || countries == ""))
        if(is.null(polySrc) || polySrc == "" || is.null(polyVer) || polyVer == "")
          return()
      
      custPolyPath <- if(polySrc == "CUST") polyVer else NULL
      
      admLevelNames <- Rnightlights:::getCtryStructAdmLevelNames(ctryCode = countries, gadmVersion = polyVer, custPolyPath = custPolyPath)
      
      admLevelNames
    })
    
  ######################## reactive ctryAdmLevelNames ###################################
  
    ctryAdmLevelNames <- shiny::reactive({
      #print(paste0("here: ctryAdmLevelNames"))
      
      countries <- getInputCountries()
      
      if (length(countries) != 1)
        return()

      polySrc <- input$polySrc
      
      polyVer <- input$polyVer
      
      custPolyPath <- if(polySrc == "CUST") polyVer else NULL
      
      ctryStructFile <- Rnightlights:::getCtryStructFnamePath(ctryCode = countries, gadmVersion = polyVer, custPolyPath = custPolyPath)
      
      if(!file.exists(ctryStructFile))
        return()
      
      hdr <- data.table::fread(ctryStructFile, nrows = 1, header = T)
       
      colClasses <- names(hdr)
      
      colClasses[-grep("area_sq_km|NL_", colClasses)] <- "character"
      colClasses[grep("area_sq_km|NL_", colClasses)] <- "NULL"
      
      data <- data.table::fread(Rnightlights:::getCtryStructFnamePath(ctryCode = countries, gadmVersion = polyVer, custPolyPath = custPolyPath), colClasses = colClasses, header = T)
    })
  
  ######################## reactive ctryNlTypes ###################################
  
  ctryNlTypes <- shiny::reactive({
    #print(paste0("here: ctryNlTypes"))
    
    countries <- getInputCountries()
    
    polySrc <- input$polySrc
    
    polyVer <- input$polyVer
    
    if ((length(countries) == 0 || grepl("^\\s*$", countries)) && (is.null(polySrc) || polySrc =="" || is.null(polyVer) || polyVer==""))
      return()
    
    if(!(length(countries) == 0 || grepl("^\\s*$", countries)))
       if(is.null(polySrc) || polySrc =="" || is.null(polyVer) || polyVer=="")
         return()
    
    custPolyPath <- if(polySrc == "CUST") polyVer else NULL
    
    nlTypes <- unique(Rnightlights::listCtryNlData(ctryCodes = countries, polySrcs = polySrc, polyVers = polyVer)$nlType)
    
    return(nlTypes)
  })
  
  ######################## reactive ctryDataStats ###################################
  
  ctryDataStats <- shiny::reactive({
    #print(paste0("here: ctryDataStats"))
    #print(paste0("here: ctryNlTypes"))
    
    countries <- getInputCountries()
    polySrc <- input$polySrc
    polyVer <- input$polyVer
    
    if ((length(countries) == 0 || grepl("^\\s*$", countries)) && (is.null(polySrc) || polySrc =="" || polySrc == "GADM" || is.null(polyVer) || polyVer==""))
      return()

    if(is.null(polySrc) || polySrc == "")
      return()
    
    if(is.null(polyVer) || polyVer == "")
      return()
    
    nlType <- input$nlType
    
    if(is.null(nlType))
      return(NULL)
        
    nlStats <- NULL

    custPolyPath <- if(polySrc == "CUST") polyVer else NULL
    
    if (length(countries) == 1)
    {
      admLevel <- selectedAdmLevel()
      
      ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = countries, admLevel = admLevel, gadmVersion = polyVer, custPoly = custPolyPath)
      
      if(file.exists(ctryNlDataFile))
        hdrs <- data.table::fread(ctryNlDataFile, nrows = 1, header = T)
      else
        hdrs <- NULL
      
      cols <- grep(pattern = paste0("NL_", nlType), x = names(hdrs), value = T)
      
      nlStats <- list(unique(gsub(".*._.*._.*._", "", cols)))
    }
    else if(length(countries) > 1) #remove subcountry admin levels
    {
      for (ctryCode in countries)
      {
        admLevel <- paste0(ctryCode, "_adm0")
        
        ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = polyVer, custPolyPath = custPolyPath)
        
        if(file.exists(ctryNlDataFile))
          hdrs <- data.table::fread(ctryNlDataFile, nrows = 1, header = T)
        else
          hdrs <- NULL
        
        cols <- grep(pattern = paste0("NL_", nlType), x = names(hdrs), value = T)
        
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
  
  ######################## reactive ctryNlDataLvl2 ###################################
  
  ctryNlDataLvl2 <- shiny::reactive({
    #print(paste0("here: ctryNlDataLvl2"))
    input$btnGo
    
    countries <- shiny::isolate(getInputCountries())
    polySrc <- shiny::isolate(input$polySrc)
    polyVer <- shiny::isolate(input$polyVer)
    nlType <- shiny::isolate(input$nlType)
    
    if (is.null(polySrc) || polySrc=="" || is.null(polyVer) || polyVer=="")
      return(NULL)
    
    if(is.null(nlType))
      return(NULL)
    
    ctryData <- NULL
    
    custPolyPath <- if(polySrc == "CUST") polyVer else NULL
    
    if (length(countries) == 1)
    {
      admLevel <- unlist(Rnightlights:::getCtryShpAllAdmLvls(ctryCodes = countries, gadmVersion = polyVer, custPolyPath = custPolyPath))[2]
      
      if(input$strict)
      {
        ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = countries, admLevel = admLevel, gadmVersion = polyVer, custPolyPath = custPolyPath)
        
        if(file.exists(ctryNlDataFile))
          ctryData <- data.table::fread(ctryNlDataFile)
        else
          ctryData <- NULL
      }
      else
      {
        ctryData <- ctryNlDataMelted()
        
        ctryData <- stats::setNames(ctryData[,list(mean(value, na.rm = TRUE)), by = list(ctryData[[2]], variable)], c(admLevel, "variable", "value"))
      }
    }
    else if(length(countries) > 1) #remove subcountry admin levels
    {
      for (ctryCode in countries)
      {
        admLevel <- paste0(ctryCode, "_adm0")
        #print(ctryCode)
        
        ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = polyVer, custPolyPath = custPolyPath)
        
        if(file.exists(ctryNlDataFile))
          temp <- data.table::fread(ctryNlDataFile)
        else
          temp <- NULL
        
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
  
  ######################## reactive ctryNlData ###################################
  
    ctryNlData <- shiny::reactive({
      #print(paste0("here: ctryNlData"))
      input$btnGo
      
      countries <- shiny::isolate(getInputCountries())
      polySrc <- shiny::isolate(input$polySrc)
      polyVer <- shiny::isolate(input$polyVer)
      nlType <- shiny::isolate(input$nlType)
      
      if (is.null(polySrc) || polySrc == "" || is.null(polyVer) || polyVer == "")
        return(NULL)
      
      if(is.null(nlType))
        return(NULL)
      
      ctryData <- NULL

      custPolyPath <- if(polySrc == "CUST") polyVer else NULL
      
      if (length(countries) == 1)
      {
        lyrNum <- which(unlist(ctryAdmLevels()) == shiny::isolate(input$admLevel))-1
        
        admLevel <- unlist(Rnightlights:::getCtryShpLyrNames(ctryCodes = countries, lyrNums = lyrNum, gadmVersion = polyVer, custPolyPath = custPolyPath))
        
        if(input$strict)
        {
          ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = countries, admLevel = admLevel, gadmVersion = polyVer, custPolyPath = custPolyPath)
          
          if(file.exists(ctryNlDataFile))
            ctryData <- data.table::fread(ctryNlDataFile)
        }
        else
        {
          lyrNum <- Rnightlights:::ctryShpLyrName2Num(Rnightlights:::getCtryShpLowestLyrNames(ctryCodes = countries, gadmVersion = polyVer, custPolyPath = custPolyPath))
          
          while(lyrNum > 0 && is.null(ctryData))
          {
            admLevel <- Rnightlights:::getCtryShpLyrNames(ctryCodes = countries, lyrNums = lyrNum, gadmVersion = polyVer, custPolyPath = custPolyPath)
            
            ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = countries, admLevel = admLevel, gadmVersion = polyVer, custPolyPath = custPolyPath)
        
            if(file.exists(ctryNlDataFile))
              ctryData <- data.table::fread(ctryNlDataFile)
            
            lyrNum <- lyrNum - 1
          }
        }
      }
      else if(length(countries) > 1) #remove subcountry admin levels
      {
        for (ctryCode in countries)
        {
          admLevel <- paste0(ctryCode, "_adm0")
          #print(ctryCode)
          
          ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = ctryCode, admLevel = admLevel, gadmVersion = polyVer, custPolyPath = custPolyPath)
          
          if(file.exists(ctryNlDataFile))
            temp <- data.table::fread(ctryNlDataFile)
          else
            temp <- NULL
          
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
      print(paste0("here: ctryNlDataMelted"))
      
      ctryData <- ctryNlData()
    
      ctryStat <- input$ctryStat
      
      if(is.null(ctryData))
        return()
      
      if(is.null(ctryStat))
        return()
      
      shiny::isolate({
        #the nightlight cols
        nlCols <- names(ctryData)[grep("NL_", names(ctryData))]
        
        #the cols with the stats we want
        statCols <- names(ctryData)[grep(paste0("NL_.*.", ctryStat), names(ctryData))]
        
        #the non nightlight cols
        ctryDataCols <- setdiff(names(ctryData), nlCols)
  
        #the cols to melt by
        meltMeasureVars <- statCols
              
        #combine the non-nightlight cols and the cols with the stats we want
        ctryData <- subset(ctryData, select=c(ctryDataCols, meltMeasureVars))
  
        #remove non-digits to get only stat cols
        meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
        
        ctryData <- data.table::data.table(reshape2::melt(ctryData, measure.vars=meltMeasureVars))
  
        if(stringr::str_detect(input$nlType, "OLS"))
        {
          ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable))
          
          ctryData$variable <- as.numeric(ctryData$variable)
        }
        else if(stringr::str_detect(input$nlType, "VIIRS"))
        {
          if(stringr::str_detect(input$nlType, "M"))
            ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable),"01")
          else if(stringr::str_detect(input$nlType, "Y"))
            ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable),"0101")
        
          ctryData$variable <- as.Date(ctryData$variable, format="%Y%m%d")
        }
        
        return(ctryData)
      })
    })

  ######################## reactive ctryNlDataMeltedLvl2 ###################################
  
  ctryNlDataMeltedLvl2 <- shiny::reactive({
    #print(paste0("here: ctryNlDataMelted"))
    
    ctryData <- ctryNlDataLvl2()
    
    if(is.null(ctryData))
      return()
    
    if(is.null(input$ctryStat))
      return()
    
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
    
    if(stringr::str_detect(input$nlType, "OLS"))
    {
      ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable))
      
      ctryData$variable <- as.numeric(ctryData$variable)
    }
    else if(stringr::str_detect(input$nlType, "VIIRS"))
    {
      if(stringr::str_detect(input$nlType, "M"))
        ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable),"01")
      else if(stringr::str_detect(input$nlType, "Y"))
        ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable),"0101")
      
      ctryData$variable <- as.Date(ctryData$variable, format="%Y%m%d")
    }
    
    return(ctryData)
  })
  
  ######################## renderUI ctryStats ###################################
    
    output$ctryStats <- shiny::renderUI({
      # if(length(input$countries) != 1)
      #   return()
      
      polySrc <- input$polySrc
      
      polyVer <- input$polyVer
      
      nlType <- input$nlType
      
      if(is.null(polySrc) || polySrc=="" || is.null(polyVer) || polyVer=="" || is.null(nlType))
        return()
      
      ctryDtStats <- ctryDataStats()
      
      if(length(ctryDtStats)==0)
        return(NULL)
      
      if(!is.null(input$ctryStat))
        chosenStat <- input$ctryStat
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
    countries <- getInputCountries()
    if(length(countries) == 0 || countries=="")
      return()
    
    nlTypes <- ctryNlTypes()
    
    if(is.null(nlTypes))
      return(NULL)
    
    if(!is.null(input$nlType))
      chosenNlType <- input$nlType
    else
      chosenNlType <- NULL
    
    shiny::radioButtons(inputId = "nlType",
                        label = "NL Type",
                        choices = nlTypes,
                        selected = chosenNlType,
                        inline = TRUE
    )
  })
  
  ######################## reactiveValues values ###################################
  
    values <- shiny::reactiveValues(
      lastUpdated = NULL,
      needsDataUpdate = FALSE
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
    
  # observe({
  #   if(length(input$countries) != 1)
  #     return()
  #   
  #   admLvlCtrlNames <- names(input)
  #   
  #   selectAdmLvls <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]
  #   
  #   if(length(selectAdmLvls) > 0)
  #   lapply(selectAdmLvls, function(selectAdmLvl){
  #     lyrNum <- as.numeric(gsub("[^[:digit:]]", "", selectAdmLvl))-1
  #     
  #     admLevel <- unlist(Rnightlights:::getCtryShpLyrNames(input$countries, lyrNum))
  # 
  #     ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(input$countries, admLevel)
  #       
  #     if(!file.exists(ctryNlDataFile))
  #       shinyjs::disable(selectAdmLvl)
  #   })
  # })

  observeEvent(input$nlType, {
    values$needsDataUpdate <- TRUE
  })
  
  observeEvent(input$countries, {
    values$needsDataUpdate <- TRUE
  })
  
  observeEvent(input$polySrc, {
    values$needsDataUpdate <- TRUE
  })
  
  observeEvent(input$polyVer, {
    values$needsDataUpdate <- TRUE
  })
  
  observeEvent(input$btnGo, {
    values$needsDataUpdate <- FALSE
  })
  
  ######################## renderUI intraCountry1 ###################################
  
    output$intraCountry1 <- shiny::renderUI({
      countries <- getInputCountries()
      polySrc <- input$polySrc
      polyVer <- input$polyVer
      
      if(length(countries) != 1 || is.null(polySrc) || is.null(polyVer))
        return()

      custPolyPath <- if(polySrc == "CUST") polyVer else NULL
      
      admLevels <- unlist(ctryAdmLevels())
      
      if(is.null(admLevels))
        return()
      
      if(input$strict)
      admLevels <- unlist(sapply(1:length(admLevels), function(admLevel)
      {
        ctryNlDataFile <- Rnightlights::getCtryNlDataFnamePath(ctryCode = countries, admLevel = Rnightlights:::getCtryShpLyrNames(ctryCode = countries, lyrNums = admLevel-1, gadmVersion = polyVer, custPolyPath = custPolyPath), gadmVersion = polyVer, custPolyPath = custPolyPath)
        
        if(file.exists(ctryNlDataFile))
          return(admLevels[admLevel])
        else
          return(paste0(admLevels[admLevel], " (NA)"))
      }))

      shiny::radioButtons(inputId = "admLevel", 
                     label = "Admin Level", 
                     choiceNames = admLevels,
                     choiceValues = gsub("\\s*\\(NA\\)", "", admLevels)
                   )
    })

    ######################## render UI: intraCountry ###################################
    tags$head(tags$style(HTML("div.form-group.shiny-input-container {margin-top: -20px; margin-bottom: -20px;}")))
  
  
    output$intraCountry <- shiny::renderUI({
      
      #print("here: renderUI intracountry")
      countries <- getInputCountries()
      
      polySrc <- input$polySrc
      
      polyVer <- input$polyVer
      
      if((length(countries) != 1 || identical(countries, character(0)) || grepl("^\\s*$", countries)) && (is.null(polySrc) || polySrc=="" || is.null(polyVer) || polyVer==""))
        return()

      ctryAdmLevels <- unlist(ctryAdmLevels())
      
      if(is.null(ctryAdmLevels))
        return()
      
      ctryAdmLevelNames <- ctryAdmLevelNames()
      
      if (length(ctryAdmLevelNames)>1)
        elems <- lapply(2:length(ctryAdmLevels), function(lvlIdx){
          
          lvl <- ctryAdmLevels[lvlIdx]
          
          if(input$strict)
            lvlEnabled <- file.exists(Rnightlights::getCtryNlDataFnamePath(countries, paste(getInputCountries(), "_adm", lvlIdx-1, sep = "")))
          else
            lvlEnabled <- TRUE
          
          #lvlSelect <- unique(ctryAdmLevelNames[[ctryAdmLevels[lvlIdx]]])
          
          lvlSelect <- unique(dplyr::select(ctryAdmLevelNames, lvlIdx-1,lvlIdx))
          
          lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
          
          lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
          
          names(lvlSelect) <- paste(ctryAdmLevels[lvlIdx-1], names(lvlSelect), sep = ":")

          if(!lvlEnabled)
            lvlSelect <- NULL
          
          if(lvlEnabled)
          {
            b <- shiny::selectizeInput(inputId = paste0("selectAdm", lvlIdx),
                                label = ctryAdmLevels[lvlIdx],
                                #choices = NULL,
                                choices = lvlSelect,
                                multiple = TRUE
          )
          }else
          {
            b <- shiny::textInput(inputId = "dummy",
                             label = ctryAdmLevels[lvlIdx],
                             value = "Strict: Data Not Available",
                             placeholder = "Disable Strict to aggregate data")
          }
          
          b
        })
    })
    
    ######################## selectedAdmLevel ###################################
    
    selectedAdmLevel <- shiny::reactive({
      
      if(length(getInputCountries()) > 1)
        return()
      
      admLvlCtrlNames <- names(input)
    
      x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]
    
      if(length(x) == 0)
        return()
      
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
        
        admLvlNums <- admLvlNums - 1
        
        admLevel <- paste0(getInputCountries(), "_adm",data.table::last(admLvlNums))
        
        admLevel
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
      
      ctryAdmLevels <- unlist(ctryAdmLevels())
      
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
        
        if(input$strict)
          lvlEnabled <- file.exists(Rnightlights::getCtryNlDataFnamePath(getInputCountries(), Rnightlights:::getCtryShpLyrNames(getInputCountries(), lvlIdx-1)))
        else
          lvlEnabled <- TRUE
        
        if (length(input[[paste0("selectAdm", lvlIdx)]]) > 1)
          multipleSelected <- TRUE
        
        if (lvlIdx < lvlNum)
        {
          #print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if (length(input[[paste0("selectAdm", lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            
            #lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            lvlSelect <- unique(dplyr::select(ctryAdmLevelNamesFilter, lvlIdx-1,lvlIdx))
            
            lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
            
            lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
            
            names(lvlSelect) <- paste(ctryAdmLevels[lvlIdx-1], names(lvlSelect), sep = ":")
          }
          else
          {
            #lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            lvlSelect <- unique(dplyr::select(ctryAdmLevelNamesFilter, lvlIdx-1,lvlIdx))
            
            lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
            
            lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
            
            names(lvlSelect) <- paste(ctryAdmLevels[lvlIdx-1], names(lvlSelect), sep=":")
          }
          #print(paste0("lvlSelect:",lvlSelect))
          
          #print(paste0("lvlselect: ", lvlSelect))
          #print(paste0("top10: ", top10))
          
          #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = TRUE)
          
          if(!lvlEnabled)
            lvlSelect <- NULL
          
          shiny::updateSelectInput(session, paste0("selectAdm",lvlIdx), choices = lvlSelect, selected = input[[paste0("selectAdm",lvlIdx)]])
        }
        else if(lvlIdx == lvlNum)
        {
          #print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if (length(input[[paste0("selectAdm", lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            #lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            lvlSelect <- unique(dplyr::select(ctryAdmLevelNamesFilter, lvlIdx-1,lvlIdx))
            
            lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
            
            lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
            
            names(lvlSelect) <- paste(ctryAdmLevels[lvlIdx-1], names(lvlSelect), sep=":")
            
            #top10 <- if(length(lvlSelect) > 1) lvlSelect[1] else no = lvlSelect
            
            #print(paste0("lvlselect: ", lvlSelect))
            #print(paste0("top10: ", top10))
            
            #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = TRUE)
            
            if(!lvlEnabled)
              lvlSelect <- NULL
            
            # if (length(input[[paste0("radioAdm", lvlIdx)]])==0)
              shiny::updateSelectizeInput(session, paste0("selectAdm",lvlIdx), choices = lvlSelect, selected = input[[paste0("selectAdm",lvlIdx)]])
          }else
          {
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = NULL)
          }
        }
        else
        {
          #print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
#          if (multipleSelected)
#          {
#            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = "")
#            next()
#          }
          
          if(length(input[[paste0("selectAdm",lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            #lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            lvlSelect <- unique(dplyr::select(ctryAdmLevelNamesFilter, lvlIdx-1,lvlIdx))
            
            lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
            
            lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
            
            names(lvlSelect) <- paste(ctryAdmLevels[lvlIdx-1], names(lvlSelect), sep=":")
            
            #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = FALSE)

            if(!lvlEnabled)
              lvlSelect <- NULL
            
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect)
          }
          else if(length(input[[paste0("selectAdm",lvlIdx-1)]]) == 0 && length(input[[paste0("selectAdm", lvlNum)]])==1)
          {
            #lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            lvlSelect <- unique(dplyr::select(ctryAdmLevelNamesFilter, lvlIdx-1,lvlIdx))
            
            lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
            
            lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
            
            names(lvlSelect) <- paste(ctryAdmLevels[lvlIdx-1], names(lvlSelect), sep=":")
            
            if(!lvlEnabled)
              lvlSelect <- NULL
            
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect)
          }
          else
          {
            ##lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            #lvlSelect <- unique(dplyr::select(ctryAdmLevelNamesFilter, lvlIdx-1,lvlIdx))
            
            #lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
            
            #lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
            
            #shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect, selected = NULL)
            if(length(input[[paste0("selectAdm", lvlIdx-1)]]) > 0)
              ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]] %in% input[[paste0("selectAdm", lvlIdx-1)]])
            #lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            lvlSelect <- unique(dplyr::select(ctryAdmLevelNamesFilter, lvlIdx-1,lvlIdx))
            
            lvlSelect <- group_by(lvlSelect, ctryAdmLevels[lvlIdx-1])
            
            lvlSelect <- split(lvlSelect[[ctryAdmLevels[lvlIdx]]], lvlSelect[[ctryAdmLevels[lvlIdx-1]]])
            
            names(lvlSelect) <- paste(ctryAdmLevels[lvlIdx-1], names(lvlSelect), sep = ":")
            
            #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = FALSE)
            
            if(!lvlEnabled)
              lvlSelect <- NULL
            
            shiny::updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect)
          }
        }
        
        # if(!lvlEnabled)
        # {
        #   shinyjs::disable(selector = paste0("[type=radio][value=selectAdm", lvlIdx,"]"))
        #   
        #   shinyjs::disable(paste0("selectAdm", lvlIdx))
        #   
        #   shinyjs::runjs(paste0("$('selectAdm", lvlIdx, "').parent().parent().addClass('disabled').css('opacity', 0.4)"))
        # }
      }
      #})
    })
  #})


    ######################## sliderNlPeriodRange ###################################
    
    output$sliderNlPeriodRange <- shiny::renderUI({
      #print(paste0("here: sliderNlPeriodRange"))
      ctryData <- ctryNlDataMelted()
      
      shiny::isolate({
        if (is.null(ctryData))
        {
          nlRangeStart <- NULL
          nlRangeEnd <- NULL
          return()
          # shiny::sliderInput(inputId = "nlPeriodRange",
          #             label = "Time",
          #             min = as.Date("2012-04-01", "%Y-%m-%d"),
          #             max = as.Date("2017-10-31", "%Y-%m-%d"),
          #             timeFormat = "%Y-%m",
          #             step = 31,
          #             value = c(as.Date("2012-04-01","%Y-%m-%d"),as.Date("2017-10-31","%Y-%m-%d"))
          # )
        }
        else
        {
          minDate <- min(ctryData$variable)
          maxDate <- max(ctryData$variable)
          startDate <- minDate
          endDate <- maxDate
          
          if(stringr::str_detect(input$nlType, "D"))
          {
            tmFmt <- "%Y-%m-%d"
            
            if(!is.null(input$nlPeriodRange))
            {
              nlRangeStart <- as.Date(as.character(input$nlPeriodRange[1]), tmFmt)
              nlRangeEnd <- as.Date(as.character(input$nlPeriodRange[2]), tmFmt)
            }
            
            step <- 1
          }else if(stringr::str_detect(input$nlType, "M"))
          {
            tmFmt <- "%Y-%m"
            
            if(!is.null(input$nlPeriodRange))
            {
              nlRangeStart <- as.Date(as.character(input$nlPeriodRange[1]), tmFmt)
              nlRangeEnd <- as.Date(as.character(input$nlPeriodRange[2]), tmFmt)
            }
            
            step <- 31
          }else if(stringr::str_detect(input$nlType, "Y"))
          {
            tmFmt <- "%Y"
            
            if(!is.null(input$nlPeriodRange))
            {
              nlRangeStart <- lubridate::year(as.Date(as.character(input$nlPeriodRange[1]), tmFmt))
              nlRangeEnd <- lubridate::year(as.Date(as.character(input$nlPeriodRange[2]), tmFmt))
            }
            
            step <- 1
          }
          
          if(!is.null(input$nlPeriodRange))
          {
            if(is.na(nlRangeStart))
            {
              nlRangeStart <- minDate
              nlRangeEnd <- maxDate
            }
            
            if(nlRangeStart > minDate)
              startDate <- nlRangeStart
            
            if(nlRangeEnd < maxDate)
              endDate <- nlRangeEnd
          }
          
          shiny::sliderInput(inputId = "nlPeriodRange",
                      label = "Time",
                      min = minDate,
                      max = maxDate,
                      timeFormat = tmFmt,
                      step = step,
                      value = c(startDate, endDate),
                      animate = animationOptions(interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL)
          )
        }
      })
    })
    
    ######################## sliderNlPeriod ###################################
    
    output$sliderNlPeriod <- shiny::renderUI({
      #print(paste0("here: sliderNlPeriod"))
      
      ctryData <- ctryNlDataMelted()
      
      shiny::isolate({
        if (is.null(ctryData))
        {
          shiny::sliderInput(inputId = "nlPeriod",
                      label = "Time",
                      min = as.Date("2012-04-01", "%Y-%m-%d"),
                      max = as.Date("2017-10-31", "%Y-%m-%d"),
                      timeFormat = "%Y-%m",
                      step = 31,
                      value = as.Date("2012-04-01", "%Y-%m-%d")
          )
        }
        else
        {
          minDate <- min(ctryData$variable)
          maxDate <- max(ctryData$variable)
  
          if(stringr::str_detect(input$nlType, "D"))
          {
            tmFmt <- "%Y-%m-%d"
            
            if(!is.null(input$nlPeriod))
            {
              value <- as.Date(as.character(input$nlPeriod), tmFmt)
            }
            
            step <- 1
          }
          else if(stringr::str_detect(input$nlType, "M"))
          {
            tmFmt <- "%Y-%m"
            
            if(!is.null(input$nlPeriod))
            {
              value <- as.Date(as.character(input$nlPeriod), tmFmt)
            }
            
            step <- 31
          }
          else if(stringr::str_detect(input$nlType, "Y"))
          {
            tmFmt <- "%Y"
            
            if(!is.null(input$nlPeriod))
            {
              value <- lubridate::year(as.Date(as.character(input$nlPeriod), tmFmt))
            }
            
            step <- 1
          }
  
          if(!is.null(input$nlPeriod))
            value <- as.Date(as.character(input$nlPeriod), tmFmt)
          else
            value <- minDate
          
          shiny::sliderInput(inputId = "nlPeriod",
                             label = "Time",
                             min = minDate,
                             max = maxDate,
                             timeFormat = tmFmt,
                             step = step,
                             value = value,
                             animate = animationOptions(interval = 1000, loop = FALSE, playButton = "Play", pauseButton = NULL)
          )
        }
      })
    })
    
    ######################## hCluster ###################################
    
    hCluster <- shiny::reactive({
      print(paste0("here: reactive hCluster"))
      input$btnGo
      
      countries <- shiny::isolate(getInputCountries())
      
      if (is.null(countries) || length(countries) > 1)
        return()
      
      scale <- input$scale
      normArea <- input$norm_area
      
      shiny::isolate({
        nlPeriodRange <- input$nlPeriodRange
        graphType <- input$graphType
        admLevel <- unlist(ctryAdmLevels())[2]
  
        #return if the country doesn't have adm levels below country
        if (is.null(admLevel) || is.na(admLevel))
          return()
        
        #meltCtryData <- ctryNlDataMelted()
        
        meltCtryData <- ctryNlDataMeltedLvl2()
        
        if (is.null(countries) || is.null(meltCtryData))
          return()
        
        if (normArea)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
        
        #aggMeltCtryData <- stats::aggregate(mean(value), by=list(eval(admLevel)+variable), data=meltCtryData, mean)
        aggMeltCtryData <- stats::setNames(meltCtryData[,list(mean(value, na.rm = TRUE)), by = list(meltCtryData[[make.names(admLevel)]], variable)], c(admLevel, "variable", "value"))
        
        dcastFormula <- paste(paste0("`", admLevel, "`", collapse = " + "), "~", paste("variable", collapse = " + "))
        
        unmeltCtryData <- data.table::dcast(aggMeltCtryData, dcastFormula, value.var='value', aggregate='mean')
        
        d <- stats::dist(unmeltCtryData)
        
        h <- stats::hclust(d)
        
        h$labels <- unmeltCtryData[[make.names(admLevel)]]
        
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
      
      countries <- shiny::isolate(getInputCountries())
      
      if(length(countries) < 1)
        return()
      
      clusts <- hCluster()

      if(is.null(clusts))
        return()
      
      normArea <- input$norm_area
            
      admLevel <- unlist(ctryAdmLevels())[2]

      #return if the country doesn't have adm levels below country
      if (admLevel == "")
        return()
      
      numClusters <- input$kClusters
      scale <- input$scale
      
      isolate({
        meltCtryData <- ctryNlDataMeltedLvl2()
        
        if (normArea)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
        
        cutClusts <- stats::cutree(clusts, k=numClusters)
        
        #ctryAvg <- aggregate(value ~ admLevel, data=meltCtryData, mean)
        ctryAvg <- stats::setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[make.names(admLevel)]])], c(admLevel, "value"))
  
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        clusters = as.factor(cutClusts)
        
        g <- ggplot2::ggplot(data=ctryAvg,
                             aes(x=ctryAvg[[admLevel]],
                                 y=value, col=clusters
                                 )) +
          geom_point(size=2) +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
          ggplot2::scale_colour_manual(values=cbPalette) +
          xlab(admLevel) +
          ylab("Radiance")
        
          p <- plotly::ggplotly(g)
          p$elementId <- NULL
          p
      })
    })
    
    ######################## mapHCluster ###################################
    
    output$mapHCluster <- leaflet::renderLeaflet({
      print(paste0("here: draw mapHCluster"))
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      input$btnGo
      
      countries <- shiny::isolate(getInputCountries())
      scale <- input$scale
      numClusters <- input$kClusters
      normArea <- input$norm_area
      
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
        
        admLevel <- unlist(ctryAdmLevels())[2]
        
        meltCtryData <- ctryNlDataMeltedLvl2()
        
        if (normArea)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
        
        ctryPoly0 <- Rnightlights::readCtryPolyAdmLayer(countries, unlist(Rnightlights::getCtryShpLyrNames(countries,0)))
        
        #map <- leaflet::leaflet(data=ctryPoly0) %>%
        map <- leaflet::leaflet() %>%
          #addTiles("http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>%
          leaflet::addTiles() %>%
          #leaflet::addWMSTiles(layerId="nlRaster", baseUrl = "http://localhost/cgi-bin/mapserv?map=nightlights_wms.map", layers = "nightlights_201401", options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE, opacity=1)) %>%
          leaflet::addPolygons(data=ctryPoly0, layerId = countries, fill = FALSE, fillColor = "#fefe40", stroke = TRUE, weight=4, smoothFactor = 0.7, opacity = 1, color="white", dashArray = "5", group = "country")
        
        
        lvlCtryData <- stats::setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[make.names(admLevel)]])], c(admLevel, "value"))
        
        lvlCtryData[["rank"]] <- with(lvlCtryData, rank(-value, ties.method = 'first'))
        
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        pal <- cbPalette
        
        #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
        ctryPoly <- Rnightlights::readCtryPolyAdmLayer(countries, unlist(Rnightlights::getCtryShpLyrNames(countries, 1)))
        
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
    
    ######################## renderPlot plotTSDecomposed ###################################

    output$plotTSDecomposed <- shiny::renderPlot({
      #print(paste0("here: plotTSDecomposed"))
      input$btnGo
      
      countries <- shiny::isolate(getInputCountries())
      
      if(length(countries) < 1)
        return()
      
      normArea <- input$norm_area
      
      admLevel <- unlist(ctryAdmLevels())[2]
      
      #return if the country doesn't have adm levels below country
      if (is.null(admLevel) || admLevel == "")
        return()
      
      scale <- input$scale
      
      shiny::isolate({
        nlType <- input$nlType
        
        #meltCtryData <- ctryNlDataMeltedLvl2()
        meltCtryData <- ctryNlDataMelted()
        
        if(nrow(meltCtryData) < 2)
          return()
        
        if (normArea)
          meltCtryData$value <- meltCtryData$value/meltCtryData$area_sq_km

        ctryAvg <- meltCtryData
        #ctryAvg <- aggregate(value ~ country, data=meltCtryData, mean)
        #ctryAvg <- stats::setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[make.names(admLevel)]], variable)], c(admLevel, "variable", "value"))
        
        if(stringr::str_detect(nlType, "\\.D"))
          fmt <- "%Y-%M-%d"
        else if(stringr::str_detect(nlType, "\\.M"))
          fmt <- "%Y-%M-%d"
        else if(stringr::str_detect(nlType, "\\.Y"))
          fmt <- "%Y"

        minDate <- as.character(min(ctryAvg$variable))
        maxDate <- as.character(max(ctryAvg$variable))
        
        if(stringr::str_detect(nlType, "\\.Y") && stringr::str_detect(minDate, "\\d+$"))
        {
           minDate <- paste0(minDate, "-01-01")
           maxDate <- paste0(maxDate, "-01-01")
           freq <- 2
        }
        
        startYear <- lubridate::year(as.Date(minDate, fmt))
        endYear <- lubridate::year(as.Date(maxDate, fmt))

        if(startYear == endYear)
          stop(Sys.time(), ": Only 1 data point (year) in the dataset")
        
        tsStart <- c(startYear)
        tsEnd <- c(endYear)
        
        if(stringr::str_detect(nlType, "\\.D|\\.M"))
        {
          startMonth <- lubridate::month(lubridate::ymd(min(ctryAvg$variable)))
          endMonth <- lubridate::month(lubridate::ymd(max(ctryAvg$variable)))
          
          tsStart <- c(tsStart, startMonth)
          tsEnd <- c(tsEnd, endMonth)
          freq <- 12
        }
        
        if(stringr::str_detect(nlType, "\\.D"))
        {
          startDay <- lubridate::day(lubridate::ymd(max(ctryAvg$variable)))
          endDay <- lubridate::month(lubridate::ymd(max(ctryAvg$variable)))
          
          tsStart <- c(tsStart, startDay)
          tsEnd <- c(tsEnd, endDay)
          freq <- 7
        }
        
        ctryDataTS <- stats::ts(ctryAvg$value, start = tsStart, end = tsEnd, frequency = freq)

        ctryDataTScomponents <- stats::decompose(ctryDataTS)
        #g <- ggplot2::autoplot(ctryDataTScomponents)
        
        graphics::plot(ctryDataTScomponents)
      })
    })
    
    ######################## plotYearly ###################################
    
    output$plotYearly <- shiny::renderPlot({
      #print(paste0("here: renderPlotYearly"))
      input$btnGo
      
      countries <- shiny::isolate(getInputCountries())
      
      if (is.null(countries))
        return()
      
      scale <- input$scale
      nlPeriodRange <- input$nlPeriodRange
      graphType <- input$graphType
      normArea <- input$norm_area

      shiny::isolate({    
        nlType <- input$nlType
        
        ctryData <- ctryNlDataMeltedLvl2()
        
        if (is.null(countries) || is.null(ctryData))
          return()

          admLevel <- unlist(ctryAdmLevels())[1]
          
          #print(paste0("admLevel:", admLevel))
          
          if (!exists("admLevel") || is.null(admLevel) || length(admLevel)==0)
            admLevel <- "country"

          ctryData$year <- lubridate::year(as.Date(as.character(ctryData$variable), "%Y"))
          
          lstAggBy <- paste0("list(", admLevel, ", variable")
               
          lstAggBy <- paste0(lstAggBy, ", as.factor(year)")
          aggNames <- "year"
          
          if(stringr::str_detect(nlType, "\\.M"))
          {
            ctryData$month <- lubridate::month(as.Date(as.character(ctryData$variable)))
            lstAggBy <- paste0(lstAggBy, ", as.factor(month)")
            aggNames <- c(aggNames, "month")
          }
          
          lstAggBy <- paste0(lstAggBy, ")")
          
          if(stringr::str_detect(nlType, "\\.D"))
          {
            ctryData$day <- lubridate::day(as.Date(as.character(ctryData$variable)))
            lstAggBy <- paste0(lstAggBy, ", as.factor(day)")
            aggNames <- c(aggNames, "day")
          }
          
          #print(paste0("ctrydata nrow:", nrow(ctryData)))
          
          if (normArea)
            ctryData$value <- (ctryData$value)/ctryData$area_sq_km
          
          if (length(countries)==1)
          {
            #switched to data.table aggregation
            #ctryData <- stats::setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
            ctryData <- stats::setNames(
              ctryData[,list(mean(value, na.rm = TRUE)),
                       by = eval(parse(text=lstAggBy))],
              c(admLevel, "variable", aggNames, "value"))
            g <- ggplot2::ggplot(ctryData, aes(x=eval(parse(text=aggNames[length(aggNames)])), y=value, col=year, group=year)) + ggplot2::geom_line(alpha=0.3) + ggplot2::geom_point() + ggplot2::geom_smooth(aes(group=1),method = "loess", weight=1,alpha=0.2, lty='twodash') #+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #+ labs(col=year)
          }
          else
          {
            #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
            #switched to data.table aggregation
            ctryData <- stats::setNames(ctryData[,mean(value, na.rm = TRUE),by = eval(parse(text=lstAggBy))], c(admLevel, "variable", aggNames, "value"))
            #g <- ggplot2::ggplot(data=ctryData, aes(x=variable, y=value, col=country, group=year))
            g <- ggplot2::ggplot(ctryData, aes(x=eval(parse(text=aggNames[length(aggNames)])), y=value, col=country, shape=year, group=interaction(country,year))) + ggplot2::geom_line(lwd=.5, alpha=0.3) + ggplot2::geom_point()+ ggplot2::geom_smooth(aes(group=country),method = "loess", weight=1,alpha=0.2, lty='twodash')
          }
          
        if ("scale_y_log" %in% scale)
          g <- g + ggplot2::scale_y_log10()
        
        if ("scale_x_log" %in% scale)
          g <- g + ggplot2::scale_x_log10()
        
        if (normArea)
          g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Avg Rad (W.Sr^-1.cm^-2/Km2)") #y=expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2})))
        else
          g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Total Rad (W.Sr^-1.cm^-2)") #y=expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2}))
          
          #plotly::ggplotly(g)
          g
      })
    })
    
    ######################## plotNightLights ###################################
    
    output$plotNightLights <- plotly::renderPlotly({
      #print(paste0("here: renderPlot"))
      input$btnGo
      
      countries <- shiny::isolate(getInputCountries())
      
      if (is.null(countries) || length(countries) == 0 || countries == "")
        return()
      
      scale <- input$scale
      nlPeriodRange <- input$nlPeriodRange
      graphType <- input$graphType
      nlType <- shiny::isolate(input$nlType)
      normArea <- input$norm_area
      
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
        
        ctryAdmLevels <- unlist(ctryAdmLevels())
        admLevel <- ctryAdmLevels[as.numeric(data.table::last(admLvlNums))]
        
        #print(paste0("admLevel:", admLevel))
        
        if (!exists("admLevel") || is.null(admLevel) || length(admLevel)==0)
          admLevel <- "country"
          
      ctryData <- subset(ctryData, variable >= nlPeriodRange[1] & variable <= nlPeriodRange[2])
      
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
      
      if (input$norm_area)
        ctryData$value <- (ctryData$value)/ctryData$area_sq_km

      if (graphType == "boxplot")
      {
        if (length(countries)==1)
        {
          g <- ggplot2::ggplot(data=ctryData, ggplot2::aes(x=ctryData[[make.names(admLevel)]], y=value, col=ctryData[[make.names(admLevel)]])) + ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::labs(col=admLevel) + facet_grid(lubridate::year(variable) ~ lubridate::month(x=variable, label=T, abbr=T))
        }
        else
        {
          g <- ggplot2::ggplot(data=ctryData, ggplot2::aes(x=country, y=value, col=country)) + ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),panel.spacing.x=unit(0.1, "lines")) + ggplot2::labs(col=admLevel) + facet_grid(lubridate::year(variable) ~ lubridate::month(x=variable, label=T, abbr=T))
        }
        
        #ggplot2::ggplot(data = ctryData, ggplot2::aes(x = factor(variable), y = value, col = country)) + ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggplot2::labs(col = admLevel) + geom_boxplot() + facet_grid(country ~ .)
        
        g <- g + ggplot2::geom_boxplot()# +facet_grid(.~variable)
      }
      else if (graphType == "line")
      {
        if (length(countries)==1)
        {
          #switched to data.table aggregation
          #ctryData <- stats::setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
          ctryData <- stats::setNames(ctryData[,mean(value, na.rm = TRUE),by = list(ctryData[[make.names(admLevel)]], variable)], c(admLevel, "variable", "value"))
          g <- ggplot2::ggplot(data=ctryData, aes(x=variable, y=value, col=ctryData[[make.names(admLevel)]]))
          
          if(stringr::str_detect(nlType, "VIIRS"))
            g <- g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
        }
        else
        {
          #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
          #switched to data.table aggregation
          ctryData <- stats::setNames(ctryData[,mean(value, na.rm = TRUE),by = list(country, variable)], c("country", "variable", "value"))
          g <- ggplot2::ggplot(data=ctryData, aes(x=variable, y=value, col=country))
          
          if(stringr::str_detect(nlType, "VIIRS"))
            g <- g + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
        }

        g <- g+ ggplot2::geom_line() + ggplot2::geom_point()+ ggplot2::theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggplot2::labs(col=admLevel)
      }
      else if (graphType == "histogram")
      {
        #ctryData <- aggregate(value ~ country+variable, data=ctryData, mean)
        
        g <- ggplot2::ggplot(data=ctryData, aes(x=value))
        
        g <- g + ggplot2::geom_histogram(aes(y=..density..), bins = 30, colour="black", fill="white") + ggplot2::geom_density(alpha=.2, fill="#FF6666") + ggplot2::facet_grid(ctryData[[make.names(admLevel)]] ~ lubridate::year(variable)) # Overlay with transparent density plot

      }
      else
        return(NULL)

      if ("scale_y_log" %in% scale)
        g <- g + ggplot2::scale_y_log10()
      
      if ("scale_x_log" %in% scale)
        g <- g + ggplot2::scale_x_log10()
      
      if (normArea)
        g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Avg Rad (W.Sr^-1.cm^-2/Km2)") #y=expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2})))
      else
        g <- g + ggplot2::labs(title="Nightlight Radiances", x = "Month", y = "Total Rad (W.Sr^-1.cm^-2)") #y=expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2}))
      
        p <- plotly::ggplotly(g)
        p$elementId <- NULL
        p
      })
    })
    
    ######################## renderDataTable dataset ###################################
    
    output$dataset <- DT::renderDataTable({
      if(is.null(ctryNlData()))
        return()
      
      ctryData <- ctryNlData()
      
      allCols <- names(ctryData)
      
      dataCols <- grep("NL_", allCols, value = T)
      
      admCols <- setdiff(allCols, dataCols)
      
      if(!is.null(input$nlType))
        dataCols <- grep(input$nlType, dataCols, value = T)
        
      if(!is.null(input$nlPeriod))
        dataCols <- grep(input$nlPeriod, dataCols, value = T)
      
      if(!is.null(input$ctryStat))
        dataCols <- grep(input$ctryStat, dataCols, value = T)
      
      ctryData <- ctryData[, c(admCols, dataCols), with=F]
    },
      
      options = list(scrollX = TRUE, scrolly = TRUE)
    )
    
    ######################## observe map ###################################
#     observe({
#       if(!exists("nlPeriod"))
#         return()
#       
#       nlYm <- substr(gsub("-", "", nlPeriod[1]), 1, 6)
#       ctryPeriod <- paste0(countries, "_", nlYm)
#       
#       leafletProxy("map") %>%
#         clearTiles("nlRaster") %>%
#         addWMSTiles(baseUrl = "http://localhost/cgi-bin/mapserv?map=test.map", layers = ctryPeriod, options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=0.5), layerId="nlRaster")
#     })
    
#     observeEvent(input$admLevel, {
#       print(paste0("here: observe admLevel 2 update map"))
#       admLevel <- input$admLevel
#       countries <- input$countries
#       
#       if (input$drawMap == 0)
#         return()
#       
#       lyrs <- unlist(ctryAdmLevels())
#       
#       lyrNum <- which(lyrs == admLevel) - 1
#       
#       ctryPoly <- Rnightlights::readCtryPolyAdmLayer(countries, ifelse(is.null(admLevel),  yes = Rnightlights::getCtryShpLyrNames(countries,0), no = Rnightlights::getCtryShpLyrName(countries,lyrNum)))
#       s
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

      if(is.null(getInputCountries()) || is.null(input$nlPeriod) || is.null(input$nlType))
        return()

      countries <- shiny::isolate(getInputCountries())
      nlPeriod <- input$nlPeriod
      admLevel <- shiny::isolate(input$admLevel)
      scale <- input$scale
      nlType <- shiny::isolate(input$nlType)
      normArea <- input$norm_area
      
      #shiny::isolate({
      # if (is.null(countries) || is.null(nlPeriod) || is.null(admLevel))
      #   return()
      
      if (length(countries) == 0)
      {
        shiny::renderText("Please select only one country/region")
        return()
      }
      
      map <- NULL
      
      for(country in countries)
      {
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
      ctryAdmLevels <- Rnightlights:::getCtryStructAdmLevelNames(ctryCode = country, gadmVersion = input$polyVer, custPolyPath = NULL)

      #ctryAdmLevels <- unlist(ctryAdmLevels[which(countries == country)])
      
      lyrNum <- which(ctryAdmLevels == admLevel)
      
      #line weight increases. max=4 min=1
      deltaLineWt <- (4 - 1) / as.numeric(lyrNum)

      if(stringr::str_detect(nlType, "OLS"))
        nlYm <- nlPeriod
      else if (stringr::str_detect(nlType, "VIIRS"))
        nlYm <- as.Date(nlPeriod, "%Y%m%d")

      ctryData <- ctryNlDataMelted()
      
      if (is.null(ctryData))
        return()
      
      #get our data ready to match with polygons
      #subset data based on level selections
      if(stringr::str_detect(nlType, "OLS"))
        ctryData <- subset(ctryData, variable == nlYm)
      else if(stringr::str_detect(nlType, "VIIRS"))
        ctryData <- subset(ctryData, lubridate::year(variable) == lubridate::year(nlYm) & lubridate::month(variable) == lubridate::month(nlYm))

      if (normArea)
        ctryData$value <- (ctryData$value)/ctryData$area_sq_km

      #print(paste0("ctrydata nrow:", nrow(ctryData)))
      
      #print("drawing leaflet")
      
      ctryPeriod <- paste0(country, "_", nlYm)
      
      #message(ctryPeriod)
      
      ctryPoly0 <- Rnightlights::readCtryPolyAdmLayer(country, unlist(Rnightlights::getCtryShpLyrNames(country,0)))

      if(stringr::str_detect(nlType, "OLS"))
        nlPeriod <- substr(gsub("-","",nlYm),1,4)
      else if(stringr::str_detect(nlType, "VIIRS"))
        nlPeriod <- substr(gsub("-","",nlYm),1,6)
      
      ctryRastFilename <- Rnightlights::getCtryRasterOutputFnamePath(country, nlType, nlPeriod)
      
      if(file.exists(ctryRastFilename))
      {
        ctryRast <- raster::raster(ctryRastFilename)
        
        #raster::projection(ctryRast) <- wgs84
      }
      else
        ctryRast <- NULL
      
      map <- map %>% leaflet::leaflet()
        #leaflet::addTiles("http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png")
        
      if(country == countries[1])
        map <- map %>% leaflet::addTiles()
        
      if(inherits(ctryRast, "RasterLayer"))
      {
        map <- map %>% leaflet::addRasterImage(x = ctryRast,layerId = c("ctryRasterLocal_", country), group = "ctryRaster", project = T)

        leaflet::projectRasterForLeaflet(ctryRast)
      }
              
      #map <-  map %>% leaflet::addWMSTiles(layerId="nlRaster",
      #                       baseUrl = "http://localhost/cgi-bin/mapserv?map=nightlights_wms.map", 
      #                       layers = "ctryRasterWMS",
      #                       group = "ctryRaster",
      #                       options = leaflet::WMSTileOptions(format = "image/png",
      #                                                transparent = TRUE, opacity=1)
      #                       ) %>%
        
      map <- map %>%  leaflet::addPolygons(layerId = country,
                             fill = FALSE,
                             fillColor = "#fefe40",
                             stroke = TRUE,
                             weight=4,
                             smoothFactor = 0.7,
                             opacity = 1,
                             color="white",
                             dashArray = "5",
                             group = "country",
                             data=ctryPoly0
                             )

        selected <- NULL
        
        if(lyrNum > 1) #skip drawing the country level. avoid reverse seq (2:1)
        for(iterAdmLevel in 2:lyrNum)
        {
          #aggregate the data to the current level
          iterAdmLevelName <- ctryAdmLevels[iterAdmLevel]

          #data already in data.table form
          lvlCtryData <- stats::setNames(ctryData[,list(mean(value,na.rm=T), sum(area_sq_km, na.rm=T)), by=list(ctryData[[iterAdmLevelName]], ctryData[["variable"]])], c(iterAdmLevelName, "variable", "value", "area_sq_km"))

          #rank the data
          varname <- paste0('rank',iterAdmLevel)
          lvlCtryData[[varname]] <- with(lvlCtryData, rank(-value, ties.method = 'first'))
          
          #palette deciles for the layer
          bins <- unique(stats::quantile(lvlCtryData$value, seq(0,1,0.1), na.rm=T))
          brewerPal <- rev(RColorBrewer::brewer.pal(10, "YlOrRd"))
          pal <- leaflet::colorBin(brewerPal, domain = lvlCtryData$value, na.color = "grey", bins=bins)
          
          #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
          ctryPoly <- Rnightlights::readCtryPolyAdmLayer(country, unlist(Rnightlights::getCtryShpLyrNames(country, iterAdmLevel-1)))
          
          ctryPoly <- sp::spTransform(ctryPoly, wgs84)
          
          if (length(admLvlNums) > 0)
          if((iterAdmLevel) == data.table::last(admLvlNums)) #iterAdmLevel+1 %in% admLvlNums)
            selected <- which(ctryPoly@data[[paste0("NAME_",iterAdmLevel-1)]] %in% input[[paste0("selectAdm", iterAdmLevel)]])
          else
            selected <- c()

          mapLabels <- sprintf(
            paste0("<strong>%s:%s</strong>", "<br/>Area: %s km<superscript>2</superscript>","<br/>Date: %s", ifelse(normArea, "<br/>Rad: %s /sq.km", "<br/>Rad: %s"), "<br/>Rank: %s/%s"),
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
      }
      
      map <- map %>% leaflet::addLayersControl(overlayGroups = c("ctryRaster", ctryAdmLevels[2:lyrNum], "selected"))
      
      if (admLevel != "country")
        map <- map %>% leaflet::addLegend(position = "bottomright", 
                                 pal = pal, 
                                 values = format(ctryData$value, scientific = T),
                                 labels = stats::quantile(ctryData$value, seq(0,1,0.1), na.rm=T),
                                 #title = "Nightlight percentiles",
                                 title = ifelse(normArea, "Rad/sq. Km.", "Total Rad"),
                                 opacity = 1 )
#       #Zoom in disabled
#       if (exists("mapExtent"))
#         map <- map %>% fitBounds(mapExtent@xmin, mapExtent@ymin, mapExtent@xmax, mapExtent@ymax)
      
      map
      #})
    })
    
})
