suppressMessages(library(plotly)) #some functions don't work unless library is imported

missingPkgs <- NULL
 
if (!requireNamespace("Rnightlights", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "Rnightlights")
}

if (!requireNamespace("shiny", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "shiny")
}

if (!requireNamespace("shinydashboard", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "shinydashboard")
}

if (!requireNamespace("leaflet", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "rstudio/leaflet")
}

if (!requireNamespace("plotly", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "plotly")
}

if (!requireNamespace("DT", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "DT")
}

if (!requireNamespace("shinycssloaders", quietly = TRUE))
{
  missingPkgs <- c(missingPkgs, "shinycssloaders")
}

if(!is.null(missingPkgs))
  stop(Sys.time(), ": Missing packages needed for this function to work. 
       Please install missing packages: '", paste0(missingPkgs, collapse = ", "), "'", call. = FALSE)

# existingData <- Rnightlights::listCtryNlData()
# 
# ctryCodesWithData <- unique(existingData$ctryCode)
# 
# ctryCodeNames <- lapply(ctryCodesWithData, function(x) Rnightlights::ctryCodeToName(x))
# 
# ctryCodesWithData <- stats::setNames(ctryCodesWithData, ctryCodeNames)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

shinydashboard::dashboardPage(

  # Application title
  shinydashboard::dashboardHeader(title="Nightlights"),

  # Sidebar with a slider input for number of bins
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(style = "overflow-y:auto; height: 92vh; position:relative;",

      shiny::uiOutput(outputId = "textDate"),
      
      shiny::uiOutput(outputId = "countries"),
      
      shiny::uiOutput(outputId = "btnGo"),
                                
      shinydashboard::menuItem("Inputs", selected = TRUE, startExpanded = TRUE, tabName = "inputs",

               shiny::uiOutput(outputId = "selectAdmLevel"),
               
               shiny::uiOutput("radioAdmLevel")
               ),

      shinydashboard::menuItem("Stats", selected = FALSE, startExpanded = TRUE, tabName = "stats",
                               shiny::uiOutput(outputId = "nlType"),
                               
                               shiny::uiOutput(outputId = "ctryStats"),
                               
                               shiny::checkboxInput(inputId = "optionNaRm",
                                                    label = "na.rm",
                                                    value = TRUE
                               ),

                               shiny::uiOutput(outputId = "newStat"),
                               
                               shiny::uiOutput(outputId = "btnNewStat"),
                               
                               shiny::uiOutput(outputId = "polySrc"),
                               
                               shiny::uiOutput(outputId = "polyVer"),
                               
                               shiny::uiOutput(outputId = "polyType"),
                               
                               shiny::uiOutput(outputId = "configName"),
                               
                               shiny::uiOutput(outputId = "multiTileMergeStrategy"),
                               
                               shiny::uiOutput(outputId = "multiTileMergeFun"),
                               
                               shiny::uiOutput(outputId = "removeGasFlares"),
                               
                               shiny::checkboxInput(inputId = "norm_area",
                                                         label = "norm_area",
                                                         value = FALSE
                               )
      ),

               shinydashboard::menuItem(text = "Options", tabName = "options",

                        #shiny::checkboxInput(inputId = "strict", label = "Strict", value = T),
                        shiny::radioButtons(inputId = "graphType",
                                     label = "Graph type",
                                     choices = c("line", "boxplot", "histogram", "point"),
                                     selected = "line",
                                     inline = T
                        ),
                        
                        shiny::checkboxGroupInput(inputId = "scale",
                                      label = "Scale",
                                      choices = c("scale_x_log", "scale_y_log")
                        )
              )
      )
    ),

    # body
    shinydashboard::dashboardBody(height="80vh",
      shiny::tabsetPanel(id = "tabs",
        shiny::tabPanel(title = "Plot",
                        style = tags$style("overflow-y:auto; height: 76vh; position:relative;"),
                        
                        shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plotNightLights", height = "82vh")),
                        
                        shiny::uiOutput(outputId = "sliderNlPeriodRange")
        ),

        shiny::tabPanel(title = "Map",
                        style = tags$style("overflow-y:auto; height: 80vh; position:relative;"),
                        
                        shinycssloaders::withSpinner(leaflet::leafletOutput(outputId = "map", height = "82vh")),
                        
                        shiny::absolutePanel(draggable = T, top = "65vh", left = "7%",
                                             shiny::uiOutput(outputId = "sliderNlPeriod"))
        ),

        shiny::tabPanel(title = "Stats",
                 shiny::fluidRow(
                   shinydashboard::box(title = "Annual Trends", 
                       shiny::plotOutput(outputId = "plotYearly")),
                   
                   shinydashboard::tabBox(
                     shiny::tabPanel(title = "Cluster Points",
                                     shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = "plotPointsCluster"))
                              ),
                     
                     shiny::tabPanel(title = "Cluster Hierarchy",
                                     shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plotHCluster"))
                              ),
                     shiny::tabPanel(title = "Cluster Map",
                                     shinycssloaders::withSpinner(leaflet::leafletOutput(outputId = "mapHCluster"))
                     ),
                     shiny::sliderInput("kClusters", "Num Clusters", min=1, max=10, value=2)
                   )
                 ),
                
                shiny::fluidRow(
                  shinydashboard::tabBox(
                    shiny::tabPanel(title = "Time Series",
                                    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "plotTSDecomposed"))
                    
                    )
                  )
                )
              ),
        
        shiny::tabPanel(title = "Models",
                 shiny::textOutput(outputId = "models")
                 ),
        
        shiny::tabPanel(title = "Loaded Data",
                DT::dataTableOutput(outputId = "dataset")
                 ),
        
        shiny::tabPanel(title = "Available Data",
                        tags$style("height: 80vh; position:relative;"),
                        DT::dataTableOutput(outputId = "availableData")
        )
      )
    )
  )