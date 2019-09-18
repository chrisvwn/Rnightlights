
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')
# 
# pacman::p_load(shiny, shinydashboard, plotly)
# 
# pacman::p_load_gh("rstudio/leaflet", "cloudyr/aws.s3") #get the github version of leaflet
# 
# suppressMessages(library(shiny))
# 
# suppressMessages(library(shinydashboard))
# 
# suppressMessages(library(leaflet))
# 

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

                                  
        shiny::uiOutput(outputId = "countries"),
        
        shiny::uiOutput(outputId = "btnGo"),
                                  
        shinydashboard::menuItem("Inputs", selected = TRUE, startExpanded = TRUE, tabName = "inputs",

                 shiny::uiOutput(outputId = "selectAdmLevel"),
                 
                 shiny::uiOutput("radioAdmLevel")
                 ),

        shinydashboard::menuItem("Stats", selected = FALSE, startExpanded = TRUE, tabName = "stats",
                                 shiny::uiOutput(outputId = "nlType"),
                                 
                                 shiny::uiOutput(outputId = "ctryStats"),
                                 
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

                          shiny::checkboxInput(inputId = "strict", label = "Strict", value = T),
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
      shinydashboard::dashboardBody(
        shinydashboard::tabBox(width = 12,
          shiny::tabPanel(title = "Plot",
                          style = tags$style("overflow-y:auto; height: 76vh; position:relative;"),
                          
                          shiny::plotOutput(outputId = "plotNightLights"),
                          
                          shiny::uiOutput("sliderNlPeriodRange")
          ),

          shiny::tabPanel(title = "Map",
                          tags$style("overflow-y:auto; height: 80vh; position:relative;"),
                          
                          shinycssloaders::withSpinner(leaflet::leafletOutput("map")),
                          
                          shiny::uiOutput("sliderNlPeriod")
          ),

          shiny::tabPanel(title = "Stats",
                   shiny::fluidRow(
                     shinydashboard::box(title = "Annual Trends", 
                         shiny::plotOutput("plotYearly")),
                     
                     shinydashboard::tabBox(
                       shiny::tabPanel(title = "Cluster Points",
                                       shinycssloaders::withSpinner(plotly::plotlyOutput("plotPointsCluster"))
                                ),
                       
                       shiny::tabPanel(title = "Cluster Hierarchy",
                                       shinycssloaders::withSpinner(shiny::plotOutput("plotHCluster"))
                                ),
                       shiny::tabPanel(title = "Cluster Map",
                                       shinycssloaders::withSpinner(leaflet::leafletOutput("mapHCluster"))
                       ),
                       shiny::sliderInput("kClusters", "Num Clusters", min=1, max=10, value=2)
                     )
                   ),
                  
                  shiny::fluidRow(
                    shinydashboard::tabBox(
                      shiny::tabPanel(title = "Time Series Decomposed",
                                      shinycssloaders::withSpinner(shiny::plotOutput("plotTSDecomposed"))
                      
                      )
                    )
                  )
                ),
          
          shiny::tabPanel(title = "models",
                   shiny::textOutput("Models")
                   ),
          
          shiny::tabPanel(title = "data",
                  DT::dataTableOutput("dataset")
                   )
        )
      )
    )