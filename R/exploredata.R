######################## exploreData ###################################

#' Run a web application to explore the processed nightlight data cached
#'     locally
#'
#' Run a web application to perform some exploratory data analysis. The
#'     application written in shiny either loads demo data or the data 
#'     processed and cached locally.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' exploreData()
#' }
#'
#' @export
exploreData <- function()
{
  appDir <- system.file("application", package = "Rnightlights")
  if (appDir == "")
  {
    stop(Sys.time(), ": Could not find application directory.
         Try re-installing the `Rnightlights` package.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}