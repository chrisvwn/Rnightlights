library(testthat)
library(Rnightlights)

internetAvailable <- function(testSite="google.com")
{
    !as.logical(system(paste("ping -n -c 1 ", testSite)))
}
