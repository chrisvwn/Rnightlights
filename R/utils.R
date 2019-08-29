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
  valid <- unlist(sapply(testData, function(x) eval(parse(text="testFun(x, ...)"))))
  
  invalidData <- testData[!valid]
  
  if(length(invalidData) > 0)
    message(Sys.time(), ": Invalid data: ", paste0(invalidData, collapse = ", "))
  
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
  if(Sys.info()[["sysname"]] == "Windows")
  {
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    freeMem <- gsub("\r", "", x, fixed = TRUE)
    
    freeMem <- as.integer(freeMem)
    
  } else if(Sys.info()[["sysname"]] == "macOS")
  {
    freeBlocks <- system2("vm_stat | grep free | awk '{ print $3 }' | sed 's/\\.//'", stdout = TRUE)
    speculativeBlocks <- system2("vm_stat | grep speculative | awk '{ print $3 }' | sed 's/\\.//'")
    freeMem <- (freeBlocks + speculativeBlocks)*4096
    
    freeMem <- as.integer(freeMem)
    
  } else if(Sys.info()[["sysname"]] == "Linux")
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
  sysFreeRAM <- getFreeRAM() * 2^10
  
  if(grepl("%", freeRAM))
  {
    freeRAM <- as.numeric(gsub("%","",freeRAM))
    
    freeRAM <- freeRAM/100
    
    freeRAM <- freeRAM*sysFreeRAM
  } else if(grepl("KB", freeRAM))
  {
    freeRAM <- as.numeric(gsub("KB","",freeRAM))
    
    freeRAM <- freeRAM*2^10
  } else if(grepl("MB", freeRAM))
  {
    freeRAM <- as.numeric(gsub("MB","",freeRAM))
    
    freeRAM <- freeRAM*2^20
  } else if(grepl("GB", freeRAM))
  {
    freeRAM <- as.numeric(gsub("GB","",freeRAM))
    
    freeRAM <- freeRAM*2^30
  }
  
  if(freeRAM > sysFreeRAM)
    freeRAM <- 0.1 * sysFreeRAM * 2^20

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
nlCleanup <- function(temp=TRUE, tileCache=FALSE)
{
  #remove any global vars we created in .onLoad
  #suppressWarnings(rm(map, shpTopLyrName, wgs84, nlTiles, tilesSpPolysDFs))
 
  #message("Cleaning up the environment")
   
  #the destructor
  
  #del temp files used in this session in the nlTempDir
  unlink(list.files(getNlDir("dirNlTemp"), full.names = TRUE), recursive = TRUE, force = TRUE)
  
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
printCredits <- function(credits, newLineChar="\n", surroundChar = "*", horzPadding = 1, vertPadding = 1, horzWidth = 3, vertWidth = 2)
{
  width <- getOption("width")
  
  if(newLineChar != "\n")
    credits <- gsub(pattern = "\n", replacement = "", x = credits)
  
  credits <- unlist(strsplit(x = credits, split = newLineChar, fixed = T))
  
  sideFrame <- paste(rep(surroundChar, horzWidth), collapse = "")  
  
  longestLine <- max(sapply(credits, nchar))
  
  fullHorzFrame <- paste(rep(surroundChar, longestLine + horzPadding * 2 + horzWidth * 2), collapse = "")
  
  emptyHorzFrame <- paste0(sideFrame, paste0(rep(" ", longestLine + horzPadding * 2), collapse=""), sideFrame)
  
  header <- rep(fullHorzFrame, vertWidth)
  
  footer <- header
  
  mainBody <- sapply(credits, USE.NAMES = F, function(x){
    leftPad <- floor((longestLine-nchar(x))/2)
    
    rightPad <- ifelse((longestLine-nchar(x)) %% 2 == 0, leftPad, leftPad + 1)
    
    leftSpace <- paste(rep(" ", leftPad+horzPadding), collapse = "")
    
    rightSpace <- paste(rep(" ", rightPad+horzPadding), collapse = "")
    
    # print((longestLine-nchar(x)))
    # print(paste0(leftPad, ":", rightPad))
    out <- if(!grepl(pattern = "^\\s*$", x = x))
    {
      paste0(sideFrame, leftSpace, x, rightSpace, sideFrame)
    }else
    {
      if(vertWidth > 0)
        c(emptyHorzFrame, fullHorzFrame, emptyHorzFrame)
      else
        fullHorzFrame
    }
    # message("'", leftSpace,"'")
    # message("'", x, "'")
    # message("'", rightSpace,"'")
    
    #paste0(out,"\n")
  })
  
  credits <- c(header, emptyHorzFrame, unlist(mainBody), emptyHorzFrame, footer)
  
  #cat(paste(credits, collapse = "\n"))
  
  credits <- sapply(credits, function(cred)
  {
    ws <- paste(rep(" ", floor((width - nchar(cred))/2)), collapse = "")
    
    paste(ws, cred, sep = "", collapse = "")
  }, USE.NAMES = F)
  
  packageStartupMessage(paste(credits, collapse = "\n"))
}
