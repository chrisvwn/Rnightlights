######################## setupDataPath ###################################

#' Interactively allows the user to set up the default root path
#'
#' Interactively allows the user to set up the default root path where data
#'     is cached
#'
#' @param newDataPath Default root path to set
#' 
#' @param ... Not used.
#'
#' @return character string Returns (invisibly) the root path,
#'     or @NULL if running a non-interactive session.
#' 
#' @seealso Internally, @see "setNlDataPath" is used to set the root path.
#'     The \code{"base::interactive"} function is used to test whether 
#'     \code{R} is running interactively or not.
#'
#' @export
setupDataPath <- function(newDataPath=tempdir(), ...)
{
  dirName <- ".Rnightlights"
  
  defaultPath <- file.path("~")
  
  dataPath <- getNlDataPath()
  
  if (missing(newDataPath))
  {
    if (is.null(dataPath))
    {
      #if we don't find a data path and none is supplied ask the user where to create it
      #both null; ask user
      if (interactive())
      {
        prompt <- paste0("The Rnightlights package needs to create a directory ",
                         "that will hold package files and data which may be large.",
                         "\n\nPlease choose a location where this directory will ",
                         "be created. Recommend 3GB+ For multiple countries & periods.",
                         "If none is chosen a temporary directory will be used ",
                         "for this session only.",
                         "\n\nWould you like to choose a different data directory?",
                         "\n\nEnter 0 to use a temporary directory for this session only.")
        
        
        ans <- utils::menu(choices = c(paste0("Create data path under home directory '",  
                                              path.expand("~")), 
                                       "Choose a different directory as the data path"),
                           graphics = F, title = prompt);
        
        if (ans == 1)
          dataPath <- defaultPath
        else if (ans == 2)
          dataPath <- tryCatch(
            {
              path <- tcltk::tk_choose.dir("~")
            }, 
            error=function(ex) 
            {
              path <- readline("Please enter the directory path: ")
              if (dir.exists(path))
                return (path)
              else
                return(NULL)
            })
        else if (ans == 0)
        {
          message(Sys.time(), ": Using temporary directory for this session only")
          dataPath <- tempdir()
        }
        
        if (is.null(dataPath) || is.na(dataPath))
        {
          message(Sys.time(), ": Exiting. dataPath not set: Re-run to set/change dataPath")
          return(invisible(getNlDataPath()))
        }
      }
      else
      {
        #if not interactive and the dataPath isn't set, set dataPath to tempdir()
        dataPath <- tempdir()
        
        message(Sys.time(), ": Creating data folder in temporary location ", path.expand(file.path(dataPath, dirName)))
      }
      
      setNlDataPath(dataPath)
      #invisible(dataPath)
    }
    else
    {
      #if a directory currently exists ask the user if they want to change it
      if (interactive())
      {
        prompt <- paste0("The Rnightlights package needs to create a directory ",
                         "that will hold package files and data which may be large.",
                         "\n\nPlease choose a location where this directory will ",
                         "be created. Recommend 3GB+ For multiple countries & periods.",
                         "If none is chosen a temporary directory will be used ",
                         "for this session only.",
                         "\n\nWould you like to choose a different data directory?",
                         "\n\nEnter 0 to use a temporary directory for this session only.")
        
        
        ans <- utils::menu(choices = c(paste0("Use current directory '",  
                                              path.expand(getNlDataPath()), " as the data path"), 
                                       "Choose a different directory as the data path"),
                           graphics = F, title = prompt);
        
        if (ans == 1)
          return(invisible(getNlDataPath()))
        else if (ans == 2)
        {
          dataPath <- tryCatch(
          {
            path <- tcltk::tk_choose.dir(getNlDataPath())
          }, 
          error=function(ex) 
          {
            path <- readline("Please enter the directory path: ")
            if (dir.exists(path))
              return (path)
            else
              stop(path, " not found")
          })
          
          if (is.null(dataPath) || is.na(dataPath))
          {
            message(Sys.time(), ": Exiting. dataPath not set: Re-run to set/change dataPath")
            return(invisible(getNlDataPath()))
          }
          
          #this is a move
          setNlDataPath(dataPath)
        }
        else if (ans == 0)
        {
          message(Sys.time(), ": Using temporary directory for this session only")
          dataPath <- tempdir()
        }
        
        #is.na if dialog cancelled, is.null if readline empty
        if (is.null(dataPath) || is.na(dataPath)) 
        {
          message(Sys.time(), ": Exiting. dataPath not set: Re-run to set/change dataPath")
          return(getNlDataPath())
        }
      }
      else
      {
        dataPath <- getNlDataPath()
        if(!is.null(dataPath))
        {
          message(Sys.time(), ": Using previous install detected at ", path.expand(file.path(dataPath, dirName)))
          
          setNlDataPath(tempdir())
        }
      }
      
      #setNlDataPath(dataPath)
    }
  }
  else
  {
    if(is.null(dataPath))
    {
      #create dir in newDataPath
      message(Sys.time(), ": Creating default directory")
      setNlDataPath("~")
      
      message(Sys.time(), ": Creating data directory")
      setNlDataPath(newDataPath)
    }
    else
    {
      message(Sys.time(), ": Attempting data directory move")
      setNlDataPath(newDataPath)
      
      #invisible(newDataPath)
    }   
  }
  
  dataPath <- getNlDataPath()
  
  return(invisible(dataPath))
} # setupCacheDataPath()

######################## setNlDataPath ###################################
#' Sets the root path to the package data directory
#'
#' By default, this function will set the root path to \code{~/.Rnightlights/}.
#'
#' @param dataPath The path
#'
#' @return
#'   Returns (invisibly) the old root path.
#'   
#' @examples
#' \dontrun{
#' Rnightlights:::setNlDataPath("/new/path")
#' }
#' 
setNlDataPath <- function(dataPath)
{
  if(missing(dataPath))
    stop(Sys.time(), ": Missing required parameter dataPath")
  
  if (!is.character(dataPath) || is.null(dataPath) || is.na(dataPath) || dataPath == "")
    stop(Sys.time(), ": dataPath must be a valid character string")
  
  dataPath <- as.character(dataPath)
  
  dataDirName <- ".Rnightlights"
  
  homePath <- file.path("~", ".Rnightlights")
  
  existingPath <- getNlDataPath()
  
  #if existingPath is not null we already have an existing directory. This is potentially a move
  if (!is.null(existingPath))
  {
    #if the supplied directory is the same as the current dataPath stop. Nothing to do
    if(path.expand(dataPath) == path.expand(existingPath))
    {
      message(Sys.time(), ": The directories are the same. Not changing")
      return(invisible(dataPath)) #return user version. less expensive
    }
    else #if they are different we will move
    {
      isMove <- TRUE
    }
  }
  else #is a new install
  { 
    isMove <- FALSE
  }
  
  #create the dataPath
  if(dir.exists(dataPath))
  {
    dirCreate <- file.path(dataPath, dataDirName)
    
    successCreate <- tryCatch({
      dir.create(dirCreate)
    }, error=function(err)
    {
      message(Sys.time(), ": Error: ", err)
      return(FALSE)
    }, warning=function(war)
    {
      message(Sys.time(), ": Warning: ", war)
      return(FALSE)
    })
    
    if(!successCreate)
      message(Sys.time(), ": Unable to create directory ", dirCreate)
    else
    {
      message(Sys.time(), ": Data directory created ", path.expand(dirCreate))
      message(Sys.time(), ": Rnightlights may require 3GB+. Run setupDataPath() to change the location")
    }
  }
  else
    stop(Sys.time(), ": Directory ", dataPath, " not found")
  
  #If we are here we have created a new directory
  #If dataPath not the tempdir(), Make sure the homePath exists and persist the dataPath
  #~/.Rnightlights Must always exist even if it does not hold the data
  if(dataPath != tempdir() && !exists(file.path(homePath)))
    if(dir.exists(file.path(homePath)) || dir.create(file.path(homePath)))
      if(!isMove) #only change the path if not a move since move may fail
        saveRDS(path.expand(dataPath), file.path(homePath, "datapath.rda"))
  
  #only if this is a move
  if(isMove && dataPath != tempdir())
  {
    message(Sys.time(), ": Moving dataPath .Rnightlights from ", existingPath, " to ", dataPath)
    
    copySuccess <- tryCatch({
      file.copy(file.path(existingPath, dataDirName), file.path(dataPath), recursive = TRUE)
    }, error = function(err)
    {
      message(Sys.time(), ": Error: ", err, "\n")
      return(FALSE)
    }, warning = function(war)
    {
      message(Sys.time(), ": Warning: ",war, "\n")
      return(FALSE)
    })
    
    #copy the .Rnightlights folder to newDataPath
    if(copySuccess)
    {
      #persist the changed data path
      saveRDS(path.expand(dataPath), file.path(homePath, "datapath.rda"))
      
      #if the old directory was the default dir in the home dir then do not attempt to delete old directory
      if(path.expand(existingPath) == path.expand("~"))
      {
        #remove the datapath.rda from the new path
        if(dataPath != "~")
          if(file.exists(file.path(dataPath, dataDirName, "datapath.rda")))
            file.remove(file.path(dataPath, dataDirName, "datapath.rda"))
        
        #remove the _RNIGHTLIGHTS_SAFE_TO_DELETE file from the new path
        if(file.exists(file.path(dataPath, dataDirName, "_RNIGHTLIGHTS_SAFE_TO_DELETE")))
          file.remove(file.path(dataPath, dataDirName, "_RNIGHTLIGHTS_SAFE_TO_DELETE"))
        
        message(Sys.time(), ": Move of datapath from ", existingPath, " to ", dataPath, " complete.")
      }
      else #else mark the dir for deletion and prompt user to delete it
      {
        #unlink(dataPath, recursive = T, force = T)
        delText <- "This is an old Rnightlights package data directory. It is safe to delete this directory."
        readr::write_file(delText, file.path(existingPath, dataDirName, "_RNIGHTLIGHTS_SAFE_TO_DELETE"))
        
        #if the new location was an old data dir the _RNIGHTLIGHTS_SAFE_TO_DELETE file might still be present. Delete it.
        if(file.exists(file.path(dataPath, dataDirName, "_RNIGHTLIGHTS_SAFE_TO_DELETE")))
          file.remove(file.path(dataPath, dataDirName, "_RNIGHTLIGHTS_SAFE_TO_DELETE"))
        
        #remove the copied datapath.rda if it exists. Usually if the datapath is moving from the default location i.e. home dir
        if(dataPath != "~")
          file.remove(file.path(dataPath, dataDirName, "datapath.rda"))
        
        message(Sys.time(), ": Move of datapath from ", existingPath, " to ", dataPath, " complete.")
        message(Sys.time(), ": You may now delete ", file.path(existingPath, dataDirName))
      }
    }
    else
    {
      if(dataPath != tempdir())
      {  
        #roll back copy
        message(Sys.time(), ": Rolling back partial copy")
        
        successRollback <- tryCatch(
          {
            unlink(file.path(dataPath, dataDirName), recursive = TRUE)
          }, error = function(err)
          {
            message(Sys.time(), ": Error: ", err, "\n")
            return(FALSE)
          }, warning = function(war)
          {
            message(Sys.time(), ": Warning: ", war, "\n")
            return(FALSE)
          })
        
        if(successRollback == 0)
          message(Sys.time(), ": Rolled back. Please fix errors and try again.")
        else
          message(Sys.time(), ": Rollback failed. Please manually delete folder ", file.path(dataPath, dataDirName))
      }
    }
  }
  
  #If dataPath was created
  if(!is.null(getNlDataPath()))
    if(path.expand(getNlDataPath()) == path.expand(dataPath))
    {
      # Add a README.txt file, if missing.
      addREADME(to=file.path(dataPath, dataDirName))
      
      #add data-version.txt if a new install
      #also prevents upgrade from running first time
      if(!isMove && !file.exists(file.path(dataPath,dataDirName,"data-version.txt")))
        setDataVersion(path=file.path(dataPath, dataDirName), pkgVersion = as.character(utils::packageDescription("Rnightlights")$Version))
      
      #create the package dirs
      createNlDataDirs()
    }
  
  getNlDataPath()
} # setNlDataPath()

######################## getNlDataPath ###################################

#' Gets the root path to the file directory"
#'
#' Gets the root path to the file directory"
#'
#' @return Returns the folder containing the root of the current data path
#'     as a @character string.
#'
#' @examples
#'   print(getNlDataPath())
#'
#' @seealso To set the directory where package data files are stored,
#'     see @see "setNlDataPath".
#'     
#' @export
getNlDataPath <- function()
{
  
  homePath = path.expand("~")
  dirName = ".Rnightlights"
  dataPathFile = "datapath.rda"
  
  if(dir.exists(file.path(tempdir(), dirName)))
    return(file.path(tempdir()))
  
  if (dir.exists(file.path(homePath, dirName)))
  {
    if (file.exists(file.path(homePath, dirName, dataPathFile)))
    {
      dataPath <- readRDS(file = file.path(homePath, dirName, dataPathFile))
      
      if (!dir.exists(dataPath))
      {
        dataPath <- homePath
        
        RnightlightsDataPath <- homePath
        saveRDS(path.expand(dataPath), file.path(homePath, "datapath.rda"))
      }
    }
    else
      dataPath <- homePath
  }
  else
  {
    #finally check if the .Rnightlights folder exists under tempdir()
    #meaning user chose temporary location
    if(dir.exists(file.path(tempdir(), dirName)))
      dataPath <- tempdir()
    else
      dataPath <- NULL #if all else fails send NULL
  }
  
  dataPath
}

######################## removeDataPath ###################################

#' Deletes a root data path all sub-directories
#'
#' Deletes a root data path and all sub-directories. It can be the current
#'     directory or a previously used data path. It will only delete if it
#'     has the default folder structure of a root data path.
#'
#' @param dataPath \code{character} The path to the root folder to be deleted
#' 
#' @param confirm \code{logical} Used when in non-interactive mode. If missing or FALSE
#'     the operation will be aborted.
#'     
#' @return None
#'
#' @examples
#'   \dontrun{
#'   Rnightlights:::removeDataPath(getNlDataPath())
#'   }
#'     
removeDataPath <- function(dataPath = file.path(getNlDataPath(), ".Rnightlights"), confirm=FALSE)
{
  if(basename(dataPath) != ".Rnightlights")
    stop(Sys.time(), ": You must specify the full path including .Rnightlights")
  
  if(interactive())
  {
    menuPrompt <- paste0("You are about to remove the Rnightlights data folder in \n", dataPath, ". Do you want to continue?")
    
    response <- utils::menu(choices = c("Yes", "no"), graphics = F, title = menuPrompt)
  }else
  {
    response <- 0
    
    if(confirm)
      response <- 1
  }
  
  if(response == "1")
  {
    unlink(dataPath, recursive = T, force = T)
    message(Sys.time(), ": Removed dataPath")
  }
  else if(response == "2")
    message(Sys.time(), ": Not deleting")
  else if(response == "0")
    message(Sys.time(), ": Aborted")
}

######################## addREADME ###################################

#' Add README file to the root data path
#'
#' Add README file to the root data path
#'
#' @param to The folder to add the README file to
#'     
#' @return None
#'
#' @examples
#'   \dontrun{
#'   Rnightlights:::addREADME()
#'   }
#'     
addREADME <- function(to=getNlDataPath())
{
  # Add a README.txt to dataPath (expaining what the directory is)
  filename <- "README.txt"
  
  pathnameD <- file.path(to, filename)
  
  if (!file.exists(pathnameD))
  {
    pathnameS <- system.file("_Rnightlights", package="Rnightlights")
    
    file.copy(pathnameS, pathnameD)
  }
} # addREADME()

######################## setDataVersion ###################################

#' Add data version file to the root data path
#'
#' Add data version file to the root data path
#'
#' @param path The folder to add the README file to
#' 
#' @param pkgVersion The version of the package
#'     
#' @return None
#'
#' @examples
#'   \dontrun{
#'   Rnightlights:::setDataVersion(version="0.2.4")
#'   }
#'     
setDataVersion <- function(path=getNlDataPath(), pkgVersion = utils::packageDescription("Rnightlights")$Version)
{
  # Add a data-version.txt to dataPath (to show the data directory version)
  filename <- "data-version.txt"
  
  pathnameD <- file.path(path, filename)
  
  cat(pkgVersion, file = pathnameD)
  
} # setDataVersion()

######################## createNlDataDirs ###################################

#' Create required data subdirectories in the root data path
#'
#' Create required data subdirectories in the root data path
#'
#' @return None
#'
#' @examples
#'   \dontrun{
#'   Rnightlights:::createNlDataDirs()
#'   }
#'     
createNlDataDirs <- function()
{
  #set directory paths (tiles, ctrypoly, output/cropped rasters, downloads/temp?)
  
  #create directories
  if(!dir.exists(getNlDir("dirPolygon")))
    dir.create(getNlDir("dirPolygon"))
  
  if(!dir.exists(getNlDir("dirNlTiles")))
    dir.create(getNlDir("dirNlTiles"))
  
  if(!dir.exists(getNlDir("dirNlGasFlares")))
    dir.create(getNlDir("dirNlGasFlares"))
  
  if(!dir.exists(getNlDir("dirNlData")))
    dir.create(getNlDir("dirNlData"))
  
  if(!dir.exists(getNlDir("dirRasterOutput")))
    dir.create(getNlDir("dirRasterOutput"))
  
  if(!dir.exists(getNlDir("dirRasterWeb")))
    dir.create(getNlDir("dirRasterWeb"))
  
  if(!dir.exists(getNlDir("dirNlTemp")))
    dir.create(getNlDir("dirNlTemp"))
  
  if(!dir.exists(getNlDir("dirZonals")))
    dir.create(getNlDir("dirZonals"))
}

######################## getNlDir ###################################

#' Get the paths to the various data locations
#' 
#' Get the paths to the various locations of nightlights data generated by
#'     the Rnightlights package. These correspond to the various "dir..." 
#'     options in the pkgOptions settings
#'
#' @param dirName character vector The name of the directory to retrieve
#' 
#' @examples
#' getNlDir("dirRasterOutput")
#' 
#' getNlDir("dirNlTiles")
#' 
#' getNlDir("dirPolygon")
#' 
#' getNlDir("dirZonals")
#' 
#' @export
getNlDir <- function(dirName)
{
  if(missing(dirName))
    stop(Sys.time(), ": Missing required parameter dirName")
  
  if(!is.character(dirName) || is.null(dirName) || is.na(dirName) || dirName == "")
    stop(Sys.time(), ": Invalid dirName: ", dirName)
  
  #check if dataPath is already set
  dataPath <- getNlDataPath()
  
  #If getNlDataPath() returns NULL prompt the user to setupDataPath()
  #Put here in case at installation user chooses temp directory so in
  #a new R session it will be NULL. We also want all exported functions to
  #work without explicitly loading the package
  if(is.null(dataPath))
  {
    setupDataPath()
    
    dataPath <- getNlDataPath()
  }
  
  if(dirName == "dirNlDataPath")
    nlDir <- file.path(dataPath, pkgOptions("dirNlRoot"))
  else
    nlDir <- file.path(dataPath, pkgOptions("dirNlRoot"), pkgOptions(dirName))
  
  return(nlDir)
}