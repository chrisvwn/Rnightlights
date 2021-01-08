######################## saveCredentialsEOG ###################################

#' Save credentials required for download from the EOG site
#'
#' Save credentials required for download from the new EOG site at University of
#'     Colorado, Dept of Mines
#'
#' @param credFile (character) Path to a file containing the credentials
#'     obtained from registration at 
#'     \url{https://eogauth.mines.edu/auth/realms/master/account/}
#'
#' @export
saveCredentialsEOG <-function(credFile = file.path(getNlDataPathFull(),
                                                   pkgOptions("EOG_CredFile")))
{
  ans <- -1
  
  while(ans != 0)
  {
    if (interactive())
    {
      prompt <-
        paste0(
          "Registration required for VIIRS satellite imagery access\n\n",
          "Please browse to ",
          pkgOptions("EOG_UserRegURL"),
          " and register an account.",
          "\n\nIf you already have the credentials use the menu ",
          "below to save the username and password.",
          "\n\nEnter 0 to Exit."
      )
      
      ans <-
        utils::menu(
          choices = c(
            "Save credentials",
            "View saved credentials",
            "Open EOG registration page in default browser"
          ),
          graphics = F,
          title = prompt
        )
    } else { #if not interactive
      stop("saveCredentialsEOGVIIRS() can only run interactively. Please run it manually")
    }
    
    if (ans == 1)
    {
      username <- ""
      password <- ""
      
      message("Please enter the email and password you registered at the EOG site")
      
      while(username == "")
      {
        username <- readline(prompt = "Username: ")
      }
      
      while(password == "")
      {
        password <- readline(prompt = "Password: ")
      }
      
      cat(paste(c("username", "password"), c(username, password), sep = ":"),
          file = credFile,
          sep = "\n")
      
    } else if (ans == 2)
    {
      creds <- getCredentialsEOG()
  
      if(length(creds) != 2)
      {
        message("No credentials found")
      } else
      {
        print(creds[1])
        print(creds[2])
      }
      readline("Press enter to continue ... ")
    } else if(ans == 3)
    {
      utils::browseURL("https://eogauth.mines.edu/auth/realms/master/account/")
    }else if (ans == 0)
    {
      creds <- getCredentialsEOG()
      
      if(length(creds) != 2)
      {
          
        msg <-
          paste0(
            "Credentials not found\n\n",
            "Registration required for VIIRS satellite imagery access\n\n",
            "Please browse to ",
            pkgOptions("EOG_UserRegURL"),
            " and register an account.",
            "\n\nIf you already have the credentials run 'saveCredentialsEOG()'",
            "interactively to save the username and password.",
            "\n\nRun saveCredentialsEOG() to save EOG download credentials"
          )
      }
    }
  }
}

######################## getCredentialsEOG ###################################

#' Retrieve credentials required for download from the EOG site
#'
#' Retrieve credentials required for download from the new EOG site at 
#'     University of Colorado, Dept of Mines
#'
#' @param credFile (character) Path to a file containing the credentials
#'     obtained from registration at 
#'     \url{https://eogauth.mines.edu/auth/realms/master/account/}
#'
#' @export
getCredentialsEOG <- function(credFile = file.path(getNlDataPathFull(), pkgOptions("EOG_CredFile")))
{
  if(!file.exists(credFile))
  {
    message(Sys.time(), ": EOG credential file not found")
    
    return(NULL)
  }
  
  creds <- readLines(con = credFile)
}

saveAuthTokenEOGResult <- function(authTokenEOGResult)
{
  .RnightlightsEnv$authTokenEOGResult <- authTokenEOGResult
}

existsAuthTokenEOG <- function()
{
  exists(x = "authTokenEOGResult", envir = .RnightlightsEnv)
}

getAuthTokenEOG <- function()
{
  if(existsAuthTokenEOG())
    return(.RnightlightsEnv$authTokenEOGResult$access_token)
}

getExistingRefreshTokenEOG <- function()
{
  if(existsAuthTokenEOG())
    return(.RnightlightsEnv$authTokenEOGResult$refresh_token)
}

getAuthTokenEOGRefreshToken <- function()
{
  if(existsAuthTokenEOG())
    return(.RnightlightsEnv$authTokenEOGResult$refresh_token)
}

getAuthTokenEOGRequestTime <- function()
{
  if(existsAuthTokenEOG())
    return(.RnightlightsEnv$authTokenEOGResult$reqTime)
}

getAuthTokenEOGExpireTime <- function()
{
  if(existsAuthTokenEOG())
    return(.RnightlightsEnv$authTokenEOGResult$expires_in)
}

getAuthRefreshTokenEOGExpireTime <- function()
{
  if(existsAuthTokenEOG())
    return(.RnightlightsEnv$authTokenEOGResult$refresh_expires_in)
}

expiredAuthTokenEOG <- function()
{
  requestTime <- getAuthTokenEOGRequestTime()
  expireTime <- getAuthTokenEOGExpireTime()
  
  #if request time + expire time is less than current time 
  #
  ifelse(!is.null(requestTime) && !is.null(expireTime),
         requestTime + expireTime <= Sys.time() - lubridate::as.duration("5 sec"),
         TRUE)
}

expiredAuthRefreshTokenEOG <- function()
{
  requestTime <- getAuthTokenEOGRequestTime()
  expireTime <- getAuthRefreshTokenEOGExpireTime()
  
  ifelse(!is.null(requestTime) && !is.null(expireTime),
         requestTime + expireTime <= Sys.time() - lubridate::as.duration("5 sec"),
         TRUE)
}

######################## reqAuthTokenEOG ###################################

#' Retrieve an access token required for download from the EOG site
#'
#' Retrieve a temporary access token required for the actual download from the
#'    new EOG site at University of Colorado, Dept of Mines
#'
#' @export
reqAuthTokenEOG <- function()
{
  #if we have an existing token and it is not expired
  if(existsAuthTokenEOG() && !expiredAuthTokenEOG())
  {
    message(Sys.time(), ": Download token available and not expired")
    
    access_token <- getAuthTokenEOG()
  } else #if we don't have an existing token or it is expired
  {
    #common fields required
    client_id <- pkgOptions("EOG_ClientID")
    client_secret <- pkgOptions("EOG_ClientSecret")
    
    h <- RCurl::basicTextGatherer()
    hdr <-  RCurl::basicHeaderGatherer()
    
    #If we have an existing token but it is expired and the refresh token is
    #not expired
    if(existsAuthTokenEOG() && expiredAuthTokenEOG() && !expiredAuthRefreshTokenEOG())
    {
      message(Sys.time(), ": Download token expired. Refreshing")
      
      refresh_token <- getExistingRefreshTokenEOG()
      
      req <- list(client_id=client_id,
                 client_secret=client_secret,
                 refresh_token=refresh_token,
                 grant_type='refresh_token')
      
    } else #we don't have an existing token or refresh token is expired. Request afresh
    {
      message(Sys.time(), ": Download token expired. Refresh token expired. Requesting new")
      creds <- getCredentialsEOG()
      
      while(length(creds) != 2)
      {
        message("Invalid EOG credentials")
        
        saveCredentialsEOG()
        
        creds <- getCredentialsEOG()
      }
      
      username <- unlist(strsplit(creds[1], ":"))[2]
      password <- unlist(strsplit(creds[2], ":"))[2]
      
      req <- list(client_id=client_id,
                 client_secret=client_secret,
                 username=username,
                 password=password,
                 grant_type='password')
    }
    
    req <- paste(names(req), req, sep = '=', collapse = '&')
    
    #body = enc2utf8(jsonlite::toJSON(req))
    
    body <- req
    
    h$reset()
    
    RCurl::curlPerform(
      url = pkgOptions("EOG_ClientAuthURL"),
      httpheader=c('Content-Type' = "application/x-www-form-urlencoded"),
      postfields=body,
      writefunction = h$update,
      headerfunction = hdr$update,
      verbose = TRUE
    )
    
    headers = hdr$value()
    
    httpStatus = headers["status"]
    
    access_token <- NULL
    
    if (httpStatus >= 400)
    {
      print(paste("The request failed with status code:", httpStatus, sep=" "))
      
      result <- jsonlite::fromJSON(h$value())
      
      print(paste("The error is: ", result))
      # 
      # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
      print(headers)
      
      print("Please ensure the username and password saved can login on the EOG site")
    } else
    {
      message(Sys.time(), ": Successfully retrieved access token")
      
      result <- jsonlite::fromJSON(h$value())
      
      access_token <- result$access_token
      
      result$reqTime <- Sys.time()
      
      #in case refresh_token fields are missing assume this means we should
      #use the previous values. May happen after refreshing
      if(is.null(result$refresh_token))
        result$refresh_token <- getExistingRefreshTokenEOG()
      
      if(is.null(result$refresh_expires_in))
        result$refresh_expires_in <- getAuthRefreshTokenEOGExpireTime()
      
      saveAuthTokenEOGResult(result)
    }
  }
  
  return(access_token)
}

######################## downloadNlTilesVIIRS ###################################

#' Download VIIRS nightlight tile
#'
#' Download VIIRS nightlight tile
#'
#' @param nlPeriod the nlPeriod of the tile to download
#'
#' @param tileNum the index of the tile as given by \code{getNlTiles}
#'
#' @param downloadMethod The method to use for download.
#'
#' @param nlType A character string of nlType
#'
#' @param configName character the config short name of raster being processed
#' 
#' @param extension character the extension of raster being processed
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(Rnightlights:::downloadNlTilesVIIRS("201401", "1"))
#'   print("download successful")
#'   }
#'
downloadNlTilesVIIRS <- function(nlPeriod,
                                 tileNum,
                                 downloadMethod = pkgOptions("downloadMethod"),
                                 nlType,
                                 configName = pkgOptions(paste0("configName_", nlType)),
                                 extension)
{
  if (missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if (missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if (missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if (!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType")
  
  if (!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  if (!validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  rsltDnld <- NA
  
  #get the zip local names
  ntLtsZipLocalNamePathVIIRS <-
    getNlTileZipLclNamePath(nlType = nlType,
                            configName = configName,
                            nlPeriod = nlPeriod,
                            tileNum = tileNum)
  
  #get the tif local names
  ntLtsTifLocalNamePathVIIRS <-
    getNlTileTifLclNamePath(nlType = nlType,
                            configName = configName,
                            extension = extension,
                            nlPeriod = nlPeriod,
                            tileNum = tileNum)
  
  #if the .tif doesn't exist download tgz tile. For aria and wget, if the tgz exists
  #it should attempt to complete it if incomplete else confirm it is complete and move
  #to extraction. For the other methods it will restart the download and overwrite
  if (!file.exists(ntLtsTifLocalNamePathVIIRS) && !file.exists(ntLtsZipLocalNamePathVIIRS))
  {
    ntLtsFileUrl <-
      getNlUrlVIIRS(nlPeriod = nlPeriod,
                    tileNum = tileNum,
                    nlType = nlType,
                    configName = configName)
    
    if (is.null(ntLtsFileUrl) || length(ntLtsFileUrl) == 0)
    {
      message(
        Sys.time(),
        ": ** Tile not available on the NOAA page.\n Please manually check for the ",
        nlPeriod,
        " tile for '",
        configName,
        " at ",
        pkgOptions(paste0("ntLtsInfoUrl", nlType)),
        "'. If it exists please report this as a bug **"
      )
      return(FALSE)
    }
    
    validDnldMethods <- c("auto", "curl", "libcurl", "wget", "aria")
    
    if (!(downloadMethod %in% validDnldMethods))
      downloadMethod <- "auto"
    
    access_token <- reqAuthTokenEOG()

    accessTokenHeader <- 
      if(downloadMethod == "auto")
      {
        list("Authorization" =  paste0("Bearer ", access_token))
      } else if(downloadMethod %in% c("curl", "libcurl"))
      {
        list("Authorization", paste0("Bearer ", access_token))
      } else if(downloadMethod == "wget")
      {
        list("Authorization", paste0("Bearer ", access_token))
      } else if(downloadMethod == "aria")
      {
        paste0('\"Authorization: Bearer ', access_token, '"')
      }
    
    if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
      rsltDnld <- utils::download.file(
        url = ntLtsFileUrl,
        destfile = ntLtsZipLocalNamePathVIIRS,
        mode = "wb",
        method = downloadMethod,
        extra = "-c",
        headers = accessTokenHeader
      )
    else if (downloadMethod == "aria")
      #downloads to path relative to -d if specified else local dir
      rsltDnld <-
      system(
        command = paste0(
          "aria2c -c -s2 -x", #continue downloads even if they were started elsewhere
          pkgOptions("numParDnldConns"),
          " --header ",
          accessTokenHeader,
          " --show-console-readout=false --summary-interval=10 ",
          ntLtsFileUrl,
          " -d ",
          getNlDir("dirNlTiles"),
          " -o ",
          getNlTileZipLclNameVIIRS(
            nlType = nlType,
            configName = configName,
            nlPeriod = nlPeriod,
            tileNum = tileNum
          )
        )
      )
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    message(Sys.time(), ": File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if (rsltDnld == 0)
  {
    message(Sys.time(), ": Extracting ", ntLtsZipLocalNamePathVIIRS)
    
    #for VIIRS.D
    if (nlType == "VIIRS.D" && exists("ntLtsFileUrl"))
    {
      lenZipLclName <- nchar(x = ntLtsFileUrl)
      extZipLclName <-
        substr(x = ntLtsFileUrl,
               start = lenZipLclName - 2,
               stop = lenZipLclName)
      
      if (tolower(extZipLclName) != "tgz")
      {
        file.rename(from = ntLtsZipLocalNamePathVIIRS, to = ntLtsTifLocalNamePathVIIRS)
      }
    } else if (!file.exists(
      getNlTileTifLclNamePathVIIRS(
        nlPeriod = nlPeriod,
        tileNum = tileNum,
        nlType = nlType,
        configName = configName,
        extension = extension
      )
    ))
    {
      message(Sys.time(),
              ": Getting list of files in ",
              ntLtsZipLocalNamePathVIIRS)
      
      tgzFileList <-
        utils::untar(tarfile = ntLtsZipLocalNamePathVIIRS,
                     list = TRUE,
                     tar = "internal")
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
      
      if (is.null(tgzFileList))
      {
        message(Sys.time(), ": Error extracting file list. ")
        
        return (FALSE)
      }
      
      #combined with section below to handle all VIIRS.* types
      # DELETE after confirmation
      # if(nlType == "VIIRS.Y")
      # {
      #   configShortName <- pkgOptions("configName_VIIRS.Y")
      #
      #   tgzAvgRadFilename <- tgzFileList[grep(paste0("svdnb.*.", configShortName, ".*.avg_rade9.*.tif$"),tgzFileList, ignore.case = T)]
      # } else
      # {
      #   if(nlType == "VIIRS.D")
      #     configShortName <- pkgOptions("configName_VIIRS.D")
      #   else if(nlType == "VIIRS.M")
      #     configShortName <- pkgOptions("configName_VIIRS.M")
      #
      #   tgzAvgRadFilename <- tgzFileList[grep(paste0("svdnb.*.", configShortName ,".*.avg_rade9.*.tif$"),tgzFileList, ignore.case = T)]
      # }
      
      tgzFileRgxp <- getNlFileRegex(nlTypes = nlType, configNames = configName,
                                    extensions = extension, regexNum = "2", nlPeriod = nlPeriod,
                                    tileName = tileIdx2Name(tileNum = tileNum, nlType = nlType),
                                    extension = extension
                                    )
      
      # tgzFileRgxp <- gsub(pattern = "{tileName}", replacement = tileIdx2Name(tileNum = tileNum, nlType = nlType), x = tgzFileRgxp, fixed = T)
      # 
      # tgzFileRgxp <- gsub(pattern = "{nlPeriod}", replacement = nlPeriod, x = tgzFileRgxp, fixed = T)
      # 
      # tgzFileRgxp <- gsub(pattern = "{extension}", replacement = extension, x = tgzFileRgxp, fixed = T)
      # 
      
      tgzAvgRadFilename <- tgzFileList[grep(tgzFileRgxp, tgzFileList, ignore.case = T)]
      
      # tgzAvgRadFilename <-
      #   tgzFileList[grep(
      #     pattern = paste0("svdnb.*.", configShortName , ".*.avg_rade9.*.tif$"),
      #     x = tgzFileList,
      #     ignore.case = T
      #   )]
      
      message(Sys.time(), ": Extracting ", tgzAvgRadFilename)
      
      if (!file.exists(
        getNlTileTifLclNamePathVIIRS(
          nlPeriod = nlPeriod,
          tileNum = tileNum,
          nlType = nlType,
          configName = configName,
          extension = extension
        )
      ))
      {
        utils::untar(
          tarfile = ntLtsZipLocalNamePathVIIRS,
          files = tgzAvgRadFilename,
          exdir = getNlDir("dirNlTiles"),
          tar = "internal"
        )
        
        file.rename(
          from = file.path(getNlDir(dirName = "dirNlTiles"), tgzAvgRadFilename),
          to = getNlTileTifLclNamePathVIIRS(
            nlPeriod = nlPeriod,
            tileNum = tileNum,
            nlType = nlType,
            configName = configName,
            extension = extension
          )
        )
        
        #unlink(ntLtsZipLocalNamePathVIIRS, force = TRUE)
      }
    }
    else
    {
      message(Sys.time(), ": TIF file found")
    }
  }
  else
  {
    message(Sys.time(), ": An error occurred downloading")
    return(FALSE)
  }
  
  return (rsltDnld == 0)
}

######################## downloadNlTilesOLS ###################################

#' Download OLS nightlight tile
#'
#' Download OLS nightlight tile
#'
#' @param nlPeriod the nlPeriod of the tile
#'
#' @param downloadMethod The method to use for download.
#'
#' @param nlType A character string of nlType
#'
#' @param configName character the config shortname of the raster to process
#' 
#' @param extension character the extension of the raster to process
#'
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#'
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' \dontrun{
#' if(Rnightlights:::downloadNlTilesOLS("201405"))
#'   print("download successful")
#'   }
#'
downloadNlTilesOLS <- function(nlPeriod,
                               downloadMethod = pkgOptions("downloadMethod"),
                               nlType = "OLS.Y",
                               configName = pkgOptions(paste0("configName_", nlType)),
                               extension,
                               multiTileStrategy = pkgOptions("multiTileStrategy"))
{
  if (missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if (!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  rsltDnld <- 0
  
  nlUrlsOLS <- getNlUrlOLS(nlType = nlType, nlPeriod = nlPeriod, configName = configName)
  
  if (is.null(nlUrlsOLS) || length(nlUrlsOLS) == 0)
  {
    message(
      Sys.time(),
      ": ** Tile not available on the NOAA page.\n Please manually check for the ",
      nlPeriod,
      " tile for '",
      configName,
      ", ",
      extension,
      " at ",
      pkgOptions(paste0("ntLtsInfoUrl", nlType)),
      "'. If it exists please report this as a bug **"
    )
    return(FALSE)
  }
  
  if (length(nlUrlsOLS) > 1)
  {
    message(
      Sys.time(),
      ": Multiple (",
      length(nlUrlsOLS),
      ") tiles found for ",
      nlType,
      ":",
      nlPeriod
    )
    
    if (multiTileStrategy == "first")
    {
      message(Sys.time(),
              ": MultiTile Strategy: Selecting tile: ",
              multiTileStrategy)
      nlUrlsOLS <- nlUrlsOLS[1]
    } else if (multiTileStrategy == "last")
    {
      message(Sys.time(),
              ": MultiTile Strategy: Selecting tile: ",
              multiTileStrategy)
      
      nlUrlsOLS <- nlUrlsOLS[length(nlUrlsOLS)]
    } else if (is.integer(multiTileStrategy))
    {
      message(Sys.time(),
              ": MultiTile Strategy: Selecting tile(s): ",
              multiTileStrategy)
      nlUrlsOLS <- nlUrlsOLS[multiTileStrategy]
    } else if (pkgOptions("multiTileStrategy") == "merge")
    {
      message(Sys.time(),
              ": MultiTile Strategy: Selecting all tiles: ",
              multiTileStrategy)
    }
  }
  
  ntLtsTifLocalNamePathOLS <-
    getNlTileTifLclNamePath(nlType = nlType,
                            nlPeriod = nlPeriod,
                            configName = configName,
                            extension = extension)
  
  if (!file.exists(ntLtsTifLocalNamePathOLS))
  {
    message(Sys.time(), ": Commencing download")
    
    for (i in seq_along(nlUrlsOLS))
    {
      rsltDnld <- NA
      
      ntLtsZipLocalNameOLS <- basename(nlUrlsOLS[i])
      # getNlTileZipLclNameOLS(nlType = nlType,
      #                        nlPeriod = nlPeriod,
      #                        configName = configName)
      ntLtsZipLocalNamePathOLS <- file.path(getNlDir(dirName = "dirNlTile"), ntLtsZipLocalNameOLS)
      # getNlTileZipLclNamePath(nlType = nlType,
      #                         nlPeriod = nlPeriod,
      #                         configName = configName)

      #get the zip and tif local names
      ntLtsZipLocalNameOLSTemp <- basename(nlUrlsOLS[i])
        # getNlTileZipLclNameOLS(nlType = nlType,
        #                        nlPeriod = nlPeriod,
        #                        configName = configName)
      ntLtsZipLocalNamePathOLSTemp <- file.path(getNlDir("dirNlTiles"), basename(nlUrlsOLS[i]))
        # getNlTileZipLclNamePath(nlType = nlType,
        #                         nlPeriod = nlPeriod,
        #                         configName = configName)
      ntLtsTifLocalNamePathOLSTemp <-
        getNlTileTifLclNamePath(nlType = nlType,
                                nlPeriod = nlPeriod,
                                configName = configName,
                                extension = extension)
      
      # ntLtsZipLocalNameOLSTemp <-
      #   gsub(pattern = "(\\.tar)",
      #        paste0("_", i, "\\1"),
      #        ntLtsZipLocalNameOLSTemp)
      # ntLtsZipLocalNamePathOLSTemp <-
      #   gsub(pattern = "(\\.tar)",
      #        paste0("_", i, "\\1"),
      #        ntLtsZipLocalNamePathOLSTemp)
      ntLtsTifLocalNamePathOLSTemp <-
        gsub(pattern = "(\\.tif)",
             paste0("_", i, "\\1"),
             ntLtsTifLocalNamePathOLSTemp)
      
      if (!file.exists(ntLtsTifLocalNamePathOLSTemp))
      {
        if (!file.exists(ntLtsZipLocalNamePathOLSTemp))
        {
          #get the first only to cater for Where multiple tiles exist
          ntLtsFileUrl <- nlUrlsOLS[i]
          
          ntLtsFileUrl <- gsub("\n", "", ntLtsFileUrl)
          
          validDnldMethods <-
            c(c("auto", "curl", "libcurl", "wget", "aria"))
          
          if (!(downloadMethod %in% validDnldMethods))
            downloadMethod <- "auto"
          
          access_token <- reqAuthTokenEOG()
          
          accessTokenHeader <- 
            if(downloadMethod == "auto")
            {
              list("Authorization" =  paste0("Bearer ", access_token))
            } else if(downloadMethod %in% c("curl", "libcurl"))
            {
              list("Authorization", paste0("Bearer ", access_token))
            } else if(downloadMethod == "wget")
            {
              list("Authorization", paste0("Bearer ", access_token))
            } else if(downloadMethod == "aria")
            {
              paste0('\"Authorization: Bearer ', access_token, '"')
            }
          
          message(
            Sys.time(),
            ": Downloading tile(",
            i,
            "/",
            length(nlUrlsOLS),
            "): ",
            ntLtsFileUrl
          )
          
          if (downloadMethod %in% c("auto", "curl", "libcurl", "wget"))
            rsltDnld <-
            utils::download.file(
              url = ntLtsFileUrl,
              destfile = ntLtsZipLocalNamePathOLSTemp,
              mode = "wb",
              method = downloadMethod,
              extra = "-c",
              headers = accessTokenHeader
            )
          else if (downloadMethod == "aria")
            #downloads to path relative to -d if specified else local dir
            rsltDnld <-
            system(
              paste0(
                "aria2c -c -s2 -x",
                pkgOptions("numParDnldConns"),
                " --header ",
                accessTokenHeader,
                " --show-console-readout=false --summary-interval=10 ",
                ntLtsFileUrl,
                " -d ",
                getNlDir("dirNlTiles"),
                " -o ",
                ntLtsZipLocalNameOLSTemp
              )
            )
        } else
        {
          rsltDnld <- 0
        }
      }
      else
      {
        #if the file is found we can return positive? Probably not unless there's an overwrite option
        #for our purposes return true
        message(Sys.time(),
                ": File exists, set Overwrite = TRUE to overwrite")
        
        rsltDnld <- 0
      }
      
      
      if (rsltDnld == 0)
      {
        message(Sys.time(),
                ": Extracting ",
                ntLtsZipLocalNamePathOLSTemp)
        
        tileNum <- "dummyTileNum"
        
        if (!file.exists(ntLtsTifLocalNamePathOLS))
        {
          message(Sys.time(),
                  ": Getting list of files in ",
                  ntLtsZipLocalNamePathOLSTemp)
          
          #get a list of files in the tar archive
          tarFileList <-
            utils::untar(ntLtsZipLocalNamePathOLSTemp,
                         list = TRUE,
                         tar = "internal")
          
          #get the nightlight data filename
          #https://ngdc.noaa.gov/eog/gcv4_readme.txt
          #F1?YYYY.v4[b|c]_cf_cvg.tif: Cloud-free coverages tally
          #F1?YYYY.v4[b|c]_avg_vis.tif: Raw avg_vis
          #F1?YYYY.v4[b|c]_stable_lights.avg_vis.tif: The cleaned up avg_vis
          #F1?YYYY.v4[b|c]_stable_lights.lights_pct.tif
          #F1?YYYY.v4[b|c]_avg_lights_x_pct.tif
          
          tgzFileRgxp <- getNlFileRegex(nlTypes = nlType, configNames = configName,
                                        extensions = extension, regexNum = "2", nlPeriod = nlPeriod)
          
          # tgzFileRgxp <- gsub(pattern = "{nlPeriod}", replacement = nlPeriod, x = tgzFileRgxp, fixed = T)
          
          tgzFile <- tarFileList[grep(tgzFileRgxp, tarFileList, ignore.case = T)]
          
          if (toupper(configName) %in% c("stable_lights") || toupper(extension) %in% toupper(c("cf_cvg", "avg_vis")))
          {
            #extract the nightlight gz data file
            utils::untar(
              tarfile = ntLtsZipLocalNamePathOLSTemp,
              files = tgzFile,
              exdir = getNlDir("dirNlTiles"),
              tar = "internal"
            )
            
            #the tif has the same name as the compressed file without the .gz
            tifFile <- stringr::str_replace(tgzFile, ".gz", "")
            
            #lights_pct and avg_lights_x_pct are not compressed
            
            message(Sys.time(), ": Decompressing ", tgzFile)
            
            if (!file.exists(ntLtsTifLocalNamePathOLSTemp))
              R.utils::gunzip(
                filename = file.path(getNlDir("dirNlTiles"), tgzFile),
                destname = ntLtsTifLocalNamePathOLSTemp,
                overwrite = TRUE
              )
            
            #unlink(ntLtsZipLocalNamePathOLS, force = TRUE)
          } else if (toupper(extension) %in% toupper(c("pct_lights", "avg_lights_x_pct")))
          {
            message(
              Sys.time(),
              ": Decompressing ",
              tgzFile,
              " and renaming to ",
              ntLtsTifLocalNamePathOLSTemp
            )
            
            #the tifs are not compressed so extract directly and rename
            if (!file.exists(tgzFile))
              utils::untar(
                tarfile = ntLtsZipLocalNamePathOLSTemp,
                files = tgzFile,
                exdir = getNlDir("dirNlTiles"),
                tar = "internal"
              )
            
            file.rename(file.path(getNlDir("dirNlTiles"), tgzFile),
                        ntLtsTifLocalNamePathOLSTemp)
          }
        }
        else
        {
          message(Sys.time(), ": TIF file found")
        }
      }
    }
    
    if (length(nlUrlsOLS) == 1)
    {
      message(Sys.time(), ": Renaming single tile")
      
      file.rename(ntLtsTifLocalNamePathOLSTemp,
                  ntLtsTifLocalNamePathOLS)
    } else
    {
      message(Sys.time(), ": Processing multiple tiles")
      
      wgs84 <- getCRS()
      
      ntLtsTifLocalNameOLS <-
        getNlTileTifLclNameOLS(nlType = nlType,
                               nlPeriod = nlPeriod,
                               configName = configName,
                               extension = extension)
      
      ntLtsTifLocalNamePathOLS <-
        getNlTileTifLclNamePath(nlType = nlType,
                                nlPeriod = nlPeriod,
                                configName = configName,
                                extension = extension)
      
      ntLtsTifList <- sapply(seq_along(nlUrlsOLS), function(i) {
        #get the zip and tif local names
        
        ntLtsTifLocalNamePathOLS <-
          gsub(pattern = "(\\.tif)",
               paste0("_", i, "\\1"),
               ntLtsTifLocalNamePathOLS)
      })
      
      message(Sys.time(), ": Merging Tifs")
      
      r <- raster::raster(x = ntLtsTifList[1])
      
      #get the extent and change to minx, miny, maxx, maxy order for use
      #in gdal_rasterize. Explanation below
      ext <- raster::extent(r)
      ext <- paste(ext[1], ext[3], ext[2], ext[4])
      
      #get the resolution of the raster. will be used in gdal_rasterize
      #for target resolution which should be the same as the source resolution.
      #Specifying makes it run faster (?)
      res <- paste(raster::res(r)[1], raster::res(r)[2])
      
      rm(r)
      
      outputFileVrt <-  gsub(".tif", ".vrt", ntLtsTifLocalNameOLS)
      
      outputFileVrt <-
        file.path(getNlDir("dirNlTemp"), outputFileVrt)
      
      if (file.exists(outputFileVrt))
        file.remove(outputFileVrt)
      
      message(Sys.time(), ": gdalwarp masking to VRT")
      
      gdalUtils::gdalbuildvrt(
        gdalfile = ntLtsTifList,
        output.vrt = outputFileVrt,
        te = as.character(ext),
        tr = as.character(res),
        tap = TRUE,
        a_srs = wgs84,
        multi = TRUE,
        wm = pkgOptions("gdalCacheMax"),
        wo = paste0("NUM_THREADS=", pkgOptions("numThreads"))
      )
      
      message(Sys.time(), ": gdal_translate converting VRT to TIFF ")
      gdalUtils::gdal_translate(#co = "LZW",
        src_dataset = outputFileVrt,
        dst_dataset = ntLtsTifLocalNamePathOLS)
      
      message(Sys.time(), ": Deleting the component rasters ")
      
      file.remove(ntLtsTifList)
      file.remove(outputFileVrt)
    }
  } else
  {
    message("Merged tile already exists")
  }
  
  return (rsltDnld == 0)
}

deleteNlTile <- function(nlType,
                         configName,
                         nlPeriod,
                         tileNum)
{
  if (missing(nlType))
    stop(Sys.time(), ": Missing required parameter nlType")
  
  if (!validNlTypes(nlType))
    stop(Sys.time(), ": Invalid nlType")
  
  if (!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(), ": Invalid nlPeriod: ", nlPeriod)
  
  if (!validNlTileNumVIIRS(tileNum, nlType))
    stop(Sys.time(), ": Invalid tileNum: ", tileNum)
  
  if (missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if (missing(tileNum))
    stop(Sys.time(), ": Missing required parameter tileNum")
  
  if(grepl(nlType, "VIIRS"))
  {
    #get the zip local names
    ntLtsZipLocalNamePathVIIRS <-
      getNlTileZipLclNamePath(nlType = nlType,
                              configName = configName,
                              nlPeriod = nlPeriod,
                              tileNum = tileNum)
    
    #get the tif local names
    ntLtsTifLocalNamePathVIIRS <-
      getNlTileTifLclNamePath(nlType = nlType,
                              configName = configName,
                              nlPeriod = nlPeriod,
                              tileNum = tileNum)
  }
}