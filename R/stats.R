######################## validNlStats ###################################

#' Check if given statistics are valid
#'
#' Check if given statistics are valid. A valid statistic is one which
#'    is a function available in the current environment and returns
#'    a valid value to match.fun
#'
#' @param nlStats the statistics to check
#'
#' @return named logical TRUE/FALSE
#'
#' @examples
#'
#' Rnightlights:::validNlStats(c("sum", "mean"))
#'  #returns TRUE TRUE
#'
#' Rnightlights:::validNlStats("unknownFunction")
#'  #returns FALSE
#'
validNlStats <- function(nlStats)
{
  if (missing(nlStats))
  {
    message(Sys.time(), ": Missing required parameter nlStats")
    
    return(FALSE)
  }
  
  if ((!is.character(nlStats) &&
       !is.list(nlStats)) || length(nlStats) == 0)
  {
    message(Sys.time(), ": Invalid nlStats")
    
    return(FALSE)
  }
  
  if (is.list(nlStats) &&
      length(nlStats) > 1 &&
      all(sapply(2:length(nlStats), function(i)
        ! is.list(nlStats[[i]]) &&
        (grepl("=", nlStats[i]) || length(names(nlStats[i])) > 0))))
    nlStats <- list(nlStats)
  
  matchedFuns <- sapply(nlStats, function(nlStat)
    tryCatch({
      if (is.list(nlStat) &&
          all(sapply(2:length(nlStat), function(i)
            grepl("=", nlStat[i]) || length(names(nlStat[i])) > 0)))
        matched <-
          is.character(nlStat[[1]]) && !is.null(match.fun(nlStat[[1]]))
      else
        matched <-
          is.character(nlStat) && !is.null(match.fun(nlStat))
    }, error = function(err)
    {
      #sep when a=b without a name may cause fn(=param=arg...)
      message(Sys.time(), ": ", paste0(
        "Invalid nlStat: ",
        gsub(
          "\\(=|,=|funArgs=",
          "\\(",
          paste0(
            as.character(nlStat[[1]]),
            "(",
            paste(
              names(nlStat[-1]),
              nlStat[-1],
              sep = "=",
              collapse = ","
            ) ,
            ")"
          )
        )
      ))
      matched <- FALSE
      return(matched)
    }))
  
  return(matchedFuns)
}

######################## nlStatParams ###################################

#' Get the parameters of an nlStat
#'
#' Get the parameters of an nlStat
#'
#' @param nlStatName the name of the nlStat to process
#'
#' @return named character A vector of parameters
#'
#' @examples
#'
#' Rnightlights:::nlStatParams("sum")
#'
#' #returns
#' #...   na.rm = FALSE
#' #"..."       "na.rm"
#'
nlStatParams <- function(nlStatName)
{
  fmls <- names(formals(eval(parse(text = nlStatName))))
  
  #formals cannot inspect .Primitive functions
  #so try args
  if (is.null(fmls))
  {
    fmls <- trimws(utils::capture.output(args(nlStatName)))
    
    #take out the last bracket which is the last char
    fmls <- unlist(strsplit(fmls, "function\\s*\\("))[2]
    fmls <- substr(fmls, 1, nchar(fmls) - 1)
    fmls <- trimws(unlist(strsplit(fmls, ",")))
    fmls <- unlist(strsplit(fmls, ","))
    
    #split by = to separate name from value
    fmls <-
      sapply(fmls, function(x)
        trimws(unlist(strsplit(x, "="))[1]))
  }
  
  fmls
}

######################## nlStatArgs ###################################

#' Get the arguments passed to an nlStat
#'
#' Get the arguments passed to an nlStat when specifying it
#'
#' @param nlStat the name of the nlStat to process
#'
#' @return named character A vector of arguments passed to the nlStat
#'
#' @examples
#'
#' nlStat <- list("sum", "na.rm=TRUE")
#'
#' Rnightlights:::nlStatArgs(nlStat = nlStat)
#'
#' #returns "na.rm=TRUE"
#'
nlStatArgs <- function(nlStat)
{
  #TODO:
  #unmatched arg when only one left out e.g. rnorm(mean=0,sd=1,5)
  #unmatched args where ... can take multiple e.g. sum(1,2,3,4,5)
  
  if (length(nlStat) == 0)
    return("")
  
  if (length(nlStat) == 1)
  {
    if (!is.list(nlStat))
      return("")
    
    nlStat <- nlStat[[1]]
    
    if (length(nlStat) == 1)
      return("")
  }
  
  #if args are passed as named lists e.g. list("sum", na.rm=TRUE) convert each named argument
  #to the string format "arg=value"
  if (!is.null(names(nlStat)))
  {
    args <-
      paste(names(nlStat)[2:length(nlStat)], nlStat[2:length(nlStat)], sep = "=")
    
    #cater for empty names in args where you have a mix of named and explicit args
    #empty names result in args starting with "=" e.g. "=na.rm=TRUE"
    #removing "=" prefix
    args <- gsub("(^|,)=", "\\1", args)
    
    nlStat <- list(nlStat[[1]], args)
  }
  
  #1st param is the function name, the rest are params
  params <- paste(unlist(nlStat[2:length(nlStat)]), collapse = ",")
  
  params1 <- unlist(strsplit(params, ","))
  
  #split by = to separate name from value
  params2 <-
    lapply(params1, function(x)
      trimws(unlist(strsplit(x, "="))))
  
  #length 1 is unnamed param
  paramLengths <- sapply(params2, length)
  
  paramNames <-
    sapply(params2, function(x)
      if(length(x) == 1)
        ""
      else
        x[[1]])
  
  paramValues <-
    sapply(params2, function(x)
      if(length(x) == 1)
        x[[1]]
      else
        x[[2]])
  
  fmls <- nlStatParams(nlStat[[1]])
  
  unnamedParams <- paramValues[which(paramLengths == 1)]
  
  #unnamedParamsPos <- sapply(unnamedParams, function(x) which(params1 == params2[x]))
  
  unnamedParams <-
    stats::setNames(unnamedParams, fmls[which(paramNames == "")])
  
  namedParams <- which(paramLengths == 2)
  
  namedParams <-
    stats::setNames(paramValues[namedParams], paramNames[namedParams])
  
  #match the first part (param) with the formal args
  #number is position, NA is not direct match so possibly ... but
  #if no ... found then keep in position
  matchFmls <- fmls[match(paramNames, fmls)]
  
  matchedNamedParams <-
    stats::setNames(paramValues[which(!is.na(matchFmls))], paramNames[which(!is.na(matchFmls))])
  
  unMatchedNamedParams <-
    sapply(params2[paramNames != "" &
                     !(paramNames %in% names(matchedNamedParams))], paste, collapse = "=")
  
  if (length(unMatchedNamedParams) > 0)
  {
    if ("..." %in% fmls)
    {
      unMatchedNamedParams <-
        stats::setNames(unMatchedNamedParams, rep("...", length(unMatchedNamedParams)))
    } else
    {
      unMatchedNamedParams <-
        paste("xUnMatchedx",
              unMatchedNamedParams,
              sep = "=",
              collapse = ",")
    }
  }
  
  #aim is to get a unique, consistent list of params that we can use
  #to match even if parameters are in a different order
  allParams <-
    c(
      if (length(unnamedParams) > 0)
        unnamedParams
      else
        NULL,
      if (length(matchedNamedParams) > 0)
        matchedNamedParams
      else
        NULL,
      if (length(unMatchedNamedParams) > 0)
        unMatchedNamedParams
      else
        NULL
    )
  
  dups <- duplicated(allParams)
  
  allParams <- allParams[!dups]
  
  #get order of args according to formal params
  paramIdxs <-
    unlist(sapply(fmls, function(x)
      which(names(allParams) == x), USE.NAMES = F, simplify = T))
  
  #put the supplied args in order
  if (length(paramIdxs) > 0)
  {
    allParams <- allParams[paramIdxs]
    
    allParams <-
      paste(names(allParams),
            allParams,
            sep = "=",
            collapse = ",")
  }
  
  #uniqueParams <- paste(uniqueParams, unMatchedNamedParams, collapse = ",", sep = "")
  
  allParams
}

######################## nlSignatureAddArg ###################################

#' Add an argument to an nlSignature
#'
#' Add an argument to an nlSignature
#'
#' @param nlStatSigs character A vector of nlStat signatures to which to add
#'     the argument
#'
#' @param addArg character The argument to add to the signatures
#'
#' @return named character A vector of modified nlStat signatures
#'
#' @examples
#'
#' nlStat <- list("sum")
#'
#' nlSig <- Rnightlights:::nlStatSignature(nlStat)
#'
#' Rnightlights:::nlSignatureAddArg(nlStatSigs = nlSig, addArg = "na.rm = TRUE")
#'
#' #returns
#' #            sum()
#' # "sum(na.rm=TRUE)"
#'
nlSignatureAddArg <- function(nlStatSigs, addArg)
{
  addArg <- nlStatArgsStandardize(addArg)
  
  sapply(nlStatSigs, function(nlStatSig)
  {
    #convert the signature into an nlStat list
    nlStat <- nlSignatureStat(nlStatSignature = nlStatSig)
    
    if (length(nlStat) == 1 && length(unlist(nlStat)) == 1)
      nlStat <- list(nlStat[[1]], addArg)
    else if (length(nlStat) == 1 && length(unlist(nlStat) == 2))
      nlStat <-
        list(list(nlStat[[1]][[1]], paste(
          nlStat[[1]][2], addArg, collapse = "", sep = ","
        )))
    else if (length(nlStat) == 2)
      nlStat <-
        list(list(nlStat[[1]], paste(
          nlStat[[2]], addArg, collapse = "", sep = ","
        )))
    
    #convert back to a signature. this should also remove duplicate args
    newNlStatSig <- nlStatSignature(nlStat)
    
    if (any(grepl("xUnMatchedx", nlStat)))
    {
      message(Sys.time(),
              ": Incompatible argument detected: ",
              nlStatSig)
      return(nlStatSig)
    }
    
    newNlStatSig
  })
}

######################## nlStatSignature ###################################

#' Get the signature of an nlStat
#'
#' Get the signature of an nlStat i.e. the nlStat function with all arguments
#'     passed in by the user. Used to uniquely identify an nlStat function call
#'     i.e. the same function called with different arguments results in a
#'     different signature
#'
#' @param nlStat character The name of an nlStat
#'
#' @return named character The signature of the nlStat
#'
#' @examples
#'
#' nlStat <- list("sum")
#'
#' nlSig <- Rnightlights:::nlStatSignature(nlStat)
#'
#' #returns "sum()"
#'
#' nlStat <- list("sum", "na.rm=TRUE")
#'
#' nlSig <- Rnightlights:::nlStatSignature(nlStat)
#'
#' #returns "sum(na.rm=TRUE)"
#'
nlStatSignature <- function(nlStat)
{
  statArgs <- nlStatArgs(nlStat)
  
  statArgs <- nlStatArgsStandardize(statArgs)
  
  paste0(nlStat[[1]][[1]], "(", statArgs, ")")
}

######################## nlStatArgsStandardize ###################################

#' Standardize the arguments of an nlStat
#'
#' Standardize the arguments of an nlStat to ensure signatures can be
#'     compared. Currently this entails converting
#'     logical `T` to TRUE and `F` to FALSE
#'
#' @param nlStatArg character The argument of an nlStat
#'
#' @return named character The signature of the nlStat
#'
#' @examples
#'
#' Rnightlights:::nlStatArgsStandardize("na.rm=T")
#'
#' #returns "na.rm=TRUE"
#'
nlStatArgsStandardize <- function(nlStatArg)
{
  #standardize TRUE/FALSE
  #replace any standalone T ie not single or double quoted, not preceded or followed by any
  #alnum; may have space before and/or after; may be at the beginning or end of string
  nlStatArg <-
    gsub("(^|[^'\"a-zA-Z0-9])\\s*T\\s*([^'\"a-zA-Z0-9]|$)",
         "\\1TRUE\\2",
         nlStatArg)
  
  nlStatArg <-
    gsub("(^|[^'\"a-zA-Z0-9])\\s*F\\s*([^'\"a-zA-Z0-9]|$)",
         "\\1FALSE\\2",
         nlStatArg)
  
  nlStatArg
}

######################## nlSignatureStat ###################################

#' Convert a signature into an nlStat
#'
#' Convert a signature into an nlStat
#'
#' @param nlStatSignature character The signature of an nlStat
#'
#' @return named character An nlStat
#'
#' @examples
#'
#' nlSignature <- "sum(na.rm=T)"
#'
#' Rnightlights:::nlSignatureStat(nlStatSignature = nlSignature)
#'
#' #returns an nlStat list
#' #[[1]]
#' #[[1]][[1]]
#' #[1] "sum"
#' #
#' #[[1]][[2]]
#' #[1] "na.rm=TRUE"
#' #
#' #list structure
#' #List of 1
#' #$ :List of 2
#' #..$ : chr "sum"
#' #..$ : chr "na.rm=TRUE"
#'
nlSignatureStat <- function(nlStatSignature)
{
  #the stat name is the first part before first bracket
  nlStatName <- gsub("(^.*)\\(.*", "\\1", nlStatSignature)
  
  #params are the rest of the signature
  params <- gsub(nlStatName, "", nlStatSignature)
  
  #remove the brackets
  nlStatArgs <- substr(x = params, 2, nchar(params) - 1)
  
  #if there are args
  if (nlStatArgs != "")
  {
    #remove ...= which is not a legit arg
    statArgs <- gsub("...=", "", nlStatArgs, fixed = T)
    
    #standardize TRUE/FALSE
    statArgs <- nlStatArgsStandardize(statArgs)
    
    list(list(nlStatName, statArgs))
  } else
  {
    #if no args just return the func name as a list
    list(nlStatName)
  }
}

######################## prettyNlSignature ###################################

#' Remove triple-dots (...) from names of arguments in a signature
#'
#' Remove triple-dots (...) from names of arguments in a signature
#'
#' @param nlStatSig character The signature of an nlStat
#'
#' @return named character An nlStat signature without the triple-dots
#'
#' @examples
#'
#' nlStat <- list("sum", "x=10", "na.rm=T")
#'
#' nlStatSig <- Rnightlights:::nlStatSignature(nlStat) #"sum(...=x=10,na.rm=TRUE)"
#'
#' Rnightlights:::prettyNlSignature(nlStatSig = nlStatSig)
#' #returns "sum(x=10,na.rm=TRUE)"
#'
prettyNlSignature <- function(nlStatSig)
{
  gsub(
    pattern = "...=",
    replacement = "",
    nlStatSig,
    fixed = T
  )
}

######################## getSavedNlStatFname ###################################

#' The name of the file in which the saved nlStats are stored
#'
#' The name of the file in which the saved nlStats are stored
#'
#' @return character The name of the saved nlStats file
#'
getSavedNlStatFname <- function()
{
  "savedNlStats.rda"
}

######################## getSavedNlStatFname ###################################

#' The path tos the file in which the saved nlStats are stored
#'
#' The path to the file in which the saved nlStats are stored
#'
#' @return character The path to the saved nlStats file
#'
getSavedNlStatFnamePath <- function()
{
  file.path(getNlDir("dirNlData"), getSavedNlStatFname())
}

######################## saveNlStat ###################################

#' Save an nlStat function
#'
#' Save an nlStat function to the persistent storage so that it can be
#'     retrieved and reused later even if the user no longer has its
#'     definition. Especially useful when saving the results from an
#'     nlStat so that the function used to retrieve the data is always
#'     available.
#'
#' @param nlStat character An nlStat
#'
#' @return logical Whether the process was successful
#'
#' @examples
#'
#' nlStat <- list("sum", "na.rm=TRUE")
#'
#' saveNlStat(nlStat)
#'
#' @export
saveNlStat <- function(nlStat)
{
  funcName <- nlStat[[1]]
  
  nlStatSig <- nlStatSignature(nlStat = nlStat)
  
  nlStatArgs <- nlStatArgs(nlStat = nlStat)
  
  nlStatBody <- eval(parse(text = funcName))
  
  nlStatHash <- hashNlStat(nlStatName = funcName)
  
  if (!validNlStats(funcName))
  {
    message(Sys.time(), ": Cannot find a function named ", funcName)
    
    return(FALSE)
  }
  
  if (existsSavedNlStat(nlStatSig = nlStatSig, nlStatHash = nlStatHash))
  {
    message(Sys.time(), ": ", nlStatSig, " already saved")
    
    return(TRUE)
  }
  
  
  nlStatEntry <-
    stats::setNames(list(
      list(
        "nlStatBody" = nlStatBody,
        "nlStatArgs" = nlStatArgs,
        "nlStatHash" = nlStatHash
      )
    ),
    nlStatSig)
  
  #add fun to the package env
  .RnightlightsEnv$savedNlStats <-
    append(x = .RnightlightsEnv$savedNlStats, values = nlStatEntry)
  
  #and save to rda
  save("savedNlStats", envir = .RnightlightsEnv, file = getSavedNlStatFnamePath())
  
  return(TRUE)
}

######################## getSavedNlStat ###################################

#' Retrieve a saved nlStat function
#'
#' Retrieve a saved nlStat function
#'
#' @param nlStatSignature character The signature of the nlStat to retrieve which
#'     is used as the key of the saved nlStats
#'
#' @return list A list representing the saved nlStat
#'
#' @examples
#'
#' Rnightlights:::getSavedNlStat("sum()")
#'
getSavedNlStat <- function(nlStatSignature)
{
  .RnightlightsEnv$savedNlStats[nlStatSignature]
}

######################## listSavedNlStats ###################################

#' List saved nlStats
#'
#' List saved nlStats
#'
#' @param nlStatNames character The signatures of the nlStats to retrieve
#'
#' @param detail logical Whether to print out the whole saved nlStat including
#'     the signature, body, arguments and hash
#'
#' @return list A list of lists representing the saved nlStats
#'
#' @examples
#' listSavedNlStats("sum()")
#'
#' #returns
#' #"sum()"
#'
#'
#' listSavedNlStats("sum()", detail = TRUE)
#'
#' #returns
#' #$`sum()`
#' #$`sum()`$nlStatBody
#' #function (..., na.rm = FALSE)  .Primitive("sum")
#' #
#' #$`sum()`$nlStatArgs
#' #[1] ""
#' #
#' #$`sum()`$nlStatHash
#' #[1] "f0fbe35d81578311ba8f362137832e779b7b4f39"
#'
#' @export
listSavedNlStats <- function(nlStatNames = NULL,
                             detail = FALSE)
{
  if (is.null(.RnightlightsEnv$savedNlStats))
    readSavedNlStats()
  
  savedNlStats <- names(.RnightlightsEnv$savedNlStats)
  
  if (!is.null(nlStatNames))
    savedNlStats <- nlStatNames[nlStatNames %in% savedNlStats]
  
  if (length(savedNlStats) > 0 && detail)
    savedNlStats <-
    sapply(savedNlStats, function(x)
      getSavedNlStat(x), USE.NAMES = F)
  
  savedNlStats
}

######################## savedNlStatsIsLoaded ###################################

#' Check if saved nlStats have been loaded into memory from disk
#'
#' Check if saved nlStats have been loaded into memory from disk
#'
#' @return logical If the saved nlStats have been loaded
#'
#' @examples
#'
#' Rnightlights:::savedNlStatsIsLoaded()
#'
#' #returns TRUE/FALSE
#'
savedNlStatsIsLoaded <- function()
{
  exists(x = "savedNlStats", envir = .RnightlightsEnv)
}

######################## readSavedNlStats ###################################

#' Read saved nlStats from disk to memory
#'
#' Read saved nlStats from disk to memory and save them into the Rnightlights
#'     environment
#'
#' @return none
#'
#' @examples
#' \dontrun{
#'   readSavedNlStats()
#' }
#'
readSavedNlStats <- function()
{
  nlStatPath <- getSavedNlStatFnamePath()
  
  savedStatList <- NULL
  
  #if the file is found
  if (file.exists(nlStatPath))
  {
    #read in all saved fns into the package env
    savedStatList <-
      load(file = nlStatPath, envir = .RnightlightsEnv)
  }
  
  return(TRUE)
}

######################## nlSignatureStatName ###################################

#' get the name of the nlStat from the signature
#'
#' get the name of the nlStat from the signature i.e. the part before the
#'     first bracket
#'
#' @param statSig character The nlStat signature
#'
#' @return character the name of the nlStat
#'
#' @examples
#' nlSignatureStatName("sum()")
#'
#' #returns "sum"
#'
#' @export
nlSignatureStatName <- function(statSig)
{
  #get the name of the function i.e. part before the first bracket
  gsub("(^.*)\\(.*", "\\1", statSig)
}

######################## loadSavedNlStat ###################################

#' Load the saved nlStat into an accessible environment
#'
#' Load the saved nlStat into an accessible environment e.g. the global
#'     environment. This is especially required if the function is saved
#'     but not present in the Global environment or an environment where
#'     the package functions can access it
#'
#' @param statSig character The nlStat signature
#'
#' @param newStatName character The name to give the nlStat in the environment
#'     in case one does not want to use the name it is saved with
#'
#' @param envir character The environment in which to append the nlStat
#'
#' @param overwrite logical Whether to overwrite a function with the same name
#'     if it is present
#'
#' @return logical Whether the process was successful
#'
#' @examples
#' \dontrun{
#'   loadSavedNlStat(statSig = "sum()", newStatName = "mySum", overwrite = TRUE)
#' }
#'
#' #returns "sum"
#'
#' @export
loadSavedNlStat <-
  function(statSig,
           newStatName = NULL,
           envir = .GlobalEnv,
           overwrite = FALSE)
  {
    if (savedNlStatsIsLoaded() &&
        is.null(.RnightlightsEnv$savedNlStats))
      return(FALSE)
    
    if (is.null(newStatName))
      newStatName <- nlSignatureStatName(statSig)
    
    statSigs <- names(.RnightlightsEnv$savedNlStats)
    
    matchIdxs <- grep(statSig, statSigs, fixed = T)
    
    if (length(matchIdxs)  == 0)
    {
      message(Sys.time(), ": ", newStatName, " not found in saved stats")
      
      return(FALSE)
    }
    
    if (exists(newStatName, envir = envir) && !overwrite)
    {
      message(
        Sys.time(),
        ": ",
        newStatName,
        " exists in the global environment. Please set newStatName or set overwrite to TRUE"
      )
      
      return(FALSE)
    }
    
    savedStat <- getSavedNlStat(nlStatSignature = statSigs[matchIdxs])
    
    assign(x = newStatName,
           value = savedStat[[1]]$nlStatBody,
           envir = .GlobalEnv)
    
    return(TRUE)
  }

######################## searchSavedNlStatName ###################################

#' Check if an nlStat has been saved using the nlStatName
#'
#' Check if an nlStat has been saved using the nlStatName
#'
#' @param nlStatName character The name of the nlStat to search for
#'
#' @return character The signatures of nlStat that match or NULL if not found
#'
#' @examples
#' nlSignatureStatName("sum()")
#'
#' #returns "sum"
#'
#' @export
searchSavedNlStatName <- function(nlStatName)
{
  if (savedNlStatsIsLoaded() &&
      is.null(.RnightlightsEnv$savedNlStats))
    return(FALSE)
  
  statSigs <- names(.RnightlightsEnv$savedNlStats)
  
  statNames <- gsub("(^.*)\\(.*", "\\1", statSigs)
  
  matchIdxs <- grep(paste0("^", nlStatName, "$"), statNames)
  
  if (length(matchIdxs)  == 0)
    return(NULL)
  
  return(statSigs[matchIdxs])
}

######################## existsSavedNlStatSig ###################################

#' Check whether an nlStat exists in the saved nlStats
#'
#' Check whether an nlStat exists in the saved nlStats given the signature
#'
#' @param nlStatSig character The name of the nlStat to check
#'
#' @return logical Whether the nlStat was found in the saved nlStats
#'
#' @examples
#' \dontrun{
#'   existsSavedNlStatSig(nlStatSig = "sum()")
#'   #returns TRUE/FALSE
#' }
#'
existsSavedNlStatSig <- function(nlStatSig)
{
  if (savedNlStatsIsLoaded() &&
      is.null(.RnightlightsEnv$savedNlStats))
    return(FALSE)
  
  existsStatSig <-
    any(grepl(nlStatSig, names(.RnightlightsEnv$savedNlStats), fixed = T))
  
  return(existsStatSig)
}

######################## existsSavedNlStatHash ###################################

#' Check whether an nlStat exists in the saved nlStats given the nlStatHash
#'
#' Check whether an nlStat exists in the saved nlStats given the nlStatHash
#'
#' @param nlStatHash character The hash of the nlStat to check
#'
#' @return logical Whether the nlStatHash was found in the saved nlStats
#'
#' @examples
#' \dontrun{
#'   existsSavedNlStatHash(nlStatHash = "f0fbe35d81578311ba8f362137832e779b7b4f39")
#'   #returns TRUE/FALSE
#' }
#'
existsSavedNlStatHash <- function(nlStatHash)
{
  if (savedNlStatsIsLoaded() &&
      is.null(.RnightlightsEnv$savedNlStats))
    return(FALSE)
  
  existsStatHash <-
    any(sapply(.RnightlightsEnv$savedNlStats, function(x)
      x == nlStatHash))
  
  return(existsStatHash)
}

######################## existsSavedNlStat ###################################

#' Check whether an nlStat exists in the saved nlStats
#'
#' Check whether an nlStat exists in the saved nlStats given the nlStat
#'     signature and the nlStatHash to ensure a unique hit
#'
#' @param nlStatSig character The signature of the nlStat to check
#'
#' @param nlStatHash character The hash of the nlStat to check
#'
#' @return logical Whether the nlStat was found in the saved nlStats
#'
#' @examples
#' \dontrun{
#'   existsSavedNlStat(nlStatName = "sum()", nlStatHash = "f0fbe35d81578311ba8f362137832e779b7b4f39")
#'   #returns TRUE/FALSE
#' }
#'
#' @export
existsSavedNlStat <- function(nlStatSig, nlStatHash)
{
  if (savedNlStatsIsLoaded() &&
      is.null(.RnightlightsEnv$savedNlStats))
    return(FALSE)
  
  existsStatSig <- existsSavedNlStatSig(nlStatSig = nlStatSig)
  
  existsStatHash <- existsSavedNlStatHash(nlStatHash = nlStatHash)
  
  return(existsStatSig && existsStatHash)
}

######################## deleteSavedNlStat ###################################

#' Delete a previously saved nlStat
#'
#' Delete a previously saved nlStat
#'
#' @param nlStat The nlStat to delete
#'
#' @return an nlPeriod vector
#'
#' @examples
#' deleteSavedNlStat(nlStat)
#' #returns "201204"
#'
#' @export
deleteSavedNlStat <- function(nlStat)
{
  nlStatName <- nlSignatureStatName(nlStat)
  nlStatArgs <- nlStatArgs(nlStat)
  nlStatHash <- hashNlStat(nlStatName)
  nlStatSig <- nlStatSignature(nlStat = nlStat)
  
  
  if (!existsSavedNlStat(nlStatSig = nlStatSig, nlStatHash = hashNlStat(nlStatName)))
    return(FALSE)
  
  .RnightlightsEnv$savedNlStats[names(.RnightlightsEnv$savedNlStats) == nlStatSig] <-
    NULL
  
  #save the new state of savedNlStats
  save("savedNlStats", envir = .RnightlightsEnv, file = getSavedNlStatFnamePath())
  
  return(TRUE)
}

######################## hashNlStatBody ###################################

#' Return the hash of an nlStat function body
#'
#' Return the hash of an nlStat function body with whitespace removed. This
#'     is to help uniquely identify a function
#'
#' @param nlStatBody The function body of the nlStat
#'
#' @return a character vector
#'
#' @examples
#' Rnightlights:::hashNlStatBody(nlStatBody = 'function (..., na.rm = FALSE)  .Primitive("sum")')
#' #returns "f0fbe35d81578311ba8f362137832e779b7b4f39"
#'
#' @export
hashNlStatBody <- function(nlStatBody)
{
  #remove <bytecode: ...> which will change per session
  nlStatBody <- gsub("<bytecode: .*>", "", nlStatBody)
  
  #remove whitespace and hash
  digest::sha1(digest::sha1(gsub("\\s*", "", nlStatBody)))
}

######################## hashNlStat ###################################

#' Return the hash of an nlStat function
#'
#' Retrieve the body of an nlStat function and return the hash of an nlStat
#'     function.
#'
#' @param nlStatName The name of the nlStat function
#'
#' @return a character vector
#'
#' @examples
#' Rnightlights:::hashNlStat(nlStatName = "sum")
#' #returns "f0fbe35d81578311ba8f362137832e779b7b4f39"
#'
#' @export
hashNlStat <- function(nlStatName)
{
  nlStatBody <-
    utils::capture.output(eval(expr = parse(text = nlStatName)))
  
  nlStatBody <- paste(nlStatBody, collapse = "", sep = "")
  
  hashNlStatBody(nlStatBody = nlStatBody)
}

######################## equalNlStats ###################################

#' Check if two nlStats are equal
#'
#' Check if two nlStats are equal
#'
#' @param nlStatName1 The name of the first nlStat function to compare
#'
#' @param nlStatName2 The name of the second nlStat function to compare
#'
#' @return logical if the two nStats are equal
#'
#' @examples
#' fn1 <- function(x) sum(x)
#' fn2 <- function(x) sum(x, na.rm=T)
#' Rnightlights:::equalNlStats("fn1", "fn2")
#' #returns FALSE
#'
equalNlStats <- function(nlStatName1, nlStatName2)
{
  if (!allValid(list(nlStatName1, nlStatName2), validNlStats))
  {
    message(Sys.time(), ": ", nlStatName1, " not found")
    
    return(NA)
  }
  
  identical(x = hashNlStat(nlStatName = nlStatName1),
            y = hashNlStat(nlStatName = nlStatName2))
}

######################## myZonal ###################################

#' Calculate zonal statistics. Used internally
#'
#' Calculate zonal statistics. Used internally by zonalpipe. Modified from
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param rast the country raster
#'
#' @param nlType the nlType of interest
#'
#' @param configName character the config shortname of raster being processed
#' 
#' @param extension character the extension of raster being processed
#'
#' @param zone the zonal country polygon layer
#'
#' @param nlStats a character list of statistics to calculate
#'
#' @param digits round off to how many decimals
#'
#' @param retVal Whether to return the raster data as a vector, or
#'     data.frame with spatial context NULL returns a vector of all
#'     values, colrowval returns a data.frame with row, col and raster
#'     value while lonlatval returns a data.frame with lon,lat and val.
#'
#' @param na.rm how to handle NAs
#'
#' @param ... Other params to pass to the nlStats functions e.g. na.rm
#'
#' @return numeric value result of the given nlStat function
#'
#' @import data.table
#' 
#' @import ff
myZonal <-
  function (rast,
            nlType,
            configName,
            extension,
            zone,
            nlStats,
            digits = 0,
            retVal = NULL,
            na.rm = TRUE,
            ...)
  {
    options(fftempdir = getNlDir("dirNlTemp"), fffinalizer = "delete")
    
    options(stringsAsFactors = FALSE)
    
    #retVal <- "colrowval"
    
    vals <- NULL
    
    zones <- NULL
    
    #create the text for the functions
    fun <- sapply(nlStats,
                  function(nlStat)
                  {
                    #remove preceding = from "(=" or ",="
                    if (length(nlStat) > 1)
                      nlStatParams <-
                        gsub("(\\(\\s*)=|(,\\s*)=", "\\1\\2", paste0(", ", paste(
                          names(nlStat[-1]),
                          nlStat[-1],
                          sep = "=",
                          collapse = ","
                        )))
                    else
                      nlStatParams <- NULL
                    
                    nlStat <- nlStat[[1]]
                    
                    #paste0(nlStat,"=", nlStat, "(x, na.rm = TRUE)")
                    nlStatArgs <- formals(nlStat)
                    
                    retVal <-
                      if (all(sapply(c("col", "row"), "%in%", names(nlStatArgs))))
                        "colrowval"
                    else if (all(sapply(c("lon", "lat"), "%in%", names(nlStatArgs))))
                      "lonlatval"
                    else
                      NULL
                    
                    fnTxt <- if (is.null(retVal))
                      paste0(nlStat, "(vals", nlStatParams, ")")
                    else if (retVal == "colrowval")
                      paste0(nlStat, "(col=cols, row=rows, val=vals", nlStatParams, ")")
                    else if (retVal == "lonlatval")
                      paste0(nlStat, "(lon=lons, lat=lats, val=vals", nlStatParams, ")")
                  })
    
    funNames <- gsub("\\(.*\\)", "", fun)
    
    #create the aggregation function
    #funs <- paste0("dta[, as.list(unlist(lapply(.SD, function(dta) list(", paste(fun, collapse=","), ")))), by=zones]")
    funs <-
      paste0(
        "dta[, as.list(unlist(stats::setNames(lapply(fun, function(fn) eval(parse(text=fn))), funNames))), by=zones]"
      )
    
    retVal <- sapply(nlStats, function(nlStat) {
      nlStat <- nlStat[[1]]
      
      nlStatArgs <- formals(nlStat)
      
      retVal <-
        if (all(sapply(c("col", "row"), "%in%", names(nlStatArgs))))
          "colrowval"
      else if (all(sapply(c("lon", "lat"), "%in%", names(nlStatArgs))))
        "lonlatval"
      else
        NULL
    })
    
    message(Sys.time(), ": Reading in raster data")
    
    #the number of columns in the raster/zone file which are identical in size
    nc <- base::ncol(rast)
    
    #get the block size recommendation
    tr <- raster::blockSize(rast)
    
    #init the progress bar
    pb <- utils::txtProgressBar(min = 0,
                                max = tr$n,
                                style = 3)
    
    rowVals <- colVals <- lonVals <- latVals <- NULL
    
    #for each block
    for (i in 1:tr$n)
    {
      start <- ((tr$row[i] - 1) * nc) + 1
      
      end <- start + (tr$nrows[i] * nc) - 1
      
      rastVals <-
        raster::getValuesBlock(rast, row = tr$row[i], nrows = tr$nrows[i])
      zoneVals <-
        raster::getValuesBlock(zone, row = tr$row[i], nrows = tr$nrows[i])
      
      if (!is.null(retVal))
      {
        lonlatVals <-
          stats::setNames(as.data.frame(raster::xyFromCell(
            object = rast, cell = start:end
          )), c("lons", "lats"))
        
        colrowVals <-
          stats::setNames(as.data.frame(raster::rowColFromCell(
            object = rast, cell = start:end
          )),
          c("rows", "cols"))
        
        colrowVals <- colrowVals[, c(2, 1)]
      }
      
      #modifications e.g. NA removal must be done now
      #as we cannot modify
      if (grepl(x = nlType, pattern = "OLS"))
      {
        # source: https://ngdc.noaa.gov/eog/gcv4_readme.txt
        # Three image types are
        # available as geotiffs for download from the version 4 composites:
        #
        #
        #   F1?YYYY_v4b_cf_cvg.tif: Cloud-free coverages tally the total
        # number of observations that went into each 30 arc second grid cell. This
        # image can be used to identify areas with low numbers of observations
        # where the quality is reduced. In some years there are areas with zero
        # cloud- free observations in certain locations.
        #
        #
        # F1?YYYY_v4b_avg_vis.tif: Raw avg_vis contains the average of the
        # visible band digital number values with no further filtering. Data
        # values range from 0-63. Areas with zero cloud-free observations are
        # represented by the value 255.
        #
        #
        # F1?YYYY_v4b_stable_lights.avg_vis.tif: The cleaned up avg_vis
        # contains the lights from cities, towns, and other sites with persistent
        # lighting, including gas flares. Ephemeral events, such as fires have
        # been discarded. Then the background noise was identified and replaced
        # with values of zero. Data values range from 1-63. Areas with zero
        # cloud-free observations are represented by the value 255.
        
        if (grepl(x = nlType, pattern = "OLS.Y") && configName %in% c("raw", "avg_vis", "stable_lights"))
        {
          #not for cf_cvg
          #in DMSP-OLS 255 == NA
          rastVals[which(rastVals == 255)] <- NA
        }
        
        #negative values are errors replace with NA
        rastVals[rastVals < 0] <- NA
        
      } else if (grepl(x = nlType, pattern = "VIIRS"))
      {
        #convert negative values to NA
        rastVals[rastVals < 0] <- NA
      }
      
      #get all pixels in zone 0 which should be
      #the pixels outside the polygon
      idxZone0 <- which(zoneVals == 0)
      
      #remove zone 0 since in large rasters it can be pretty
      #large and may not fit in memory thus causing calculations to fail
      zoneVals <- zoneVals[-idxZone0]
      rastVals <- rastVals[-idxZone0]
      
      if (!is.null(retVal))
      {
        colrowVals <- colrowVals[-idxZone0, ]
        lonlatVals <- lonlatVals[-idxZone0, ]
      }
      
      #for first block init the ff to the given filename
      if (i == 1)
      {
        vals <-
          ff::ff(initdata = rastVals,
                 finalizer = "delete",
                 overwrite = T)
        zones <-
          ff::ff(initdata = zoneVals,
                 finalizer = "delete",
                 overwrite = T)
        
        if (!is.null(retVal))
        {
          cols <-
            ff::ff(
              initdata = colrowVals$cols,
              finalizer = "delete",
              overwrite = T
            )
          rows <-
            ff::ff(
              initdata = colrowVals$rows,
              finalizer = "delete",
              overwrite = T
            )
          lons <-
            ff::ff(
              initdata = lonlatVals$lons,
              finalizer = "delete",
              overwrite = T
            )
          lats <-
            ff::ff(
              initdata = lonlatVals$lats,
              finalizer = "delete",
              overwrite = T
            )
        }
      }
      else
      {
        #otherwise append
        vals <- ffbase::ffappend(vals, rastVals, adjustvmode = T)
        zones <- ffbase::ffappend(zones, zoneVals, adjustvmode = T)
        
        if (!is.null(retVal))
        {
          cols <- ffbase::ffappend(cols, colrowVals$cols, adjustvmode = T)
          rows <-
            ffbase::ffappend(rows, colrowVals$rows, adjustvmode = T)
          lons <-
            ffbase::ffappend(lons, lonlatVals$lons, adjustvmode = T)
          lats <-
            ffbase::ffappend(lats, lonlatVals$lats, adjustvmode = T)
        }
      }
      
      #upddate progress bar
      utils::setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    #merge the zone and raster ffvectors into an ffdf
    if (!is.null(retVal))
      rDT <- ff::ffdf(zones, cols, rows, lons, lats, vals)
    else
      rDT <- ff::ffdf(zones, vals)
    
    message(Sys.time(), ": Calculating nlStats ")
    
    #calculate the nlStats on the ffdf
    #hard coded the batchbytes which is the size of the
    #data to load into memory. Currently set at 1% of an 8GB memory
    #about 80MB. Has to be set for non-Windows systems. Need a better
    #way to figure this out
    result <- ffbase::ffdfdply(
      x = rDT,
      split = as.character(zones),
      trace = TRUE,
      BATCHBYTES = getBatchBytes(),
      FUN = function(dta) {
        ## This happens in RAM - containing **several** split
        #elements so here we can use data.table which works
        #fine for in RAM computing
        dta <- data.table::as.data.table(dta)
        
        #calc aggregations
        result <- eval(parse(text = funs))
        
        as.data.frame(result)
      }
    )
    
    result <- data.table::as.data.table(result)
    
    #count cols with nlStat in them
    nlStatColCounts <-
      sapply(funNames, function(funName)
        length(grep(
          paste0("^", funName, "\\.*"), names(result)
        )))
    
    #collapse multi-value cols into one
    for (funName in funNames)
    {
      if (nlStatColCounts[[funName]] > 1)
      {
        message(Sys.time(),
                ": ",
                funName,
                " => Multi-column result detected. Merging")
        #if more than one col detected, get their names
        nlStatCols <- grep(funName, names(result), value = T)
        
        #merge them row-wise into one character vector separated by comma
        result[[funName]] <-
          apply(result[, nlStatCols, with = F], 1, function(x)
            paste(x, collapse = ","))
        
        #remove the separate cols
        result[, c(nlStatCols) := NULL]
      } else
      {
        nlStatCols <- grep(funName, names(result), value = T)
        
        names(result)[which(names(result) == nlStatCols)] <- funName
      }
    }
    
    resultDF <- as.data.frame(result[, c("zones", funNames), with = F])
    
    ff::delete(rDT, result)
    rm(rDT, result)
    
    gc()
    
    return(resultDF)
  }

######################## ZonalPipe ###################################

#' Create a zonal file if it does not exist and calculate the zonal stats
#'
#' Create a zonal file if it does not exist and calculate the zonal stats by calling the
#'     myZonal function. Modified from
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param ctryCode the ctryCode of interest
#'
#' @param admLevel The country admin level of interest
#'
#' @param ctryPoly the SpatialPolygonsDataFrame country polygon to process
#'
#' @param nlType character The nlType of interest
#'
#' @param configName character the configName of raster being processed
#' 
#' @param extension character the extension of raster being processed
#'
#' @param path.in.shp The path to the country shapefile
#'
#' @param path.in.r The path to the raster tile
#'
#' @param path.out.r The path where to save the output zonal raster
#'
#' @param path.out.shp The path to save the output zonal shapefile (Ignored)
#'
#' @param zone.attribute The zonal attribute to calculate
#'
#' @param nlStats The stats to calculate
#'
#' @param gadmVersion The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return TRUE/FALSE
#'
ZonalPipe <- function (ctryCode,
                       admLevel,
                       ctryPoly,
                       nlType,
                       configName,
                       extension,
                       path.in.shp,
                       path.in.r,
                       path.out.r,
                       path.out.shp,
                       zone.attribute,
                       nlStats,
                       gadmVersion = pkgOptions("gadmVersion"),
                       gadmPolyType = pkgOptions("gadmPolyType"),
                       custPolyPath = NULL)
{
  #Source: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  #path.in.shp: Shapefile with zone (INPUT)
  #path.in.r: Raster from which the stats have to be computed (INPUT)
  #path.out.r: Path of path.in.shp converted in raster (intermediate OUTPUT)
  #path.out.shp: Path of path.in.shp with stat value (OUTPUT)
  #zone.attribute: Attribute name of path.in.shp corresponding to the zones (ID, Country...)
  #nlStat: function to summary path.in.r values ("mean", "sum"...)
  
  if (missing(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if (missing(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if (missing(path.in.shp))
    stop(Sys.time(), ": Missing required parameter path.in.shp")
  
  if (missing(path.in.r))
    stop(Sys.time(), ": Missing required parameter path.in.r")
  
  if (missing(path.out.r))
    stop(Sys.time(), ": Missing required parameter path.out.r")
  
  if (missing(zone.attribute))
    stop(Sys.time(), ": Missing required parameter zone.attribute")
  
  if (missing(nlStats))
    stop(Sys.time(), ": Missing required parameter nlStats")
  
  if (!validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)
  
  if (!allValid(nlStats, validNlStats))
    stop(Sys.time(), ": Invalid stat(s) detected")
  
  # 1/ Rasterize using GDAL
  
  # Find a better multi-platform way to check for gdal.
  # suppressWarnings(
  #   if(system("which gdal_rasterize", intern = T) != 0)
  #     stop(Sys.time(), ": gdal_rasterize not found. Please check that GDAL is installed")
  # )
  
  #Initiate parameter
  r <- raster::raster(path.in.r)
  
  if (!file.exists(path.out.r))
  {
    message(Sys.time(),
            ": Zonal file ",
            path.out.r,
            " doesn't exist. Creating ... ")
    
    #get the extent and change to minx, miny, maxx, maxy order for use
    #in gdal_rasterize. Explanation below
    ext <- raster::extent(r)
    ext <- paste(ext[1], ext[3], ext[2], ext[4])
    
    #get the resolution of the raster. will be used in gdal_rasterize
    #for target resolution which should be the same as the source resolution.
    #Specifying makes it run faster (?)
    res <- paste(raster::res(r)[1], raster::res(r)[2])
    
    lyrName <- admLevel #getCtryShpLowestLyrNames(ctryCode)
    
    tempRast <-
      file.path(getNlDir("dirNlTemp"), paste0(basename(tempfile()), ".tif"))
    
    #ctryPolyAdm0TmpDir <- tools::file_path_sans_ext(tempRast)
    
    #rgdal::writeOGR(obj = as(ctryPoly,"SpatialPolygonsDataFrame"), dsn = ctryPolyAdm0TmpDir, driver = "ESRI Shapefile", layer = lyrName)
    
    #Gdal_rasterize
    message(Sys.time(), ": Creating zonal raster")
    command <- 'gdal_rasterize'
    #Speed-up with more cache (avice: max 1/3 of your total RAM)
    command <-
      paste(command, paste0("--config GDAL_CACHEMAX ", pkgOptions("gdalCacheMax")))
    command <- paste(command, "-l", lyrName)
    #Identifies an attribute field on the features to be used for a burn
    #in value. The value will be burned into all output bands.
    command <- paste(command, "-a", zone.attribute)
    #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed
    #in georeferenced units. If not specified, the extent of the output file
    #will be the extent of the vector layers.
    command <- paste(command, "-te", as.character(ext))
    #(GDAL >= 1.8.0) set target resolution. The values must be expressed in
    #georeferenced units. Both must be positive values.
    command <- paste(command, "-tr", res)
    command <- paste(command, path.in.shp)
    command <- paste(command, tempRast)
    
    system(command)
    
    message(Sys.time(), ": Compressing zonal raster")
    gdalUtils::gdal_translate(co = "compress=LZW",
                              src_dataset = tempRast,
                              dst_dataset = path.out.r)
    
    file.remove(tempRast)
    
    #unlink(ctryPolyAdm0TmpDir, recursive = T, force = T)
  }
  
  if (file.exists(path.out.r))
    message(Sys.time(), ": Zonal file ", path.out.r, " found")
  else
    stop(path.out.r, " not found. Zonal creation failed.")
  
  # 2/ Zonal Stat using myZonal function
  zone <- raster::raster(path.out.r)
  
  message(Sys.time(), ": Calculating zonal stats ...")
  Zstat <-
    data.frame(
      myZonal(
        rast = r,
        nlType = nlType,
        configName = configName,
        extension = extension,
        zone = zone,
        nlStats = nlStats
      )
    )
  
  message(Sys.time(), ": Calculating zonal stats ... DONE")
  
  colnames(Zstat)[2:length(Zstat)] <-
    sapply(
      X = nlStats,
      FUN = function(x)
        x[[1]]
    )
  
  return(Zstat)
  
  # 3/ Merge data in the shapefile and write it #Not required at this point
  #shp<-rgdal::readOGR(path.in.shp, sub("^([^.]*).*", "\\1", basename(path.in.shp)))
  
  #shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])
  
  #rgdal::writeOGR(shp, path.out.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)), driver="ESRI Shapefile")
}

######################## fnAggRadGdal ###################################

#' Calculate zonal statistics using GDAL
#'
#' Calculate zonal statistics using GDAL. Alternative to fnAggRadRast and
#'     faster. Modified from
#'     \url{http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/}
#'
#' @param ctryCode character string the ISO3 country code to be processed
#'
#' @param admLevel character string The admin level to process. Should match
#'     the \code{ctryPoly} given but no checks are made currently.
#'
#' @param ctryPoly Polygon the loaded country polygon layer
#'
#' @param nlType the nlType of interest
#'
#' @param configName character the config short name of raster being processed
#' 
#' @param extension character the extension of raster being processed
#'
#' @param multiTileStrategy character How to handle multiple tiles per nlPeriod
#'
#' @param multiTileMergeFun character The function to use to merge tiles
#'
#' @param removeGasFlaresMethod logical Whether to perform gas flare removal pre-processing
#'
#' @param nlPeriod character string the nlPeriod to be processed
#'
#' @param nlStats character vector The stats to calculate
#'
#' @param gadmVersion The GADM version to use
#'
#' @param gadmPolyType The format of polygons to download from GADM
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return data.frame of polygon attributes and the calculated stats, one column per stat
#'
#' @examples
#' #read the Kenya polygon downloaded from GADM and load the lowest admin level (ward)
#' \dontrun{
#' ctryPoly <- readCtryPolyAdmLayer(ctryCode="KEN",
#'     Rnightlights:::getCtryShpLowestLyrNames(ctryCodes="KEN"))
#'
#' #calculate the sum of radiances for the wards in Kenya
#' sumAvgRadRast <- Rnightlights:::fnAggRadGdal(ctryCode="KEN", ctryPoly=ctryPoly,
#'     nlType="VIIRS.M", nlPeriod="201401", nlStats=c("sum","mean"))
#' }
#'
fnAggRadGdal <- function(ctryCode,
                         admLevel,
                         ctryPoly,
                         nlType,
                         configName = pkgOptions(paste0("configName_", nlType)),
                         extension,
                         multiTileStrategy = pkgOptions("multiTileStrategy"),
                         multiTileMergeFun = pkgOptions("multiTileMergeFun"),
                         removeGasFlaresMethod = pkgOptions("removeGasFlaresMethod"),
                         nlPeriod,
                         nlStats = pkgOptions("nlStats"),
                         gadmVersion = pkgOptions("gadmVersion"),
                         gadmPolyType = pkgOptions("gadmPolyType"),
                         custPolyPath = NULL)
{
  if (missing(ctryCode))
    stop(Sys.time(), ": Missing required parameter ctryCode")
  
  if (missing(nlPeriod))
    stop(Sys.time(), ": Missing required parameter nlPeriod")
  
  if (!validCtryCodes(ctryCode))
    stop(Sys.time(), ": Invalid ctryCode: ", ctryCode)
  
  if (!allValidNlPeriods(nlPeriods = nlPeriod, nlTypes = nlType))
    stop(Sys.time(),
         ": Invalid nlPeriod: ",
         nlPeriod,
         " for nlType: ",
         nlType)
  
  if (!allValid(nlStats, validNlStats))
    stop(Sys.time(), ": Invalid stat(s) detected")
  
  #source: http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  
  path.in.shp <- getPolyFnamePath(
    ctryCode = ctryCode,
    gadmVersion = gadmVersion,
    gadmPolyType = gadmPolyType,
    custPolyPath = custPolyPath
  )
  
  path.in.r <- getCtryRasterOutputFnamePath(
    ctryCode = ctryCode,
    nlType = nlType,
    configName = configName,
    extension = extension,
    multiTileStrategy = multiTileStrategy,
    multiTileMergeFun = multiTileMergeFun,
    removeGasFlaresMethod = removeGasFlaresMethod,
    nlPeriod = nlPeriod,
    gadmVersion = gadmVersion,
    gadmPolyType = gadmPolyType,
    custPolyPath = custPolyPath
  )
  
  #cloud coverage raster
  
  #only one zonal raster per country required across all time
  if (is.null(custPolyPath))
    path.out.r <-
    file.path(
      getNlDir("dirZonals"),
      paste0(
        "NL_ZONAL_",
        ctryCode,
        "_",
        "ADM",
        substr(
          x = admLevel,
          start = nchar(admLevel),
          stop = nchar(admLevel)
        ),
        "_",
        nlType,
        "_",
        # nlPeriod, "_",
        "GF",
        toupper(removeGasFlaresMethod),
        "_",
        "GADM-",
        gadmVersion,
        "-",
        toupper(gadmPolyType),
        ".tif"
      )
    )
  else
    path.out.r <-
    file.path(
      getNlDir("dirZonals"),
      paste0(
        "NL_ZONAL_",
        ctryCode,
        "_",
        #cust layers are prefixed with layer number
        "ADM",
        substr(x = admLevel, start = 1, stop = 1),
        "_",
        nlType,
        "_",
        # nlPeriod, "_",
        "GF",
        toupper(removeGasFlaresMethod),
        "_",
        basename(custPolyPath),
        "-SHPZIP.tif"
      )
    )
  
  #path.out.shp not in use at the moment
  if (is.null(custPolyPath))
    path.out.shp <-
    file.path(
      getNlDir("dirZonals"),
      paste0(admLevel, "_zone_",
             nlType, "_",
             "GADM-", gadmVersion,
             ".shp")
    )
  else
    path.out.shp <-
    file.path(getNlDir("dirZonals"),
              paste0(admLevel, "_zone_", nlType, "SHPZIP.shp"))
  
  zone.attribute <- if (is.null(custPolyPath))
  {
    if (gadmVersion == "2.8")
      paste0("ID_", stringr::str_extract(admLevel, "\\d+$"))
    else if (gadmVersion == "3.6")
      paste0("GID_", stringr::str_extract(admLevel, "\\d+$"), "_IDX")
  } else
  {
    paste0("GID_IDX")
  }
  
  lyrName <- admLevel
  
  #which col to use as the unique id. For GADM 3.6
  #use the col we generated at download
  lyrIDCol <- if (is.null(custPolyPath))
  {
    if (gadmVersion == "2.8")
      paste0("ID_", stringr::str_extract(lyrName, "\\d+$"))
    else if (gadmVersion == "3.6")
      paste0("GID_", stringr::str_extract(lyrName, "\\d+$"), "_IDX")
  } else
  {
    paste0("GID_IDX")
  }
  
  sumAvgRad <- ZonalPipe(
    ctryCode = ctryCode,
    admLevel = admLevel,
    ctryPoly = ctryPoly,
    nlType = nlType,
    configName = configName,
    path.in.shp = path.in.shp,
    path.in.r = path.in.r,
    path.out.r = path.out.r,
    path.out.shp = path.out.shp,
    zone.attribute = zone.attribute,
    nlStats = nlStats,
    gadmVersion = gadmVersion,
    gadmPolyType = gadmPolyType,
    custPolyPath = custPolyPath
  )
  
  ctryPolyData <- ctryPoly@data
  
  ctryPolyData[, lyrIDCol] <- as.integer(ctryPolyData[, lyrIDCol])
  
  ctryPolyData <- ctryPolyData[order(ctryPolyData[, lyrIDCol]), ]
  
  #if there is only the country adm level i.e. no lower adm levels than
  #    the country adm level then we only have 1 row each but IDs may not
  #    match as seen with ATA. treat differently
  #    since we do not have IDs to merge by, we simply cbind the columns
  #    and return column 2
  
  if (grepl("^ID_0$", lyrIDCol))
  {
    sumAvgRad <- cbind(ctryPolyData$ID_0, sumAvgRad[sumAvgRad$z != 0,])
  }
  else
  {
    sumAvgRad <-
      merge(
        ctryPolyData,
        sumAvgRad,
        by.x = lyrIDCol,
        by.y = "zones",
        all.x = T,
        sort = T
      )
  }
  
  return(sumAvgRad)
}

######################## fnAggRadRast ###################################

#' Calculate statistics on a nightlight raster that fall within a polygon
#'
#' Calculate stats on the radiance of the pixels in a nightlight raster
#'     that fall within a polygon and its subpolygons using the \code{raster}
#'     package. Given a country polygon with subpolygons representing lower
#'     admin levels, it will crop and mask the raster to each subpolygon and
#'     calculate the total radiance for the polygon and return a vector of total
#'     radiances that matches the subpolygons
#'
#' @param ctryPoly The polygon of the admin level/region of interest. In general is a country polygon
#'     with sub-regions usually the lowest known admin level as given by the GADM polygons.
#'
#' @param ctryRastCropped The raster containing nightlight radiances to sum. Usually will have already be
#'     cropped to the country outline
#'
#' @param nlType Character vector The nlType to process
#'
#' @param configName character the config short name of raster being processed
#' 
#' @param extension character the extension of raster being processed
#'
#' @param nlStats The statistics to calculate
#'
#' @param custPolyPath Alternative to GADM. A path to a custom shapefile zip
#'
#' @return data.frame of polygon attributes and the calculated stats, one column per nlStat
#'
#' @examples
#' #read the Kenya polygon downloaded from GADM and load the lowest admin level (ward)
#' \dontrun{
#' ctryPoly <- readCtryPolyAdmLayer(ctryCode="KEN",
#'     Rnightlights:::getCtryShpLowestLyrNames(ctryCodes="KEN"))
#'
#' # the VIIRS nightlight raster cropped earlier to the country outline
#' ctryRastCropped <- raster::raster(Rnightlights:::getCtryRasterOutputFnamePath(ctryCode="KEN",
#'     nlType="VIIRS.M", nlPeriod="201401"))
#'
#' #calculate the sum of radiances for the wards in Kenya
#' sumAvgRadRast <- Rnightlights:::fnAggRadRast(ctryPoly=ctryPoly,
#'     ctryRastCropped=ctryRastCropped, nlType="VIIRS.M", nlStats=c("sum","mean"))
#' }
#' @importFrom foreach %dopar%
fnAggRadRast <-
  function(ctryPoly,
           ctryRastCropped,
           nlType,
           configName,
           extension,
           nlStats,
           custPolyPath = NULL)
  {
    if (missing(ctryPoly))
      stop(Sys.time(), ": Missing required parameter ctryPoly")
    
    if (missing(ctryRastCropped))
      stop(Sys.time(), ": Missing required parameter ctryRastCropped")
    
    if ((
      class(ctryPoly) != "SpatialPolygons" &&
      class(ctryPoly) != "SpatialPolygonsDataFrame"
    ) || is.null(ctryPoly))
      stop(Sys.time(), ": Invalid ctryPoly type: ", class(ctryPoly))
    
    if (class(ctryRastCropped) != "RasterLayer" ||
        is.null(ctryRastCropped))
      stop(Sys.time(),
           ": Invalid ctryRastCropped type: ",
           class(ctryRastCropped))
    
    if (missing(nlType))
      stop(Sys.time(), ": Missing required parameter nlType")
    
    if (!allValid(nlStats, validNlStats))
      stop(Sys.time(), ": Invalid stat(s) detected")
    
    options(stringsAsFactors = FALSE)
    
    on.exit({
      
    })
    
    cl <- snow::makeCluster(pkgOptions("numThreads"))
    
    doSNOW::registerDoSNOW(cl = cl)
    
    #max=nrow+1 to handle single row cases since must max > min
    pb <-
      utils::txtProgressBar(min = 0,
                            max = nrow(ctryPoly@data),
                            style = 3)
    progress <- function(n)
      utils::setTxtProgressBar(pb, n)
    
    #to avoid RCheck notes
    i <- NULL
    
    nlStatNames <- sapply(nlStats, function(x)
      x[[1]])
    
    result <- foreach::foreach(
      i = seq_len(nrow(ctryPoly@data)),
      .combine = rbind,
      .export = c("masqOLS", "masqVIIRS", nlStatNames),
      .packages = c("raster"),
      .options.snow = list(progress = progress)
    ) %dopar% {
      # for(i in 1:nrow(ctryPoly@data)){
      
      options(stringsAsFactors = FALSE)
      
      pid <- Sys.getpid()
      
      message(Sys.time(), ": PID:", pid, " Extracting data from polygon " , i)
      
      retVal <-
        sapply(nlStats, function(nlStat) {
          nlStat <- nlStat[[1]]
          
          nlStatArgs <- formals(nlStat)
          
          retVal <-
            if (all(sapply(c("col", "row"), "%in%", names(nlStatArgs))))
              "colrowval"
          else if (all(sapply(c("lon", "lat"), "%in%", names(nlStatArgs))))
            "lonlatval"
          else
            NULL
        })
      
      dta <-
        if (stringr::str_detect(nlType, "OLS"))
          masqOLS(
            ctryPoly = ctryPoly,
            ctryRast = ctryRastCropped,
            idx = i,
            retVal = is.null(retVal),
            configName = configName,
            extension = extension
          )
      else if (stringr::str_detect(nlType, "VIIRS"))
        masqVIIRS(
          ctryPoly = ctryPoly,
          ctryRast = ctryRastCropped,
          idx = i,
          retVal = is.null(retVal),
          configName = configName,
          extension = extension
        )
      
      message(Sys.time(),
              ": PID:",
              pid,
              " Calculating the NL stats of polygon ",
              i)
      
      result <- data.frame(lapply(
        nlStats,
        FUN = function(nlStat)
        {
          if (length(nlStat) > 1)
            nlStatParams <-
              gsub("(\\(\\s*)=|(,\\s*)=", "\\1\\2", paste0(", ", paste(
                names(nlStat[-1]),
                nlStat[-1],
                sep = "=",
                collapse = ","
              )))
          else
            nlStatParams <-
              NULL
          
          nlStat <-
            nlStat[[1]]
          
          nlStatArgs <-
            formals(nlStat)
          
          retVal <-
            if (all(sapply(c("col", "row"), "%in%", names(nlStatArgs))))
              "colrowval"
          else if (all(sapply(c("lon", "lat"), "%in%", names(nlStatArgs))))
            "lonlatval"
          else
            NULL
          
          fnTxt <-
            if (is.null(retVal))
              paste0(nlStat, "(dta$vals", nlStatParams, ")")
          else if (retVal == "colrowval")
            paste0(nlStat,
                   "(col=dta$cols, row=dta$rows, val=dta$vals",
                   nlStatParams,
                   ")")
          else if (retVal == "lonlatval")
            paste0(nlStat,
                   "(lon=dta$lons, lat=dta$lats, val=dta$vals",
                   nlStatParams,
                   ")")
          #browser()
          fnTxt <-
            paste0("data.frame('", nlStat, "' = matrix(", fnTxt, ", nrow=1))")
          eval(parse(text = fnTxt))
        }
      ))
    }
    
    #count cols with nlStat in them
    nlStatColCounts <-
      sapply(nlStatNames, function(nlStat)
        length(grep(
          paste0("^", nlStat, "\\.*"), names(result)
        )))
    
    #collapse multi-value cols into one
    for (nlStatName in nlStatNames)
    {
      if (nlStatColCounts[[nlStatName]] > 1)
      {
        message(Sys.time(),
                ": ",
                nlStatName,
                " => Multi-column result detected. Merging")
        
        #if more than one col detected, get their names
        nlStatCols <- grep(nlStatName, names(result), value = T)
        
        #merge them row-wise into one character vector separated by comma
        result[[nlStatName]] <-
          apply(result[, nlStatCols], 1, function(x)
            paste(x, collapse = ","))
        
        #remove the separate cols
        result[, nlStatCols] <- NULL
      } else
      {
        nlStatCols <- grep(nlStatName, names(result), value = T)
        
        names(result)[which(names(result) == nlStatCols)] <-
          nlStatName
      }
    }
    
    #change the col order to the order of nlStats
    result <- result[, nlStatNames]
    
    close(pb)
    
    on.exit({
      snow::stopCluster(cl)
      
      raster::removeTmpFiles(h = 0)
      
      gc()
    })
    
    return(stats::setNames(data.frame(result), nlStatNames))
  }
