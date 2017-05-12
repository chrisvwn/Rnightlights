.onLoad <- function(libname, pkgname)
{
  # ns <- getNamespace(pkgname)
  # pkg <- getPackageName(pkgname)
  # assign(pkgname, pkg, envir=ns)
  
  #data(dataPath)
  
  # Setup the data path, possibly by prompting the user.
  # setupDataPath()
}

.onAttach <- function(libname, pkgname)
{
  #pkg <- get(pkgname, envir=getNamespace(pkgname));
  #startupMessage(pkg);
  
  # Setup the data path, possibly by prompting the user. if not found
  if(is.null(getDataPath()))
    setupDataPath()
}

.onDetach <- function(libname, pkgname)
{
  #cleanup by removing any global vars created etc
  cleanupNtLts();
}

.onUnload <- function(libname, pkgname)
{
  # ns <- getNamespace(pkgname);
  # pkg <- Package(pkgname);
  # assign(pkgname, pkg, envir=ns);
  # removeDataPath()
}
