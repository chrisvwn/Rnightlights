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
  if(is.null(getNlDataPath()))
    setupDataPath()
  
  #global constants
  map <- rworldmap::getMap()
  map <- cleangeo::clgeo_Clean(map)
  shpTopLyrName <- "adm0"
  #projection system to use
  #can we use only one or does it depend on the shapefile loaded?
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #ntLtsIndexUrlVIIRS = "https://www.ngdc.noaa.gov/eog/viirs/download_monthly.html"
  
  compiler::enableJIT(3)
}

.onDetach <- function(libname)
{
  #cleanup by removing any global vars created etc
  nlCleanup();
  compiler::enableJIT(0)
}

.onUnload <- function(libname, pkgname)
{
  # ns <- getNamespace(pkgname);
  # pkg <- Package(pkgname);
  # assign(pkgname, pkg, envir=ns);
  # removeDataPath()
}
