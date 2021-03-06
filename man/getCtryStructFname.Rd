% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygons.R
\name{getCtryStructFname}
\alias{getCtryStructFname}
\title{Construct the name for the country struct file}
\usage{
getCtryStructFname(
  ctryCode = NULL,
  gadmVersion = pkgOptions("gadmVersion"),
  gadmPolyType = pkgOptions("gadmPolyType"),
  custPolyPath = NULL
)
}
\arguments{
\item{ctryCode}{The ISO3 ctryCode of the country}

\item{gadmVersion}{The GADM version to use}

\item{gadmPolyType}{The format of polygons to download from GADM}

\item{custPolyPath}{Alternative to GADM. A path to a custom shapefile zip}
}
\value{
character string The filename
}
\description{
Construct the name for the country struct file
}
\examples{
Rnightlights:::getCtryStructFname("KEN")

}
