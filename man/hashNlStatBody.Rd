% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{hashNlStatBody}
\alias{hashNlStatBody}
\title{Return the hash of an nlStat function body}
\usage{
hashNlStatBody(nlStatBody)
}
\arguments{
\item{nlStatBody}{The function body of the nlStat}
}
\value{
a character vector
}
\description{
Return the hash of an nlStat function body with whitespace removed. This
    is to help uniquely identify a function
}
\examples{
Rnightlights:::hashNlStatBody(nlStatBody = 'function (..., na.rm = FALSE)  .Primitive("sum")')
#returns "f0fbe35d81578311ba8f362137832e779b7b4f39"

}
