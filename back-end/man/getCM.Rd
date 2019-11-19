\name{getCM}
\alias{getCM}
\alias{getCMIP5}
\alias{getGCMs}
\alias{cmip5.urls}
\alias{getatt}
\alias{getncid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download GCM and RCM data
}
\description{\code{getCM} downloads GCM or RCM data from given a URL. \code{getGCMs} and \code{getCMIP5} are identical and both retrieve CMIP5 data from the KNMI climate data explorer, generating the URLs with \code{cmip5.urls} and \code{cordex.urls}. \code{getatt} reads and extracts the attribute information in a netCDF files and stores this in a list object. \code{getncid} extracts additional information from a netCDF file.
}
\usage{
getCM(url=NULL,destfile='CM.nc',path=NULL,
      lon=NULL,lat=NULL,force=FALSE,verbose=FALSE)
getncid(filename, path=NULL, verbose=FALSE)
getatt(filename)
cmip5.urls(experiment='rcp45',varid='tas',
           url="http://climexp.knmi.nl/CMIP5/monthly/", 
           force=FALSE,verbose=FALSE)
getGCMs(select=1:9,varid='tas',experiment='rcp45',
        destfile=NULL,path=NULL,force=FALSE,verbose=FALSE)
getCMIP5(select=1:9,varid='tas',experiment='rcp45',
         destfile=NULL,path=NULL,force=FALSE,verbose=FALSE)	
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{The URL locating the data files over Internet}
  \item{destfile}{The name of the local destination file when downloading}
  \item{filename}{The filename of a local netCDF file}
  \item{lon}{longitude range to retrieve}
  \item{lat}{latitude range to retrieve}
  \item{select}{index for selecting a subset of files/models to download}
  \item{varid}{variable name - see \code{link{retrieve}}}
  \item{path}{local file path to the destination file}
  \item{experiment}{Parent experiment of the model data, either representative concentration pathway RCP4.5 ("rcp45"), RCP2.6 ("rcp26"), or RCP8.5 ("rcp85").}
  \item{force}{Logical value defaulting to FALSE. If FALSE, do not
    download data from source if a local file \code{path/destfile} is available. If TRUE, download data and overwrite local file.}
  \item{verbose}{Logical value defaulting to FALSE. If FALSE, do not
    display comments (silent mode). If TRUE, displays extra information on
    progress.}
}
\value{
\code{getCM} downloads the data and returns a "ncdf4" object containing information about the file (e.g. "filename", "url", "model"). 
\code{getGCMs} returns a list of "ncdf4" objects corresponding to different models.
}
\author{
Kajsa Parding <kajsamp@met.no>
}
\examples{
\dontrun{
x <- getCM(url='http://climexp.knmi.nl/CMIP5/monthly/tas/tas_Amon_ACCESS1-0_historical_000.nc',
      destfile='GCM1.nc',lon=c(-60,60),lat=c(45,80),
      force=TRUE,verbose=TRUE)
    
filename <- x$filename
x.info <- getatt(filename)

y <- getGCMs(select=1:3)
}
}

\keyword{ cds }
\keyword{ utilities }