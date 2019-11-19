\name{retrieve}
\alias{retrieve}
\alias{retrieve.ncdf4}
\alias{retrieve.rcm}
\title{Retrieve data from a netcdf file.}
\usage{
retrieve(ncfile, param="auto", path=NULL, verbose=FALSE, ...)
retrieve.ncdf4(ncfile=ncfile, path=NULL , param="auto",
               lon=NULL, lat=NULL, lev=NULL, it=NULL,
               miss2na=TRUE, greenwich=FALSE,
               verbose=FALSE)
retrieve.rcm(ncfile, path=NULL, param=NULL, is=NULL, it=NULL,
             verbose=FALSE)	       
}
\arguments{
\item{ncfile}{A character string of full path netcdf file name (include the
path if necessary) or any object of class 'ncdf4'.}
\item{path}{path to file 'ncfile'}
\item{lon}{Numeric value of longitude for the reference point (in decimal
degrees East) or a vector containing the range of longitude values in the
form of c(lon.min,lon.max)}
\item{lat}{Numeric value of latitude for the reference point (in decimal
degrees North) or a vector containing the range of latitude values in the
form of c(lat.min,lat.max)}
\item{lev}{Numeric value of pressure levels or a vector containing the range
of pressure level values in the form of c(lev.min,lev.max)}
\item{it}{Numerical or date values of time or a vector containing the range
of values in the form of c(start,end). Date format should be in the form of
"YYYY-MM-DD".}
\item{is}{Numerical or logical values of spatial indexing for reading
station data (retrieve.station).}
\item{param}{Parameter or element type. There are several core parameters or
elements as well as a number of additional parameters. The parameters or
elements are: auto = automatic selection.  precip, prcp, pr = Precipitation
(mm) tas, tavg = 2m-surface temperature (in degrees Celcius) tmax, tasmax =
Maximum temperature (in degrees Celcius) tmin, tasmin = Minimum temperature
(in degrees Celcius)}
\item{miss2na}{Logical value. If TRUE replace missing values with NA.}
\item{greenwich}{Logical value. If FALSE, convert longitudes to -180E/180E
or centre maps on Greenwich meridian (0 deg E). In other words, when
Greenwich == TRUE, the left boundary of a global field is set to Greenwich
and not the dateline.}
\item{verbose}{Logical value defaulting to FALSE. If FALSE, do not display
comments (silent mode). If TRUE, displays extra information on progress.}
\item{\dots}{Additional input.}
}
\value{A "zoo" "field" object with additional attributes.}
\description{
Retrieve data from a netcdf file and return a "zoo" "field" object with
attributes.  \code{retrieve} assumes data on a regular lon-lat grid and
\code{retrieve.rcm} reads data on irregular (rotated) grid, typically output
from RCMs.
}