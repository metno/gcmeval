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
\item{greenwich}{Logical value. If FALSE, convert longitudes to -180E/180E
or centre maps on Greenwich meridian (0 deg E). In other words, when
Greenwich == TRUE, the left boundary of a global field is set to Greenwich
and not the dateline.}
\item{miss2na}{Logical value. If TRUE missing values are converted to "NA"}
\item{verbose}{Logical value defaulting to FALSE. If FALSE, do not display
comments (silent mode). If TRUE, displays extra information on progress.}
\item{\dots}{Additional arguments}
}
\value{
A "zoo" "field" object with additional attributes used for further
processing.
}
\description{
Retrieve data from a netcdf file and return a zoo field object with
attributes.  \code{retrieve} assumes data on a regular lon-lat grid and
\code{retrieve.rcm} reads data on irregular (rotated) grid. typically output
from RCMs.
}
\examples{

\dontrun{
  # Download air surface temperature (tas) for the 'NorESM1-ME' model
  # output prepared for 'CMIP5 RCP4.5' and for run 'r1i1p1' from the climate
  # explorer web portal (http://climexp.knmi.nl) and store the file into the 
  # local machine, e.g. temporary folder '/tmp' (Size ~96Mb) using the following
  # command if needed. Otherwise, specify a netcdf file to retrieve data from. 
  url <- "http://climexp.knmi.nl/CMIP5/monthly/tas"
  noresm <- "tas_Amon_NorESM1-ME_rcp45_000.nc"
  download.file(url=file.path(url,noresm), destfile=noresm,
               method="auto", quiet=FALSE, mode="w",
               cacheOK = TRUE)
  # Retrieve the data into "gcm" object
  gcm <- retrieve(ncfile=file.path(~,noresm),param="tas",
                lon=c(-20,30),lat=c(40,90),plot=TRUE)
  # Download the air surface temperature (tas) for RCP 4.5 scenarios and
  # NorESM1-ME model from the climate explorer and store it in destfile. 
  # Compute the anomalies
  gcm.a <- as.anomaly(gcm,ref=c(1960:2001))
  map(gcm.a,projection="sphere")
}

}
\seealso{
summary.ncdf4 check.ncdf4 file.class
}
