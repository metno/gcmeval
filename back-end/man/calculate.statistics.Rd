\name{calculate.statistics}
\alias{calculate.statistics.cmip}
\alias{calculate.rmse.cmip}
\alias{calculate.mon.weights}
\title{Calculate statistics}
\description{
\code{calculate.statistics.cmip} calculates the annual and monthly standard deviation and mean values of GCM output (CMIP5 data). If a reference data set is specified and the selected period is in the past, the spatial correlation between the reference data and the GCM output is also calculated. The calculations are done both for the whole globe and for the 26 SREX regions. 
\code{calculate.rmse.cmip} calculates the root mean square error (RMSE) of the seasonal cycle compared to the reference data.
\code{calculate.mon.weights} calculates weights based on the latitude and number of days per month that are used in \code{calculate.rmse.cmip}.
}
\usage{
calculate.statistics.cmip(reference="eraint", period=c(1981,2010),
                          variable="tas", experiment="rcp45", nfiles=5,
                          path.gcm=NULL, continue=TRUE, 
                          path=NULL, mask="coords.txt", 
                          store.file="statistics.rda",
                          stats=c("mean","spatial.sd","corr","rmse"),
                          verbose=FALSE)
calculate.mon.weights(lon,lat)
}
\arguments{
  \item{reference}{reference data set: "eraint", "era5","eobs", or "cfsr")}
  \item{period}{a subset of years to include in calculations}
  \item{variable}{variable id: 'tas' = temperature, 'pr' = precipitation}
  \item{experiment}{parent experiment of GCM data, e.g Representative Concentration Pathway RCP4.5 ('rcp45') or RCP8.5 ('rcp85')}
  \item{nfiles}{number of files to analyse, can be a number or "all"}
  \item{path.gcm}{path to GCM data}
  \item{path}{path to output files}
  \item{store.file}{filename of output file}
  \item{stats}{statistics to calculate: 
  'mean' = annual and monthly mean of the spatial average, 
  'spatial.sd' = annual and monthly mean of the spatial standard deviation,
  'corr' = annual and monthly mean spatial correlation of the GCM and a reference data set,
  'rmse' = root mean square error of the mean seasonal cycle of a GCM compared to a reference data set,
  'cmpi' = climate model performance index, i.e., the normalised rmse.}
  \item{continue}{if TRUE add statistics to existing file, if FALSE create new file}
  \item{mask}{a spatial mask}
  \item{lon}{a vector containing longitudes}
  \item{lat}{a vector containing latitudes}
  \item{verbose}{if TRUE display information on progress, if FALSE run in silent mode}
}
\examples{
\dontrun{
calculate.statistics.cmip(reference=NULL, period=c(2071,2100),
                          variable="tas", nfiles=5, continue=TRUE,
          			          experiment="rcp45", verbose=TRUE)
}
}
\keyword{statistics}
\keyword{rmse}