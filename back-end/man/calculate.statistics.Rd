\name{calculate.statistics}
\alias{calculate.statistics}
\alias{calculate.statistics.cmip}
\alias{calculate.mon.weights}
\title{Calculate statistics}
\description{
\code{calculate.statistics} and \code{calculate.statistics.cmip} calculate the annual and monthly standard deviation and mean values of GCM output (CMIP5 and CMIP6 data). If a reference data set is specified and the selected period is in the past, the spatial correlation between the reference data and the GCM output is also calculated, as well as the root mean square error (RMSE) between the GCM and reference data. The calculations are done both for the whole globe and for the 26 SREX regions.
\code{calculate.statistics} can be applied to several GCM files and for each file calls the function \code{calculate.statistics.cmip}.  
\code{calculate.mon.weights} calculates weights based on the latitude and number of days per month that are used to calculate the RMSE.
}
\usage{
calculate.statistics(files.in, meta=NULL, file.out="statistics.rda", ref=NULL,
                     path.in=NULL, path.out=NULL, path.ref=NULL,
                     stats=c("mean.gcm","spatial.sd.gcm","mean.ref","spatial.sd.ref","corr","rmse"),
                     period=c(1981,2010), add=TRUE, force=FALSE, verbose=FALSE)
calculate.statistics.cmip(file.gcm, file.ref=NULL, period=c(1981,2010), 
                          mask="coords.txt", stats=c("mean","spatial.sd","corr","rmse"),
                          verbose=FALSE)
calculate.mon.weights(lon,lat)
}
\arguments{
  \item{files.in}{a list of netCDF files containing GCM data}
  \item{file.in}{a netCDF file containing GCM data}
  \item{meta}{metadata for \code{files.in}}
  \item{ref}{reference data set: "eraint", "era5","gpcp", or "eobs")}
  \item{file.ref}{a netCDF file containing reference data (reanalysis or gridded observations)}
  \item{period}{a subset of years to include in calculations}
  \item{path.in}{path to GCM data}
  \item{path.out}{path to output files}
  \item{path.ref}{path to reference data}
  \item{file.out}{filename of output file}
  \item{stats}{statistics to calculate: 
  'mean' = annual and monthly mean of the spatial average (use 'mean.gcm' or 'mean.ref' to apply only to GCM or reference), 
  'spatial.sd' = annual and monthly mean of the spatial standard deviation (use 'spatial.sd.gcm' or 'spatial.sd.ref' to apply only to GCM or reference),
  'corr' = annual and monthly mean spatial correlation of the GCM and a reference data set,
  'rmse' = root mean square error of the mean seasonal cycle of a GCM compared to a reference data set,
  'cmpi' = climate model performance index, i.e., the normalised rmse.}
  \item{add}{if TRUE add statistics to existing file, if FALSE create new file}
  \item{force}{if TRUE calculate new statistics to replace existing values}
  \item{mask}{a spatial mask}
  \item{lon}{a vector containing longitudes}
  \item{lat}{a vector containing latitudes}
  \item{verbose}{if TRUE display information on progress, if FALSE run in silent mode}
}
\examples{
\dontrun{
x <- calculate.statistics(files.gcm, meta=meta, file.out="statistics.rda", path.out=path.out,
                            ref=NULL, period=it, stats=stats, add=add, force=force, verbose=verbose)
}
}
\keyword{statistics}
\keyword{rmse}