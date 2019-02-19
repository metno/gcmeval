\name{getReference}
\alias{getReference}
\alias{getCFSR}
\alias{getCRU}
\alias{getEOBS}
\alias{getERA}
\alias{python.getEra}
\title{Get reference data}
\description{
Functions for downloading reference data.
\code{getReference} generates the filename of a reference file but does not download the data.
\code{getCFSR} retrieves monthly Climate Forecast System Reanalysis (CFSR) data and interpolates it to a 2.5 degree grid.
\code{getCRU} retrieves CRU data.
\code{getEOBS} gets daily EOBS data and converts it to monthly averages.
\code{getERA} retrieves ERAinterim data from the ECMWF data server using the \code{python.getEra} script. To use these functions, you must register as an ECMWF data user and obtain an API key.   
}
\usage{
getReference(reference,variable,verbose=FALSE)
getCFSR(variable="tas",destfile=NULL,lon=NULL,lat=NULL,
        griddes="cmip_1.25deg_to_2.5deg.txt",verbose=FALSE)
getCRU(username,passwd,variable="tmp",version="4.00",
       griddes="cmip_1.25deg_to_2.5deg.txt",destfile=NULL,
       time.resol=NULL, verbose=FALSE)
getEOBS(variable="tas", destfile=NULL, version="17", lon=NULL, lat=NULL, verbose=FALSE)
getERA(variable,start=1979,end=2017,griddes="cmip_1.25deg_to_2.5deg.txt",
       destfile=NULL,force=FALSE,verbose=FALSE)
python.getEra(start,end,varid,steps,type,stream,outfile,verbose=FALSE)
}
\arguments{
\item{reference}{Reference data set: 'era', 'cfsr' or 'eobs'}
\item{variable}{Climate variable, e.g. 'tas' (temperature) and 'pr' (precipitation)}
\item{destfile}{filename of output file (netCDF file with extension '.nc')}
\item{lon}{longitude range, e.g. c(-20,20)}
\item{lat}{latitude range, e.g. c(45,55)}
\item{griddes}{file describing the output grid (the default 'cmip_1.25deg_to_2.5deg.txt' is included in the DECM package)}
\item{username}{username to access the CEDA datasets (to download CRU data)}
\item{passwd}{password to access the CEDA datasets (to download CRU data)}
\item{time.resol}{time resolution}
\item{version}{data set version (EOBS data)}
\item{start}{start year}
\item{end}{end year}
\item{varid}{variable ID for getting ERAinterim data from ECMWF, e.g. 167.128 for temperature and 228.128 for precipitation}
\item{steps}{If step "0" is chosen, only analysed fields are downloaded, otherwise (e.g if steps = "0-12") forecast fields will also be included (recommended for precipitation to account for spinup time)}
\item{type}{'fc' for forecasts or 'an' for analysis}
\item{stream}{forecasting system used to generated the data, e.g. 'moda' (monthly means of daily means), 'mdfa' (monthly means of daily forecast accumulations)}
\item{outfile}{output file}
\item{force}{a logical, if TRUE a new output file will be created to overwrite the old one}
\item{verbose}{if TRUE display information on progress}
}
\examples{
\dontrun{
getCFSR(variable="tas")
getEOBS(variable="pr")
}
}
\keyword{file}