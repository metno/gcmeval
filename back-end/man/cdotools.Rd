\name{cdo.command}
\alias{cdo.command}
\alias{cdo.mean}
\alias{cdo.spatSd}
\alias{cdo.gridcor}

\title{
   CDO calculations in R
}
\description{
  Do operations on netCDF files with CDO. More efficient than doing the calculations directly in R.
}
\usage{
cdo.command(commands,input,infile,outfile,bit=8,intern=FALSE)
cdo.mean(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
         monthly=FALSE,is.temp=TRUE,outfile=NULL,bit=8,verbose=FALSE)
cdo.spatSd(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
           monthly=FALSE,bit=8,verbose=FALSE)
cdo.gridcor(model.file,reference.file,period=c(1981,2010),mask=NULL,
            seasonal=FALSE,monthly=FALSE,bit=8,verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{commands}{the system commands to be invoked}
  \item{input}{input to the commands}
  \item{infile}{path to input netCDF file}
  \item{outfile}{path to output netCDF file}
  \item{bit}{number of bits for the output precision (e.g. 8, 16, 32, 64)}
  \item{intern}{A logical (not NA) which indicates whether to capture the output of the command as an R character vector.}
  \item{model.file}{path to netCDF file}
  \item{reference.file}{path to netCDF another file}
  \item{period}{a subset of years to include in calculations}
  \item{mask}{a spatial mask}
  \item{seasonal}{if TRUE return seasonal mean values}
  \item{monthly}{if TRUE return monthly mean values}
  \item{is.temp}{a logical indicating if the input is temperature data, in which case some corrections may be applied}
  \item{verbose}{if TRUE display information on progress}
}
\value{
\item{out}{A vector containing statistical values}
}
\author{
Olle Raty
}
\examples{
# Example files: 
gcm.file <- system.file("extdata", "gcm.nc", package = "DECM")
ref.file <- system.file("extdata", "ref.nc", package = "DECM")
# Set period
period <- c(1990,1993)

# Calculate annual mean spatial standard deviation
sd.space <- cdo.spatSd(ref.file, period, monthly=FALSE)

# Calculate monthly mean
meanval <- cdo.mean(ref.file, period, monthly=TRUE)

# Calculate spatial correlation between GCM and reference data
cor <- cdo.gridcor(gcm.file, ref.file, period)

\dontrun{
# Use cdo.command to calculate the monthly mean
cdo.command(c("-ymonmean","-fldmean"), c("",""), ref.file, "tmp.nc")
out <- as.numeric(cdo.command("output","","tmp.nc",NULL,intern=TRUE))
}
}

\keyword{cdo}
\keyword{cdotools}