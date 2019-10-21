\name{check.ncdf4}
\alias{check.ncdf4}
\title{Check netcdf file}
\usage{
check.ncdf4(ncid, param = "auto", verbose = FALSE)
}
\arguments{
\item{ncid}{a "ncdf4" object (output of the \code{ncdf4} function \code{nc_open})}
\item{param}{name of parameter}
\item{verbose}{if TRUE print information about progress}
}
\description{
Check content of netcdf file including parameters, units, and time format (frequency, calendar type).
}
