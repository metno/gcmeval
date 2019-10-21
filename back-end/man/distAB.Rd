\name{distAB}
\alias{distAB}
\title{Calculate distance between points on earth}
\usage{
distAB(lon, lat, lons, lats, a = 6378000)
}
\arguments{
\item{lon}{a longitude}

\item{lat}{a latitude}

\item{lons}{longitude or vector of longitudes}

\item{lats}{latitude or vector of longitudes}

\item{a}{radius of earth (unit: m)}
}
\value{
distance between [lon, lat] and [lons, lats] (unit: m)
}
\description{
Calculate distance between points on earth.
}
