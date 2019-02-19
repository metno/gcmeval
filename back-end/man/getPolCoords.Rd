\name{getPolCoords}
\alias{getPolCoords}
\title{Get Polar coordinates}
\description{
\code{getPolCoords} transforms coordinates of a region from Cartesian to polar.
}
\usage{
getPolCoords(region,shape=NULL,destfile="coords.txt")
}
\arguments{
  \item{region}{name of SREX region, if NULL all regions are selected}
  \item{shape}{filename of a shape file (if NULL use the SREX region shape file included in DECM)}
  \item{destfile}{output file}
}
\examples{
# Northern Europe (SREX region)
getPolCoords("NEU",destfile="coords.txt")
x <- read.table("coords.txt")
print(x)
}
\keyword{file}