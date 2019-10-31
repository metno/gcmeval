\name{tools}
\alias{cleanstr}
\alias{find.file}
\alias{gunzip}
\alias{resolution}
\title{Find files}
\description{
Helper function to perform simple tasks in a linux environment
}
\usage{
find.file(filename)
gunzip(filename)
resolution(filename,dim=c("lon","longitude"),verbose=FALSE)
cleanstr(x)
}
\arguments{
  \item{filename}{name of file}
  \item{dim}{name of dimension to evaluate resolution based on}
  \item{}
  \item{verbose}{If TRUE, display information on progress.}
}
\examples{
find.file("referenceRegions.shp")
}
\keyword{file}