\name{metaextract}
\alias{metaextract}
\alias{metaextract.cmip}
\alias{metaextract.v2}
\alias{metaextract.cmip5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract metadata
}
\description{
Extract metadata for GCMs.
\code{metaextract.v2} is a new version of \code{metaextract} that can deal with both CMIP5 and CMIP6 data.
\code{metaextract.v2} applies the function \code{metaextract.cmip} to each file in \code{files.in} and arranges the output.
}
\usage{
metaextract.v2(files.in,file.out="meta.rda",path.in=NULL,path.out=NULL,add=TRUE,force=FALSE,verbose=FALSE)
metaextract.cmip(x=NULL, experiment="rcp45", verbose=FALSE)
metaextract(x=NULL,add=TRUE,file="meta.rda",verbose=FALSE)
metaextract.cmip5(x=NULL, experiment="rcp45", verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files.in}{a list of netCDF files containing GCM data}
  \item{x}{a 'ncdf4' object, generated with e.g. \code{getcid} or \code{getCM}.}
  \item{add}{If TRUE add to existing metadata file, if FALSE create new.}
  \item{file.out}{Filename for output.}
  \item{experiment}{parent experiment, e.g. emission scenario 'rcp45' or 'rcp85'}
  \item{verbose}{If TRUE, display information on progress.}
}
\value{
\item{meta}{A data.frame containing metadata.}
}
\author{
Kajsa Parding, kajsamp@met.no
}

\examples{
\dontrun{
x <- getGCMs(select=1:5, varid="tas")
M <- metaextract(x)
}
}

\keyword{metadata}