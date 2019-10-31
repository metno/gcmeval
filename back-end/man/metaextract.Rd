\name{metaextract}
\alias{metaextract}
\alias{metaextract.cmip}
\title{
Extract metadata
}
\description{
Extract metadata for GCMs.
}
\usage{
metaextract(files.in,file.out="meta.rda",path.in=NULL,path.out=NULL,add=TRUE,force=FALSE,verbose=FALSE)
metaextract.cmip(x, verbose=FALSE)
}
\arguments{
  \item{files.in}{a list of netCDF files containing GCM data}
  \item{file.out}{Filename for output.}
  \item{path.in}{path to input files}
  \item{path.out}{path to output files}
  \item{add}{If TRUE add to existing metadata file, if FALSE create new.}
  \item{force}{If TRUE replace existing entries in metadata file}
  \item{verbose}{If TRUE, display information on progress.}
  \item{x}{a 'ncdf4' object, generated with e.g. \code{getncid} or \code{getCM}.}
}
\value{
\item{meta}{A data.frame containing metadata.}
}
\author{
Kajsa Parding, kajsamp@met.no
}

\examples{
\dontrun{
f <- list.files(path="/path/to/GCMfiles", pattern=".nc")
M <- metaextract(f, verbose=TRUE)
}
}

\keyword{metadata}