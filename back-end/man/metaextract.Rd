\name{metaextract}
\alias{metaextract}
\alias{metaextract.cmip5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract metadata
}
\description{
Extract metadata for GCMs.
}
\usage{
metaextract(x=NULL,add=TRUE,file="meta.rda",verbose=FALSE)
metaextract.cmip5(x=NULL, experiment="rcp45", verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{List of netCDF files, generated with e.g. getCM, or getGCMs.}
  \item{add}{If TRUE add to existing metadata file, if FALSE create new.}
  \item{file}{Filename for output.}
  \item{experiment}{parent experiment, e.g. emission scenario 'rcp45' or 'rcp85'}
  \item{verbose}{If TRUE, display information on progress.}
}
\value{
\item{meta}{List containing metadata of files x.}
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