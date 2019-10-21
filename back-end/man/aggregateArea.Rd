\name{aggregateArea}
\alias{aggregateArea}
\title{aggregate}
\usage{
aggregateArea(x, ..., is = NULL, it = NULL, FUN = "sum",
  na.rm = TRUE, smallx = FALSE, verbose = FALSE, a = 6378,
  threshold = NULL)
}
\arguments{
\item{x}{A data object to be aggregated}

\item{is}{A list or data.frame providing space index, see \code{\link{subset}}}

\item{it}{A list or data.frame providing time index, see \code{\link{subset}}}

\item{FUN}{A function, e.g., 'sum' or 'mean'}

\item{na.rm}{a boolean; if TRUE ignore NA, see \code{\link{mean}}}

\item{smallx}{a boolean defaulting to FALSE}

\item{verbose}{a boolean; if TRUE print information about progress}

\item{a}{radius of earth (unit: km)}

\item{threshold}{threshold to be used if FUN is 'area','exceedance', or 'lessthan'}

\item{\dots}{other arguments}
}
\value{
The call returns a regionally aggregated zoo object.
}
\description{
The aggregation functions are based on the S3 method for \code{zoo} objects,
but takes care of extra house keeping, such as attributes with meta data.
}
\details{
\code{aggregateArea} is used for aggregating spatial statistics, such as
the global mean or the global area of some phenomenon.
}