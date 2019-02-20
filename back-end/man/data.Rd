\name{data}
\alias{meta}
\alias{statistics}
\title{Metadata and statistics}
\description{
\code{meta} contains metadata of the CMIP5 GCM ensemble and \code{statistics} contains statistics for the same GCM runs. The statistical measures included are: 1) the annual mean and seasonal cycle of the spatial standard deviation and mean, 2) the spatial correlation between the GCM and a reference data set, and 3) the RMSE between the seasonal cycle of the GCM and a reference data set as well as the CMPI (climate model performance index) which is the normalized RMSE.

The script used to extract metadata and calculate statistics (calculate_statistics.R) can be found in the gcmeval Github repository in the folder gcmeval/R-scripts/. The metadata was extracted with the function \code{metaextract} and the statistics calculated with \code{calculate.statistics.cmip}.}
\usage{
data(meta)
data(statistics)
}
\format{list}
\keyword{statistics}
\keyword{metadata}
\keyword{data}
