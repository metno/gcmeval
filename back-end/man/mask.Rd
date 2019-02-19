\name{mask}
\alias{gen.mask.srex}
\alias{get.srex.region}
\alias{get.shapefile}
\alias{mask.zoo}
\title{Create and apply masks}
\description{
\code{gen.mask.srex} creates a raster mask for the selected SREX sub-region from the CMIP5 netCDF file.
\code{get.srex.region} uses the raster mask to get grid boxes belonging to a SREX region and then calculates some statistics for it
\code{get.shapefile} imports a shapefile
\code{mask.zoo} applies a mask to a 'zoo' object
}
\usage{
gen.mask.srex(destfile, mask.polygon=NULL, ind=FALSE, inverse=FALSE, 
              mask.values=1, verbose=FALSE) 
get.srex.region(destfile,region=NULL,print.srex=FALSE,verbose=FALSE)
get.shapefile(filename, with.path=FALSE)
mask.zoo(zoo.object,mask)
}
\arguments{
  \item{destfile}{output filename}
  \item{mask.polygon}{a spatial object obtained with get.shapefile (a SpatialPolygonsDataFrame), or NULL if no region is to be masked}
  \item{ind}{if TRUE return indices}
  \item{inverse}{if TRUE mask the region outside of mask.polygon}
  \item{mask.values}{output values of masked region}
  \item{region}{name of SREX region, if NULL all regions are selected}
  \item{print.srex}{print information about SREX regions}
  \item{filename}{name of a shapefile}
  \item{with.path}{if TRUE assume filename is a complete path, if FALSE locate file with find.file}
  \item{zoo.object}{an object of class 'zoo'}
  \item{mask}{a mask}
  \item{verbose}{if TRUE display information on progress, if FALSE run in silent mode}
}
\value{
\code{gen.mask.srex} generates a 'RasterLayer' object.
\code{get.srex.region} generates a list with information (name, label) and summary statistics (area.mean, area.sd) for the selected region.
\code{get.shapefile} returns a 'SpatialPolygonsDataFram' object.
\code{mask.zoo} returns a masked 'zoo' object.
}
\examples{

# Example file
gcm.file <- system.file("extdata", "gcm.nc", package = "DECM")

# Import SREX regions from shapefile and create mask for gcm.file
shape <-  get.shapefile("referenceRegions.shp")
mask <- gen.mask.srex(gcm.file, mask.polygon=shape[16,], verbose=TRUE)

gcm <- zoo::coredata(esd::retrieve.ncdf4(gcm.file))
gcm.masked <- mask.zoo(gcm, mask)

\dontrun{
# Download GCM data
x <- getGCMs(select=1, varid="pr", experiment="rcp85")
# Get SREX region for Northern Europe (NEU)
srex <- get.srex.region(x[[1]]$filename,"NEU")
}
}
\keyword{mask}
\keyword{shapefile}