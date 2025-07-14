## Script to extract metadata and calculate statistics and prepare
## the metadata (meta.rda) and statistics files (statistics.rda).
## The files (meta.rda and statistics.rda) should then be moved back-end/data.
##
#!/usr/bin/env Rscript

## Install gcmeval from github:
if(!require(gcmeval)) {
  if(!require(devtools)) install.packages('devtools')
  library(devtools)
  install_github('metno/gcmeval/back-end')
}
library(gcmeval)

## To install gcmeval package from local repository: 
## R CMD INSTALL gcmeval/back-end

## Set paths...
## to the gcmeval app
path.gcmeval <- "/path/to/app"

##...to GCM data:
path.cmip5 <- "/path/to/CMIP5"
path.cmip6 <- "/path/to/CMIP6"

##...to the metadata and statistics files:
path.out <- file.path(path.gcmeval, "R-scripts") 

##...to reference data:
path.ref <- "/path/to/ref"
ref.tas <- c("era5","eraint")
ref.pr <- c("era5","eraint","gpcp")
## Names of reference data files. Change in accordance with the local file names.
files.ref <- list("era5.tas"="era5_monthly_1979-2018_tas.2.5deg.nc",
                  "era5.pr"="era5_monthly_1979-2018_pr.2.5deg.nc",
                  "eraint.tas"="era-interim_monthly_1979-2017_tas.2.5deg.nc",
                  "eraint.pr"="era-interim_monthly_1979-2017_pr.2.5deg.nc",
                  "gpcp"="gpcp.nc")

## Shape file with region information. SREX regions are provided with the GCMeval code.
## Other regions could be added using another shape file.
file.shape <- file.path(path.gcmeval,
  "back-end/inst/extdata/SREX_regions/referenceRegions.shp")

## Set options for calculations:
calculate_meta <- FALSE
calculate_statistics <- TRUE
download_ref <- FALSE
download_cmip5 <- FALSE
verbose <- TRUE

## 2025-07-14: The following code for downloading data does not work anymore.
## Until it has been updated, both GCM and reference data has to be downloaded
## elsewhere, e.g., from the Climate Data Store (CDS; https://cds.climate.copernicus.eu/),
## one of the ESGF nodes (e.g., https://esgf.nci.org.au/search) or
## from KNMI Climate explorer (https://climexp.knmi.nl/start.cgi).
## If the data comes in several files for different time slices, the files should be 
## concatenated before applying the functions 'metaextract' and 'calculate.statistics'.
#
# =======================================================================
## DOWNLOAD AND PREPARE DATA
## Downloading CMIP5 data from the KNMI climate explorer using the getCMIP5 function.
#if(download_cmip5) {
#  getCMIP5(select=1:110, varid="tas", experiment="rcp45", path=path.cmip5)
#  getCMIP5(select=1:110, varid="tas", experiment="rcp85", path=path.cmip5)
#  getCMIP5(select=1:110, varid="pr", experiment="rcp45", path=path.cmip5)
#  getCMIP5(select=1:110, varid="pr", experiment="rcp85", path=path.cmip5)
#}
#
## The CMIP6 ensemble and other data that are not available from the KNMI climate explorer
## can be downloaded from CDS (https://cds.climate.copernicus.eu/api-how-to).
#
## Download reference data
#if(download_ref) {
#  for (varid in c("tas","pr")) {
#    ref.var <- switch(varid, "tas"=ref.tas, "pr"=ref.pr)
#    for(ref in ref.var) {
#      if(is.logical(getReference(ref,varid))) { 
#        if(verbose) print("Download reference data")
#        if(ref=="eraint") {
#          getERAint(variable=varid, verbose=verbose, path=path.ref)
#        } else if(ref=="era5") {
#          getERA5(variable=varid, python="python3", verbose=verbose, path=path.ref)
#        } else if(ref=="eobs") {
#          getEOBS(variable=varid, verbose=verbose, path=path.ref)
#        } else if(ref=="gpcp") {
#          getGPCP(verbose=verbose, path=path.ref)
#        }
#      }
#    }
#  }
#}
## =====================================================================================

# Generate list of GCM files:
files.cmip6 <- list.files(path=path.cmip6, pattern=".nc", full.names=TRUE)
files.cmip5 <- list.files(path=path.cmip5, pattern=".nc", full.names=TRUE)
files.gcm <- c(files.cmip6, files.cmip5)
## Files can also be listed manually:
## files.gcm <- c("file1.nc", "file2.nc", ...)


## Generate metadata and store in the file meta.rda
## add=TRUE: Add new metadata to the old metadata file. 
## add=FALSE: Create a new metadata file.
## force=TRUE: When running metaextract on a file that is already in the metadata, extract new metadata and replace the old entry.
## force=FALSE: When running metaextract on a file that is already in the metadata, do not extract new metadata.
if(calculate_meta) {
  add <- TRUE
  force <- FALSE
  meta <- metaextract(files.gcm, file.out="meta.rda", path.out=path.out,
                      add=add, force=force, verbose=verbose)
} else if(file.exists(file.path(path.out, "meta.rda"))) {
  load(file.path(path.out, "meta.rda"))
} else {
  meta <- NULL
}

if(calculate_statistics) {
  ## Calculate statistics for future periods
  force <- FALSE
  add <- TRUE
  for(it in list(c(2071,2100),c(2021,2050))) {
    stats <- c("mean.gcm","spatial.sd.gcm")
    x <- calculate.statistics(files.gcm, meta=meta, file.out="statistics.rda", 
                              path.out=path.out, file.shape=file.shape, 
                              ref=NULL, period=it, stats=stats, 
                              add=add, force=force, verbose=verbose)
    add <- TRUE
  }
  
  ## Calculate statistics for the past
  ## GCM statistics (mean.gcm and spatial.sd.gcm) only has to be calculated once 
  stats <- c("mean.gcm","spatial.sd.gcm","mean.ref","spatial.sd.ref","corr","rmse","cmpi")
  for(ref in unique(c(ref.tas,ref.pr))) {
    x <- calculate.statistics(files.gcm, meta=meta, file.out="statistics.rda", 
                              path.out=path.out, file.shape=file.shape, 
                              ref=ref, path.ref=path.ref, files.ref=files.ref,
                              period=c(1981,2010), 
                              stats=stats, add=add, force=force, verbose=verbose)
    add <- TRUE
    stats <- c("mean.ref","spatial.sd.ref","corr","rmse","cmpi")
  }
}