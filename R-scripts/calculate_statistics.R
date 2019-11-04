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
##...to reference data:
path.ref <- "/path/to/ref"
##...to GCM data:
path.cmip5 <- "/path/to/CMIP5"
path.cmip6 <- "/path/to/CMIP6"
##...to the metadata and statistics files:
path.out <- "~/git/gcmeval/R-scripts"

## Set options for calculations:
calculate_meta <- FALSE
calculate_stats <- TRUE
download_ref <- FALSE
download_cmip5 <- FALSE

ref.tas <- c("era5","eraint")
ref.pr <- c("era5","eraint","gpcp")
verbose <- TRUE

## DOWNLOAD AND PREPARE DATA
## The CMIP5 ensemble can be downloaded from the KNMI climate explorer using the getCMIP5 function.
if(download_cmip5) {
  path.cmip5 <- "/home/kajsamp/Documents/Data/CMIP5"
  getCMIP5(select=1:110, varid="tas", experiment="rcp45", path=path.cmip5)
  getCMIP5(select=1:110, varid="tas", experiment="rcp85", path=path.cmip5)
  getCMIP5(select=1:110, varid="pr", experiment="rcp45", path=path.cmip5)
  getCMIP5(select=1:110, varid="pr", experiment="rcp85", path=path.cmip5)
} 
## The CMIP6 ensemble and other data that are not available from the climate explorer
## can be downloaded from the climate data store (https://cds.climate.copernicus.eu/api-how-to).
## If the data comes in several files for different time slices, the files should be 
## concatenated before applying the functions 'metaextract' and 'calculate.statistics'.

# Generate list of GCM files:
files.cmip6 <- list.files(path=path.cmip6, pattern=".nc", full.names=TRUE)
files.cmip5 <- list.files(path=path.cmip5, pattern=".nc", full.names=TRUE)
files.gcm <- c(files.cmip6, files.cmip5)

## REFERENCE DATA
## If you already have the reference data you can put them in path.ref with the following filenames:
## ERA5 tempterature: "era5_monthly_1979-2018_tas.2.5deg.nc",
## ERA5 precipitation: "era5_monthly_1979-2018_pr.2.5deg.nc",
## ERAinterim temperature: "era-interim_monthly_1979-2017_tas.2.5deg.nc",
## ERAinterim temperature: "era-interim_monthly_1979-2017_pr.2.5deg.nc",
## GPCP (precipitation): "gpcp.nc"
## EOBS temperature: "tg_0.50deg_reg_v17.0_mon.nc",
## EOBS precipitation: "tg_0.50deg_reg_v17.0_mon.nc",
## If you don't already have it, download the reference data
## with the functions getERA5, getERAint, getGPCP, getEOBS.
## (Note that getERA5 requires installing the CDS API key and client
## and getERAint requires installing the ECMWF API key and client)
if(download_ref) {
  for (varid in c("tas","pr")) {
    ref.var <- switch(varid, "tas"=ref.tas, "pr"=ref.pr)
    for(ref in ref.var) {
      if(is.logical(getReference(ref,varid))) { 
        if(verbose) print("Download reference data")
        if(ref=="eraint") {
          getERAint(variable=varid, verbose=verbose, path=path.ref)
        } else if(ref=="era5") {
          getERA5(variable=varid, python="python", verbose=verbose, path=path.ref)
        } else if(ref=="eobs") {
          getEOBS(variable=varid, verbose=verbose, path=path.ref)
        } else if(ref=="gpcp") {
          getGPCP(verbose=verbose, path=path.ref)
        }
      }
    }
  }
}

## Generate metadata and store in the file meta.rda
## add=TRUE: Add new metadata to the old metadata file. 
## add=FALSE: Create a new metadata file.
## force=TRUE: When running metaextract on a file that is already in the metadata, extract new metadata and replace the old entry.
## force=FALSE: When running metaextract on a file that is already in the metadata, do not extract new metadata.
if(calculate_meta) {
  add <- TRUE
  force <- TRUE
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
    x <- calculate.statistics(files.gcm, meta=meta, file.out="statistics.rda", path.out=path.out,
                              ref=NULL, period=it, stats=stats, add=add, force=force, verbose=verbose)
    add <- TRUE
  }

  ## Calculate statistics for the past
  stats <- c("mean.gcm","spatial.sd.gcm","mean.ref","spatial.sd.ref","corr","rmse","cmpi")
  for(ref in unique(c(ref.tas,ref.pr))) {
    x <- calculate.statistics(files.gcm, meta=meta, file.out="statistics.rda", path.out=path.out,
                              ref=ref, path.ref=path.ref, period=c(1981,2010), 
                              stats=stats, add=add, force=force, verbose=verbose)
    add <- TRUE
    stats <- c("mean.ref","spatial.sd.ref","corr","rmse","cmpi")
  }
}

