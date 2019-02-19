## Script to extract metadata and calculate statistics.
## The metadata (metaextract.rda) and statistics files (statistics.cmip.era.tas.1981-2010.rda)
## should then be moved to the back-end/data folder.

#!/usr/bin/env Rscript
library(gcmeval)
dir <- find.file("calculate_statistics.R")
path <- dirname(dir[grepl("gcmeval",dir)])
setwd(path)

## To install DECM package: 
## R CMD INSTALL DECM/back-end 
## Requires rgdal, raster, esd (zoo, ncdf4), PCICt, RCurl

get.meta <- TRUE
cmip.rmse <- TRUE
cmip.stats <- TRUE

## Calculate statistics for CMIP5 data
opt <- list(ref.tas="era", ref.pr="eobs", verbose=TRUE,
            it.ref=c(1981,2010), nfiles="all", continue=TRUE,
	          mask="coords.txt", path=path)

# Download reference data
if(cmip.rmse | cmip.stats) {
  for (varid in c("tas","pr")) {
    ref.var <- switch(varid, "tas"=opt$ref.tas, "pr"=opt$ref.pr)
    ref <- getReference(ref.var,varid)
    # ref=FALSE if file is missing, else ref=filename
    if(is.logical(ref)) { 
      if(opt$verbose) print("Download reference data")
      if(ref.var=="era") {
        getERA(variable=varid,verbose=opt$verbose)
      } else if(ref.var=="eobs") {
        getEOBS(variable=varid, verbose=opt$verbose)
      }
    }
  }
}

if(get.meta) {
  # Set add=FALSE to create new metadata file or TRUE to add to old file
  add <- FALSE
  for(varid in c("tas","pr")) {
    for(rcp in c("rcp45","rcp85")) {
      x <- getGCMs(select=1:110,varid=varid,experiment=rcp,
                   verbose=opt$verbose,path=opt$path)
      y <- metaextract(x,verbose=opt$verbose,add=add)
      add <- TRUE # change add to TRUE so metadata is added to old file
    }
  }
}

# Calculate regional statistics (mean, sd, spatial corr) for CMIP5
if(cmip.stats) {
  for (varid in c("pr","tas")) {
    print(paste("Calculate annual cycle statistics of",varid))
    for (it in list(c(2071,2100),c(2021,2050),opt$it)) {
      for(rcp in c("rcp85","rcp45")) {
        print(paste("period:",paste(it,collapse="-"),"; scenario:",rcp))
        calculate.statistics.cmip(reference=opt$ref.cmip, period=it,
	  variable=varid, path.gcm=opt$path, nfiles=opt$nfiles,
	  continue=opt$continue, mask=opt$mask, experiment=rcp,
	  verbose=opt$verbose)
      }
    }
  }
}

# Calculate rmse and CMPI for CMIP5
if(cmip.rmse) {
  for (varid in c("pr","tas")) {
    print(paste("Calculate weighted RMSE of",varid))
    for(rcp in c("rcp85","rcp45")) {
      print(paste("Scenario:",rcp))
      calculate.rmse.cmip(reference=opt$ref.cmip, period=opt$it, variable=varid, 
                          path.gcm=opt$path, nfiles=opt$nfiles, continue=opt$continue, 
                          experiment=rcp, verbose=opt$verbose)
    }
  }
}
