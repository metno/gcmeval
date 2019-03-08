## Script to extract metadata and calculate statistics.
## The metadata (metaextract.rda) and statistics files (statistics.cmip.era.tas.1981-2010.rda)
## should then be moved to the back-end/data folder.

#!/usr/bin/env Rscript
library(gcmeval)
dir <- find.file("calculate_statistics.R")
path <- dirname(dir[grepl("gcmeval",dir)])
setwd(path)

## To install gcmeval package: 
## R CMD INSTALL gcmeval/back-end 
## Requires rgdal, raster, esd (zoo, ncdf4), RCurl

get.meta <- FALSE
get.stats <- TRUE

## Calculate statistics for CMIP5 data
opt <- list(ref.tas="eraint", ref.pr="eraint", verbose=TRUE,
            it.ref=c(1981,2010), nfiles="all", continue=TRUE,
	          mask="coords.txt", path=path)

# Download reference data
if(get.stats) {
  for (varid in c("tas","pr")) {
    ref.var <- switch(varid, "tas"=opt$ref.tas, "pr"=opt$ref.pr)
    ref <- getReference(ref.var,varid)
    # ref=FALSE if file is missing, else ref=filename
    if(is.logical(ref)) { 
      if(opt$verbose) print("Download reference data")
      if(ref.var=="eraint") {
        getERA(variable=varid, verbose=opt$verbose)
      } else if(ref.var=="era5") {
        getERA5(variable=varid, python="python3", verbose=opt$verbose)
      } else if(ref.var=="eobs") {
        getEOBS(variable=varid, verbose=opt$verbose)
      }
    }
  }
}

if(get.meta) {
  # Set add=FALSE to create new metadata file or TRUE to add to old file
  add <- TRUE
  for(varid in c("tas","pr")) {
    for(rcp in c("rcp45","rcp85")) {
      x <- getGCMs(select=1:110,varid=varid,experiment=rcp,
                   verbose=opt$verbose,path=opt$path)
      y <- metaextract(x,verbose=opt$verbose,add=add)
      #add <- TRUE # change add to TRUE so metadata is added to old file
    }
  }
}

# Calculate regional statistics for CMIP5 
# (spatial mean, sd, corr, seasonal cycle rmse, cmpi)
if(get.stats) {
  force <- TRUE
  for (varid in c("pr","tas")) {
    print(paste("Calculate annual cycle statistics of",varid))
    ref.var <- switch(varid, "tas"=opt$ref.tas, "pr"=opt$ref.pr)#
    for (it in list(opt$it.ref,c(2071,2100),c(2021,2050))) {
      for(rcp in c("rcp85","rcp45")) {
        print(paste("period:",paste(it,collapse="-"),
                    "; scenario:",rcp))
        calculate.statistics.cmip(reference=ref.var, period=it,
	        variable=varid, path.gcm=opt$path, nfiles=opt$nfiles,
	        continue=!force, mask=opt$mask, experiment=rcp,
	        verbose=opt$verbose)
      }
    }
  }
}
