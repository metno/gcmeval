## Script to extract metadata and calculate statistics and prepare
## the metadata (meta.rda) and statistics files (statistics.rda).
## The files (meta.rda and statistics.rda) should then be moved back-end/data.
##
#!/usr/bin/env Rscript

# Set path to GCM data here:
path <- "/vol/lustre/storeA/users/kajsamp/Data/CMIP5/KNMI"

## To install gcmeval package from local repository: 
## R CMD INSTALL gcmeval/back-end
if(!require(gcmeval) {
  if(!require(devtools)) install.packages('devtools')
  library(devtools)
  install_github('metno/gcmeval/back-end')
}
library(gcmeval)

get.meta <- TRUE
get.stats <- TRUE

## Calculate statistics for CMIP5 data
# Alternatives for ref: tas: eraint, era5; pr: eraint, era5, gpcp
opt <- list(ref.tas=c("era5","eraint"), ref.pr=c("era5","eraint","gpcp"),
            verbose=TRUE, it.ref=c(1981,2010), nfiles="all",
	          add=TRUE, force=FALSE, mask="coords.txt", path=path)

# Download reference data
if(get.stats) {
  for (varid in c("tas","pr")) {
    ref.var <- switch(varid, "tas"=opt$ref.tas, "pr"=opt$ref.pr)
    for(ref in ref.var) {
      if(is.logical(getReference(ref,varid))) { 
        if(opt$verbose) print("Download reference data")
        if(ref=="eraint") {
          getERAint(variable=varid, verbose=opt$verbose)
        } else if(ref=="era5") {
          getERA5(variable=varid, python="python", verbose=opt$verbose)
        } else if(ref=="eobs") {
          getEOBS(variable=varid, verbose=opt$verbose)
        } else if(ref=="gpcp") {
          getGPCP(verbose=opt$verbose)
        }
      }
    }
  }
}

# Set add=FALSE to create new metadata file or TRUE to add to old file
if(get.meta) {
  add <- opt$add
  for(varid in c("tas","pr")) {
    for(rcp in c("rcp45","rcp85")) {
      x <- getGCMs(select=1:110,varid=varid,experiment=rcp,
                   verbose=opt$verbose,path=opt$path)
      y <- metaextract(x,verbose=opt$verbose,add=add,file="meta.rda")
      add <- TRUE # change add to TRUE so metadata is added to old file
    }
  }
}

# Calculate regional statistics for CMIP5 
# (spatial mean, sd, corr, seasonal cycle rmse, cmpi)
if(get.stats) {
  stats <- c("mean","spatial.sd","corr","rmse")
  for (varid in c("pr","tas")) {
    print(paste("Calculate annual cycle statistics of",varid))
    for(rcp in c("rcp85","rcp45")) {
      for (it in list(opt$it.ref,c(2071,2100),c(2021,2050))) {
        print(paste("period:",paste(it,collapse="-"),
                    "; scenario:",rcp))
        if(all(it==opt$it.ref)) {
          ref.var <- switch(varid, "tas"=opt$ref.tas, "pr"=opt$ref.pr)
          for(ref in ref.var) {
            calculate.statistics.cmip5(reference=ref, period=it,
                                       variable=varid, path.gcm=opt$path, nfiles=opt$nfiles,
                                       add=opt$add, mask=opt$mask, experiment=rcp,
                                       verbose=opt$verbose, force=opt$force, stats=stats)
          }
        } else {
          calculate.statistics.cmip5(reference=NULL, period=it,
	          variable=varid, path.gcm=opt$path, nfiles=opt$nfiles,
	          add=opt$add, mask=opt$mask, experiment=rcp,
	          verbose=opt$verbose, force=opt$force, stats=stats)
     	  }
      }
    }
  }
}
