metaextract.cmip6 <- function(x=NULL, experiment="ssp585", verbose=FALSE) {
  if(verbose) print("metaextract.cmip6")
  ## argument 'x' is input from getGCMs, getRCMs, testGCM, etc
  if (is.null(x)) x <- getGCMs(experiment=experiment,verbose=verbose)
  
  if(!inherits(x,"list")) x <- list(gcm.1=x)
  gcms <- names(x)
  n <- length(gcms)
  
  for (i in 1:n) {
    xx <- x[[gcms[i]]]
    mip_era <- NA; url <- NA; filename <- NA; dim <- NA; dates <- NA
    var <- NA; longname <- NA; vunit <- NA; vid <- NA
    res <- NA; lon.rng <- NA; lon.unit <- NA; lat.rng <- NA; lat.unit <- NA
    experiment <- NA; #experiment_id <- NA; 
    frequency <- NA; creation_date <- NA; tracking_id <- NA
    gcm <- NA; gcm.v <- NA; gcm.realm <- NA
    gcm.ripf <- NA; realization_index <- NA; initialization_index <- NA; physics_index <- NA; forcing_index <- NA;
    qf <- NULL
    if(!is.null(xx$dim)) dim <- paste(names(xx$dim),collapse=",")
    if(!is.null(names(xx$var))) {
      var <- names(xx$var)
      if(!is.null(xx$var[[1]]$longname)) longname <- sapply(var, function(x) xx$var[[x]]$longname)
      if(!is.null(xx$var[[1]]$units)) vunit <- sapply(var, function(x) xx$var[[x]]$units)
      if(!is.null(xx$var[[1]]$id$id)) vid <- sapply(var, function(x) xx$var[[x]]$id$id)
    }
    if(!is.null(names(xx$dim))) {
      if(!is.null(xx$dim$lat$vals)) {
        res <- diff(xx$dim$lat$vals)[1]
        lat.rng <- paste(range(xx$dim$lat$vals),collapse=",")
      }
      if(!is.null(xx$dim$lon$vals)) lon.rng <- paste(range(xx$dim$lon$vals),collapse=",")
      if(!is.null(xx$dim$lat$units)) lat.unit <- xx$dim$lat$units
      if(!is.null(xx$dim$lon$units)) lon.unit <- xx$dim$lon$units
    }
    for(mi in c("url","filename","dates","frequency",
                "mip_era","experiment","experiment_id",
                "creation_date","tracking_id",
                "realization_index","initialization_index","physics_index","forcing_index")) {
      if(!is.null(xx[[mi]])) {
        eval(parse(text=paste(mi," <- xx$",mi,sep="")))
      } else if (!is.null(xx$model[[mi]])) {
        eval(parse(text=paste(mi," <- xx$model$",mi,sep="")))
      }
    }
    if(!is.null(xx$model$model_id)) gcm <- xx$model$model_id
    if(!is.null(xx$model$parent_experiment_ripf)) {
      gcm.ripf <- xx$model$parent_experiment_ripf
    }
    if(!is.null(xx$model$version_number)) gcm.v <- xx$model$version_number
    if(!is.null(xx$model$modeling_realm)) gcm.realm <- xx$model$modeling_realm
    
    ## If file is missing information in netCDF header,
    ## use the model history to obtain it. 
    if(is.null(xx$model$model_id) & xx$mip_era=="CMIP6") {
      h <- xx$model$history
      creation_date <- substr(h, 1, regexpr("[0-9]{4}",h)+3)
      h <- strsplit(h," ")
      fname <- unlist(h)[grep("ssp.*.nc",unlist(h))[1]]
      gcm <- gsub("_ssp.*","",gsub(".*_Amon_","",fname))
      gcm.rip <- substr(fname,regexpr("r[0-9]i[0-9]p[0-9]f[0-9]",fname)[1],
                        regexpr("r[0-9]i[0-9]p[0-9]f[0-9]",fname)[1]+5)
      experiment <- toupper(substr(fname,regexpr("ssp",fname)[1],
                                   regexpr("ssp",fname)[1]+4))
      N <- nchar(experiment)
      experiment <- paste(substr(experiment,1,N-1),substr(experiment,N,N),sep=".")
      experiment_id <- paste("historical",substr(fname,regexpr("ssp",fname)[1],
                                                 regexpr("ssp",fname)[1]+4),sep="+")
      frequency <- "mon"
      longname <- switch(var,"tas"="Near-Surface Air Temperature",
                         "pr"="Precipitation")
      qf <- c(qf, paste("Information about model missing from netCDF header.",
                        "Metadata recovered from history attribute."))
    } 

    ## Check and correct ripf - some simulations have the wrong ripf attached.
    if (is.na(gcm.ripf)) qf <- c(qf,"Missing experiment_ripf in netCDF header.")
    gcm.ripf2 <- paste("r", realization_index,
                      "i", initialization_index,
                      "p", physics_index,
                      "f", forcing_index,
                      sep="")
    if(grepl("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0-9]{1,2}",gcm.ripf2)) {
      if (is.na(gcm.ripf) | gcm.ripf!=gcm.ripf2) {
        gcm.ripf <- gcm.ripf2
        qf <- c(qf,paste("Discrepancy in experiment_ripf in netCDF header.",
                         "Replaced experiment_ripf with realization_index, intitialization_index, physics_index, forcing_index."))
      }
    }
    
    ## An extra check of the experiment_ripf:
    h <- strsplit(xx$model$history," ")
    fname <- unlist(h)[grep("ssp.*.nc",unlist(h))[1]]
    gcm.ripf3 <- substr(fname,regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0-9]{1,2}",fname)[1],
                       regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0-9]{1,2}",fname)[1]+5)
    if(grepl("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0-9]{1,2}",gcm.ripf3)) {
      if(gcm.ripf!=gcm.ripf3)  {
        qf <- c(qf,"Discrepancy between experiment_ripf and model history in netCDF header.")
      }
    }
    if(!is.na(filename)) filename <- gsub(".*/","",filename)
    mx <- data.frame(mip_era=mip_era, url=url, filename=filename,
                     dim=paste(dim,collapse=","), dates=dates, var=paste(var,collapse=","),
                     longname=paste(longname,collapse=","), unit=paste(vunit,collapse=","),
                     resolution=res, lon=lon.rng, lon_unit=lon.unit, 
                     lat=lat.rng, lat_unit=lat.unit,
                     experiment=experiment, 
                     experiment_id=experiment_id, 
                     frequency=frequency, 
                     creation_date=creation_date, 
                     gcm=gcm, gcm_ripf=gcm.ripf, qf=paste(qf,collapse="; "))
    meta <- names(mx)
    m <- length(meta)
    if (i==1) {
      X <- matrix(rep("NA",n*m),n,m) ## set up a matrix
      colnames(X) <- meta
      rownames(X) <- gcms
    }
    for (ii in 1:m) {
      if(!is.na(mx[[meta[ii]]])) {
        y <- as.character(mx[[meta[ii]]])
        X[i,ii] <- y
      }
    }
  }
  return(X)
}
