metaextract.cmip5 <- function(x=NULL, experiment="rcp45", verbose=FALSE) {
  if(verbose) print("metaextract.cmip")
  ## argument 'x' is input from getGCMs, getRCMs, testGCM, etc
  if (is.null(x)) x <- getGCMs(experiment=experiment,verbose=verbose)
  
  if(!inherits(x,"list")) x <- list(gcm.1=x)
  gcms <- names(x)
  n <- length(gcms)
  
  for (i in 1:n) {
    xx <- x[[gcms[i]]]
    project_id <- NA; url <- NA; filename <- NA; dim <- NA; dates <- NA
    var <- NA; longname <- NA; vunit <- NA; vid <- NA
    res <- NA; lon.rng <- NA; lon.unit <- NA; lat.rng <- NA; lat.unit <- NA
    experiment <- NA; #experiment_id <- NA; 
    frequency <- NA; creation_date <- NA; tracking_id <- NA
    gcm <- NA; gcm.v <- NA; gcm.realm <- NA
    gcm.rip <- NA; realization <- NA; initialization_method <- NA; physics_version <- NA
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
                "project_id","experiment","experiment_id",
                "creation_date","tracking_id",
                "realization","initialization_method","physics_version")) {
      if(!is.null(xx[[mi]])) {
        eval(parse(text=paste(mi," <- xx$",mi,sep="")))
      } else if (!is.null(xx$model[[mi]])) {
        eval(parse(text=paste(mi," <- xx$model$",mi,sep="")))
      }
    }
    if(!is.null(xx$model$model_id)) gcm <- xx$model$model_id
    if(!is.null(xx$model$parent_experiment_rip)) {
      gcm.rip <- xx$model$parent_experiment_rip
    }
    if(!is.null(xx$model$version_number)) gcm.v <- xx$model$version_number
    if(!is.null(xx$model$modeling_realm)) gcm.realm <- xx$model$modeling_realm
    
    ## If file is missing information in netCDF header,
    ## use the model history to obtain it. 
    if(is.null(xx$model$model_id) & xx$project_id=="CMIP5") {
      h <- xx$model$history
      creation_date <- substr(h, 1, regexpr("[0-9]{4}",h)+3)
      h <- strsplit(h," ")
      fname <- unlist(h)[grep("rcp.*.nc",unlist(h))[1]]
      gcm <- gsub("_rcp.*","",gsub(".*_Amon_","",fname))
      gcm.rip <- substr(fname,regexpr("r[0-9]i[0-9]p[0-9]",fname)[1],
                        regexpr("r[0-9]i[0-9]p[0-9]",fname)[1]+5)
      experiment <- toupper(substr(fname,regexpr("rcp",fname)[1],
                                   regexpr("rcp",fname)[1]+4))
      N <- nchar(experiment)
      experiment <- paste(substr(experiment,1,N-1),substr(experiment,N,N),sep=".")
      experiment_id <- paste("historical",substr(fname,regexpr("rcp",fname)[1],
                                                 regexpr("rcp",fname)[1]+4),sep="+")
      frequency <- "mon"
      longname <- switch(var,"tas"="Near-Surface Air Temperature",
                         "pr"="Precipitation")
      qf <- c(qf, paste("Information about model missing from netCDF header.",
                        "Metadata recovered from history attribute."))
    } 

    ## Check and correct rip - some simulations have the wrong rip attached.
    if (is.na(gcm.rip)) qf <- c(qf,"Missing experiment_rip in netCDF header.")
    gcm.rip2 <- paste("r", realization,
                      "i", initialization_method,
                      "p", physics_version, sep="")
    if(grepl("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",gcm.rip2)) {
      if (is.na(gcm.rip) | gcm.rip!=gcm.rip2) {
        gcm.rip <- gcm.rip2
        qf <- c(qf,paste("Discrepancy in experiment_rip in netCDF header.",
                         "Replaced experiment_rip with realization, intitialization_method, physics_version."))
      }
    }
    
    ## An extra check of the experiment_rip:
    h <- strsplit(xx$model$history," ")
    fname <- unlist(h)[grep("rcp.*.nc",unlist(h))[1]]
    gcm.rip3 <- substr(fname,regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",fname)[1],
                       regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",fname)[1]+5)
    if(grepl("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",gcm.rip3)) {
      if(gcm.rip!=gcm.rip3)  {
        qf <- c(qf,"Discrepancy between experiment_rip and model history in netCDF header.")
      }
    }
    if(!is.na(filename)) filename <- gsub(".*/","",filename)
    mx <- data.frame(project_id=project_id, url=url, filename=filename,
                     dim=paste(dim,collapse=","), dates=dates, var=paste(var,collapse=","),
                     longname=paste(longname,collapse=","), unit=paste(vunit,collapse=","),
                     resolution=res, lon=lon.rng, lon_unit=lon.unit, 
                     lat=lat.rng, lat_unit=lat.unit,
                     experiment=experiment, 
                     experiment_id=experiment_id, 
                     frequency=frequency, 
                     creation_date=creation_date, 
                     gcm=gcm, gcm_rip=gcm.rip, qf=paste(qf,collapse="; "))
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
