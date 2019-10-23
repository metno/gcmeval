metaextract.cmip <- function(x, verbose=FALSE) {
  if(verbose) print("metaextract.cmip")
  ## argument 'x' is input from getCM, getGCMs, getRCMs etc
  
  mip_era <- NA; filename <- NA; dim <- NA; dates <- NA
  var <- NA; longname <- NA; vunit <- NA; vid <- NA
  res <- NA; lon.rng <- NA; lon.unit <- NA; lat.rng <- NA; lat.unit <- NA
  experiment <- NA; #experiment_id <- NA; 
  frequency <- NA; creation_date <- NA; tracking_id <- NA
  gcm <- NA; gcm.v <- NA; gcm.realm <- NA
  gcm.rip <- NA; realization_index <- NA; initialization_index <- NA; physics_index <- NA; forcing_index <- NA;
  qf <- NULL
  if(!is.null(x$dim)) dim <- paste(names(x$dim),collapse=",")
  if(!is.null(names(x$var))) {
    var <- names(x$var)
    var <- var[!grepl("time|lon|lat",var)]
    if(!is.null(x$var[[1]]$longname)) longname <- sapply(var, function(v) x$var[[v]]$longname)
    if(!is.null(x$var[[1]]$units)) vunit <- sapply(var, function(v) x$var[[v]]$units)
    if(!is.null(x$var[[1]]$id$id)) vid <- sapply(var, function(v) x$var[[v]]$id$id)
  }
  if(!is.null(names(x$dim))) {
    if(!is.null(x$dim$lat$vals)) {
      res <- diff(x$dim$lat$vals)[1]
      lat.rng <- paste(range(x$dim$lat$vals),collapse=",")
    }
    if(!is.null(x$dim$lon$vals)) lon.rng <- paste(range(x$dim$lon$vals),collapse=",")
    if(!is.null(x$dim$lat$units)) lat.unit <- x$dim$lat$units
    if(!is.null(x$dim$lon$units)) lon.unit <- x$dim$lon$units
  }

  for(mi in c("filename","dates","frequency",
              "mip_era","project_id","experiment","experiment_id",
              "creation_date","tracking_id",
              "realization_index","initialization_index","physics_index","forcing_index")) {
    if(!is.null(x[[mi]])) {
      eval(parse(text=paste(mi," <- x$",mi,sep="")))
    } else if (!is.null(x$model[[mi]])) {
      eval(parse(text=paste(mi," <- x$model$",mi,sep="")))
    }
  }
  
  if(frequency=="month") frequency <- "mon"
  
  if(!is.null(x$model$model_id)) gcm <- x$model$model_id
  if(any(grep("parent_experiment_rip",names(x$model)))) {
    nm <- names(x$model)[grep("parent_experiment_rip",names(x$model))[1]]
    gcm.rip <- x$model[[nm]]
  } else if(any(grep("variant",names(x$model)))) {
    nm <- names(x$model)[grep("variant",names(x$model))[1]]
    gcm.rip <- x$model[[nm]]
  }
  ## Version number:
  if(any(grepl("version",names(x$model)))) {
    iv <- which(grepl("version",names(x$model)) & !grepl("physics_version",names(x$model)))
    gcm.v <- paste(paste(names(x$model)[iv],paste(x$model[iv])),collapse=", ")
  }
  ## Modeling realm:
  if(any(grepl("realm",names(x$model)))) {
    gcm.realm <- x$model[grep("realm",names(x$model))[1]]
  }
  #if(!is.null(x$model$modeling_realm)) gcm.realm <- x$model$modeling_realm
  
  ## If file is missing information in netCDF header,
  ## use the model history to obtain it. 
  if(is.null(x$model$model_id)) {
    h <- x$model$history
    creation_date <- substr(h, 1, regexpr("[0-9]{4}",h)+3)
    h <- strsplit(h," ")
    fname <- unlist(h)[grep("Amon.*.nc",unlist(h))[1]]
    gcm <- gsub("_.*","",gsub(".*_Amon_","",fname))
    gcm.rip <- unlist(strsplit(substr(fname,regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",fname)[1],
                      nchar(fname)),split="_"))[1]
    if(x$project_id=="CMIP6" & grep("ssp",fname)) {
      experiment <- unlist(strsplit(substr(fname,regexpr("ssp",fname)[1],nchar(fname)),split="_"))[1]
    } else if(x$project_id=="CMIP5" & grep("rcp",fname)) {
      experiment <- unlist(strsplit(substr(fname,regexpr("rcp",fname)[1],nchar(fname)),split="_"))[1]
      N <- nchar(experiment)
      experiment <- paste(substr(experiment,1,N-1),substr(experiment,N,N),sep=".")
    } else if(min(x$dates)<as.Date("2010-01-01")) {
      experiment <- "historical"
    }
    if(min(x$dates)<as.Date("2010-01-01") & !grepl("historical",experiment)) {
      experiment_id <- paste("historical",experiment,sep="+")
    } else {
      experiment_id <- experiment
    }
    frequency <- "mon"
    longname <- switch(var,"tas"="Near-Surface Air Temperature",
                       "pr"="Precipitation")
    qf <- c(qf, paste("Information about model missing from netCDF header.",
                      "Metadata recovered from history attribute."))
  } 

  ## Check and correct rip - some simulations have the wrong rip attached.
  if (is.na(gcm.rip)) qf <- c(qf,"Missing experiment_rip in netCDF header.")
  if(!is.na(realization_index) & !is.na(initialization_index) & !is.na(physics_index)) {
    gcm.rip2 <- paste0("r", realization_index,
                       "i", initialization_index,
                       "p", physics_index)
    if(!is.na(forcing_index)) gcm.rip2 <- paste0(gcm.rip2,"f",forcing_index) 
    if (is.na(gcm.rip) | gcm.rip!=gcm.rip2) {
      gcm.rip <- gcm.rip2
      qf <- c(qf,paste("Discrepancy in experiment_rip in netCDF header.",
                       "Replaced experiment_ripf with realization_index, intitialization_index, physics_index, forcing_index",
                       "(and forcing_index for CMIP6 files)."))
    }
  }
  
  ## An extra check of the experiment_ripf:
  h <- strsplit(x$model$history," ")
  fname <- unlist(h)[grep("ssp.*.nc",unlist(h))[1]]
  if(grepl("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",fname)) {
    gcm.rip3 <- unlist(strsplit(substr(fname,regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",fname)[1],
                       nchar(fname)),split="_"))[1]
    if(gcm.rip!=gcm.rip3)  {
      qf <- c(qf,"Discrepancy between experiment_rip and model history in netCDF header.")
    }
  }
  if(!is.na(filename)) filename <- gsub(".*/","",filename)
  mx <- data.frame(project_id=project_id, filename=filename, 
                   dim=paste(dim,collapse=","), dates=dates, var=paste(var,collapse=","),
                   longname=paste(longname,collapse=","), unit=paste(vunit,collapse=","),
                   resolution=res, lon=lon.rng, lon_unit=lon.unit, 
                   lat=lat.rng, lat_unit=lat.unit,
                   experiment=experiment, 
                   experiment_id=experiment_id, 
                   frequency=frequency, 
                   creation_date=creation_date, 
                   gcm=gcm, gcm_rip=gcm.rip, qf=paste(qf,collapse="; "))
  X <- matrix(sapply(mx,as.character), nrow=1, ncol=length(mx))
  colnames(X) <- colnames(mx)
  return(X)
}
