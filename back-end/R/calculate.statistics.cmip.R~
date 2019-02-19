## Functions to calculate basic statistics

## Calculate the mean annual cycle and spatial correlation for CMIP models
calculate.statistics.cmip <- function(reference="era", period=c(1981,2010), variable="tas", 
                                      experiment="rcp45", nfiles=5,  
                                      path.gcm=NULL, continue=TRUE, 
                                      mask="coords.txt", verbose=FALSE) {
  if(verbose) print("calculate.statistics.cmip")
  shape <- get.shapefile("referenceRegions.shp")
  srex.regions <- as.character(shape$LAB)
  if(max(period)>2015) reference <- NULL
  if(!is.null(reference)) {
    ref.file <- getReference(reference,variable)
    if(!is.character(ref.file)) {
      reference <- NULL
      print("Warning! Reference file not found. Continuing without reference data.")
    }
  }
  if(!is.null(reference)) {
    store.file <- paste("statistics.cmip", reference, variable, paste(period, collapse="-"),
                        experiment, "rda", sep=".")
  } else {
    store.file <- paste("statistics.cmip", variable, paste(period, collapse="-"), 
                        experiment, "rda", sep=".")
  }
  store <- list()
  if(file.exists(store.file) && !force) load(store.file)
  units <- NULL
  if(!is.null(reference)) {
    #reference.raster <- raster::raster(ref.file)
    store.name <- paste(reference,variable,sep=".")
    store[[store.name]]$global$spatial.sd <- c(cdo.spatSd(ref.file,period), 
                                               cdo.spatSd(ref.file,period,monthly=TRUE))
    store[[store.name]]$global$mean <- c(cdo.mean(ref.file,period), 
                                         cdo.mean(ref.file,period,monthly=TRUE))
    if (variable=="tas") {
      if(max(abs(store[[store.name]]$global$mean),na.rm=TRUE)>273) {
        units <- "K"
      } else {
        units <- "degrees~Celsius"
      }
    } else if(variable=="pr") {
      if(!is.null(ref.file)) {
        ncid <- ncdf4::nc_open(ref.file)
        units <- ncid$var[[1]]$units
        ncdf4::nc_close(ncid)
      } else {
        if(max(abs(store[[store.name]]$mean),na.rm=TRUE)<0.001) {
          units <- "kg m-2 s-1"
        } else {
          units <- "mm/day"
        }
      }
    }
    
    for(i in 1:length(srex.regions)) {
      getPolCoords(i,shape=shape,destfile=mask)
      store[[ store.name ]][[ srex.regions[i] ]]$spatial.sd <- 
        c(cdo.spatSd(ref.file,period,mask=mask), 
          cdo.spatSd(ref.file,period,mask=mask,monthly=TRUE))
      store[[ store.name ]][[ srex.regions[i] ]]$mean <- 
        c(cdo.mean(ref.file,period,mask=mask), 
          cdo.mean(ref.file,period,mask=mask,monthly=TRUE))
    }
  }
  
  ngcm <- length(cmip5.urls(varid=variable,experiment=experiment))
  start <- 1
  if(continue && file.exists(store.file)) {
    ok <- sapply(names(store), function(gcm) sum(c("mean","spatial.sd") %in% names(store[[gcm]]$global))==2)
    if(any(!ok)) {
      start <- as.numeric(tail(sub('.*\\.', '', names(store)[!ok], perl=TRUE),n=1))+1
    } else {
      start <- min(ngcm, as.numeric(tail(sub('.*\\.', '', names(store), perl=TRUE),n=1))+1)
    }
  }
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start+nfiles-1,ngcm) 
  }
  for(i in start:end) {
    X <- getGCMs(select=i, varid=variable, path=path.gcm, experiment=experiment)
    gcm.file <- X[[1]]$filename
    store.name <- paste("gcm",i,sep=".")
    store[[store.name]]$global$spatial.sd <- c(cdo.spatSd(gcm.file,period),
                                               cdo.spatSd(gcm.file,period,monthly=TRUE))
    store[[store.name]]$global$mean <- c(cdo.mean(gcm.file,period),
                                         cdo.mean(gcm.file,period,monthly=TRUE))
    if(!is.null(reference)) {
      store[[store.name]]$global$corr <- 
        c(cdo.gridcor(gcm.file,ref.file,period),
          cdo.gridcor(gcm.file,ref.file,period,monthly=TRUE))
    }
    for(j in 1:length(srex.regions)) {
      getPolCoords(j,shape=shape,destfile=mask)
      store[[store.name]][[srex.regions[j]]]$spatial.sd <- 
        c(cdo.spatSd(gcm.file,period,mask=mask), 
          cdo.spatSd(gcm.file,period,mask=mask,monthly=TRUE))
      store[[store.name]][[srex.regions[j]]]$mean <- 
        c(cdo.mean(gcm.file,period,mask=mask), 
          cdo.mean(gcm.file,period,mask=mask,monthly=TRUE))
      if(!is.null(reference)) {
        store[[store.name]][[srex.regions[j]]]$corr <- 
          c(cdo.gridcor(gcm.file,ref.file,period,mask=mask), 
            cdo.gridcor(gcm.file,ref.file,period,mask=mask,monthly=TRUE))
      }
    }
    save(file=store.file,store)
    if (variable=="tas") {
      if(max(abs(store[[store.name]]$global$mean),na.rm=TRUE)>273) {
        units <- c(units,"K")
      } else {
        units <- c(units,"degrees~Celsius")
      }
    } else if(variable=="pr") {
      if(!is.null(gcm.file)) {
        ncid <- ncdf4::nc_open(gcm.file)
        units <- c(units,ncid$var[[1]]$units)
        ncdf4::nc_close(ncid)
      }
    }
    #if(i==ngcm) return(store)
  }
  attr(store,"variable") <- variable
  attr(store,"unit") <- units
  save(file=store.file,store)
  return(store)
}

