## Calculate the root mean square error (rms) and relative rms (e)
calculate.rmse.cmip <- function(reference="era", period=c(1981,2010), 
                                variable="tas", experiment="rcp45", 
                                nfiles=4, continue=TRUE, path=NULL, 
                                store.file="statistics.rda", path.gcm=NULL, verbose=FALSE) {
  if(verbose) print("calculate.rmse.cmip")
  shape <-  get.shapefile("referenceRegions.shp")
  srex.regions <- as.character(shape$LAB)
  
  label.period <- paste(period, collapse="-")
  if(!is.null(path)) store.file <- file.path(path,store.file)
  if(file.exists(store.file) && !force) {
    load(store.file)
    if(length(statistics[[variable]][[experiment]][[label.period]])>0) {
      X <- statistics[[variable]][[experiment]][[label.period]]
    } else {
      X <- list()
    }
  } else {
    X <- list()
    statistics <- list()
  }
  
  ## Pre-process reference file if necessary
  ref.file <- getReference(reference,variable)
  if(is.null(path)) {
    path <- dirname(ref.file)
  } else if (!dir.exists(path)) {
    path <- getwd()
  }
  if(is.null(path.gcm)) path.gcm <- path
  
  ref.mulc <- paste(reference,"mulc",variable,"nc",sep=".")
  if(!is.character(find.file(ref.mulc)[1])) ref.mulc <- file.path(path,ref.mulc)
  ref.mon.file <- paste(reference,"monmean",variable,"nc",sep=".")
  if(!is.character(find.file(ref.mon.file)[1])) ref.mon.file <- file.path(path,ref.mon.file)
  
  if(variable=="pr") {
    if(!file.exists(ref.mulc)) cdo.command("mulc",1000,ref.file,ref.mulc)
  } else {
    if(!file.exists(ref.mulc)) ref.mulc <- ref.file
  }
  
  if(!file.exists(ref.mon.file)) {
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                ref.mulc, ref.mon.file)
  }
  ref <- zoo::coredata(esd::retrieve.default(ref.mon.file))
  
  ## Calculate weights only once
  lon <- attr(ref,"longitude")
  lat <- attr(ref,"latitude")
  weights <- calculate.mon.weights(lon,lat)
  
  ## Check which files are processed
  ngcm <- length(cmip5.urls(varid=variable, experiment=experiment))
  start <- 1
  if(continue && file.exists(store.file)) {
    gcmnames <- names(store)[grep("gcm",names(store))]
    ok <- sapply(gcmnames, function(gcm) "rms" %in% names(store[[gcm]][[length(store[[gcm]])]]))
    if(any(!ok)) {
      start <- as.numeric(head(gsub('.*\\.', '', gcmnames[!ok], perl=TRUE),n=1))
    } else {
      start <- min(ngcm, as.numeric(tail(sub('.*\\.', '', gcmnames, perl=TRUE),n=1)))
    }
  }
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start + nfiles - 1, ngcm) 
  }
  start <- min(start,end)
  for(i in start:end) {
    store.name <- paste("gcm",i,sep=".")
    gcm.file <- file.path(path.gcm,paste("GCM",i,".",variable,".",experiment,".nc",sep=""))
    if(!file.exists(gcm.file)) getGCMs(i, varid=variable, destfile=file.path(path.gcm,gcm.file))
    gcm.mon.file <- file.path(path,"gcm.monmean.nc")
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                gcm.file, gcm.mon.file)
    gcm <- zoo::coredata(esd::retrieve(gcm.mon.file))
    dim(gcm) <- dim(ref) <- c(12,length(attr(gcm,"longitude")),length(attr(gcm,"latitude")))
    X[[store.name]]$global$rms <- sqrt(sum(weights*(gcm-ref)^2)/sum(weights))
    for(region in srex.regions) {
      polygon <- shape[which(srex.regions==region),]
      mask <- gen.mask.srex(destfile=gcm.file,mask.polygon=polygon)
      dim(gcm) <- dim(ref) <- c(12,length(attr(ref,"longitude"))*length(attr(ref,"latitude")))
      gcm.masked <- mask.zoo(gcm,mask)
      ref.masked <- mask.zoo(ref,mask)
      dim(gcm.masked) <- dim(ref.masked) <- 
        c(12,length(attr(gcm,"longitude")),length(attr(gcm,"latitude")))
      X[[store.name]][[region]]$rms <- 
        sqrt(sum(weights*(gcm.masked-ref.masked)^2,na.rm=TRUE)/
               sum(weights[!is.na(gcm.masked)]))
    }
    file.remove(gcm.mon.file)
  }
  
  median.rms <- list()
  for(region in names(X[[1]])) median.rms[[region]] <- median(unlist(lapply(X, function(x) x[[region]]$rms)))
  for(i in start:end){
    store.name <- paste("gcm",i,sep=".")
    for(region in names(store[[1]])){
      X[[store.name]][[region]]$e <- 
        (X[[store.name]][[region]]$rms-median.rms[[region]])/median.rms[[region]]
    }
  }
  file.remove(ref.mon.file)
  browser()
  statistics[[variable]][[experiment]][[label.period]] <- X
  save(file=store.file,statistics)
  invisible(statistics)
}
