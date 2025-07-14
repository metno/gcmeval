## Calculate the mean annual cycle and spatial correlation for CMIP models
calculate.statistics.cmip5 <- function(reference="eraint", period=c(1981,2010),
                                       variable="tas", 
                                       experiment="rcp45", nfiles=5,
                                       path.gcm=NULL, add=TRUE, force=FALSE, 
                                       path=NULL, mask="coords.txt", 
                                       path.ref=NULL, files.ref=NULL,
                                       store.file="statistics.rda",
                                       stats=c("mean","spatial.sd","corr","rmse"),
                                       verbose=FALSE) {
  if(verbose) print("calculate.statistics.cmip")
  shape <- get.shapefile("referenceRegions.shp")
  srex.regions <- as.character(shape$LAB)
  if(min(period)>2010) reference <- NULL
  
  if(verbose) print("Find GCM files and check unit")
  urls <- cmip5.urls(varid=variable,experiment=experiment)
  ngcm <- length(urls)
  gcm.file <- file.path(path.gcm, paste0('GCM1.',variable,'.',experiment,'.nc'))
  if(!file.exists(gcm.file)) Y <- getCM(url=urls[1], destfile=gcm.file)
  nc <- ncdf4::nc_open(gcm.file)
  units <- nc$var[[length(nc$var)]]$units
  ncdf4::nc_close(nc)
  
  if(verbose) print("Prepare reference data")
  if(is.null(reference)) {
    stats <- stats[!stats %in% c("corr","rmse")]
  } else {
    ref.file <- getReference(reference, variable, path=path.ref,
                             filenames=files.ref)
    if(!is.character(ref.file)) {
      reference <- NULL
      print("Warning! Reference file not found. Continuing without reference data.")
      stats <- stats[!stats %in% c("corr","rmse")]
    } else {
      if(verbose) print("Check unit of reference data agains unit of GCM data")
      nc <- ncdf4::nc_open(file.path(ref.file))
      ref.unit <- nc$var[[length(nc$var)]]$units
      ncdf4::nc_close(nc)
      if(units!=ref.unit) {
        if(variable=="pr") {
          c.ref <- 1
          if(grepl("/s|s-1",units) & !grepl("/s|s-1",ref.unit)) {
            c.ref <- c.ref/(60*60*24)
          } else if(!grepl("/s|s-1",units) & grepl("/s|s-1",ref.unit)) {
            c.ref <- c.ref*(60*60*24)
          }
          if(grepl("mm|kg m-2",units) & !grepl("mm|kg m-2",ref.unit)) {
            c.ref <- c.ref*1E3
          } else if(!grepl("mm|kg m-2",units) & grepl("mm|kg m-2",ref.unit)) {
            c.ref <- c.ref*1E-3
          }
          cdo.command("mulc", c.ref, ref.file, "ref.nc")
        } else if(variable=="tas") {
          c.ref <- 0
          if(grepl(units,"K") & grepl(ref.unit,"C")) {
            c.ref <- 273.15
          } else if(grepl(units,"C") & grepl(ref.unit,"K")) {
            c.ref <- -273.15
          }
          cdo.command("add", c.ref, ref.file, "ref.nc")
        }
        ref.file <- "ref.nc"
      }
      res.ref <- resolution(ref.file, dim=c("lat","latitude"))
      if(res.ref<0) {
        if(verbose) print("Correct inverted latitudes of ref data")
        cdo.command("invertlat","",ref.file,"tmp.nc")
        cdo.command("copy","","tmp.nc","ref.nc")
        file.remove("tmp.nc")
        ref.file <- "ref.nc"
      }
    }
  }
  
  if(!is.null(reference) && "corr" %in% stats) ref.corr <- ref.file
  
  label.period <- paste0("period.",paste(period, collapse="_"))
  if(is.null(path)) path <- getwd()
  store.file <- file.path(path,store.file)
  
  if(file.exists(store.file) & add) {
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
  if("rmse" %in% stats) {
    if(verbose) print("Prepare reference data for rmse calculations")
    ref.mon.file <- paste(reference,"monmean",variable,"nc",sep=".")
    if(!is.character(find.file(ref.mon.file)[1])) ref.mon.file <- file.path(path,ref.mon.file)
    if(!file.exists(ref.mon.file)) {
      cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                  ref.file, ref.mon.file)
    }
    ref <- retrieve(ref.mon.file)
    lon <- attr(ref,"longitude")
    lat <- attr(ref,"latitude")
    weights <- calculate.mon.weights(lon,lat)
  }
  
  if(!is.null(reference) & any(c("spatial.sd","mean") %in% stats)) {
    if(verbose) print("Global")
    if(verbose) print("Calculate basic statistics for reference data")
    if("spatial.sd" %in% stats & 
       (!"spatial.sd" %in% names(X[[reference]]$global) | force)) {
      if(verbose) print("spatial st dev")
      X[[reference]]$global$spatial.sd <- c(cdo.spatSd(ref.file,period),
                                            cdo.spatSd(ref.file,period,monthly=TRUE))
    }
    if("mean" %in% stats & 
       (!"mean" %in% names(X[[reference]]$global) | force)) {
      if(verbose) print("mean value")
      X[[reference]]$global$mean <- c(cdo.mean(ref.file,period),
                                      cdo.mean(ref.file,period,monthly=TRUE))
    }

    for(i in 1:length(srex.regions)) {
      if(verbose) print(paste0("Region ",i,": ",srex.regions[i]))
      getPolCoords(i,shape=shape,destfile=mask)
      if("spatial.sd" %in% stats & 
         (!"spatial.sd" %in% names(X[[reference]][[srex.regions[i]]]) | force)) {
        if(verbose) print("spatial st dev")
        X[[reference]][[srex.regions[i]]]$spatial.sd <-
          c(cdo.spatSd(ref.file,period,mask=mask), 
            cdo.spatSd(ref.file,period,mask=mask,monthly=TRUE))
      }
      if("mean" %in% stats & 
         (!"mean" %in% names(X[[reference]][[srex.regions[i]]]) | force)) {
        if(verbose) print("mean value")
        X[[reference]][[srex.regions[i]]]$mean <-
          c(cdo.mean(ref.file,period,mask=mask), 
            cdo.mean(ref.file,period,mask=mask,monthly=TRUE))
      }
    }

    attr(X[[reference]],"variable") <- variable
    attr(X[[reference]],"unit") <- units
    statistics[[variable]][[experiment]][[label.period]][[reference]] <- X[[reference]]
  }
  
  start <- 1
  if(file.exists(store.file) & add & !force) {
    gcms <- paste("gcm",seq_along(urls),sep=".")
    ok <- gcms %in% names(X)
    ok[ok] <- unlist(sapply(gcms[ok], function(gcm) {
      sum(stats %in% names(X[[gcm]]$global))==length(stats)}))
    if(any(ok) & "rmse" %in% stats) {
      ok[ok] <- unlist(sapply(gcms[ok], function(gcm) {
        reference %in% names(X[[gcm]]$global$rmse)}))
    }
    if(any(ok) & "corr" %in% stats) {
      ok[ok] <- unlist(sapply(gcms[ok], function(gcm) {
        reference %in% names(X[[gcm]]$global$corr)}))
    }
    nok <- try(!ok)
    if(inherits(nok,"try-error")) browser()
    if(any(!ok)) {
      start <- as.numeric(head(sub('.*\\.', '', gcms[!ok], perl=TRUE),n=1))
    } else {
      start <- min(ngcm, as.numeric(tail(sub('.*\\.', '', gcms, perl=TRUE),n=1))+1)
    }
  }
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start+nfiles-1,ngcm) 
  }
  for(i in start:end) {
    if(verbose) print(paste0("GCM ",i))
    store.name <- paste("gcm",i,sep=".")
    gcm.file <- file.path(path.gcm,
      paste0('GCM',i,'.',variable,'.',experiment,'.nc'))
    if(!file.exists(gcm.file)) {
      Y <- getCM(url=urls[i], destfile=gcm.file)
      gcm.file <- Y$filename
    }
    if(verbose) print("Global")
    if(verbose) print("Calculate basic statistics")
    if("spatial.sd" %in% stats & 
       (!"spatial.sd" %in% names(X[[store.name]]$global) | force)) {
      if(verbose) print("spatial st dev")
      X[[store.name]]$global$spatial.sd <- c(cdo.spatSd(gcm.file,period),
                                             cdo.spatSd(gcm.file,period,monthly=TRUE))
    }
    if("mean" %in% stats & 
       (!"mean" %in% names(X[[store.name]]$global) | force)) {
      if(verbose) print("mean value")
        X[[store.name]]$global$mean <- c(cdo.mean(gcm.file,period),
                                         cdo.mean(gcm.file,period,monthly=TRUE))
    }
    if("corr" %in% stats) {
      if(!reference %in% names(X[[store.name]]$global$corr) | force) {
        if(verbose) print("spatial correlation")
        res.gcm <- resolution(gcm.file, dim=c("lat","latitude"))
        res.ref <- resolution(ref.corr, dim=c("lat","latitude"))
        if(res.ref!=res.gcm) {
          ref.new <- gsub(".nc",paste0("_",sprintf("%.2f",res.gcm),"deg.nc"),ref.corr)
          if(file.exists(ref.new)) {
            ref.corr <- ref.new
          } else {
            if(verbose) print("Regrid reference data to GCM grid")
            cdo.regrid(ref.corr,ref.new,res.lon=res.gcm,res.lat=res.gcm,
                       remap="remapcon",verbose=verbose)
            ref.corr <- ref.new
          }
        }
      }  
      X[[store.name]]$global$corr[[reference]] <- c(cdo.gridcor(gcm.file,ref.corr,period),
          cdo.gridcor(gcm.file,ref.corr,period,monthly=TRUE))
    }
    if("rmse" %in% stats) { 
      if(!reference %in% names(X[[store.name]]$global$rmse) | force) {
        if(verbose) print("rmse")
        gcm.mon.file <- file.path(path,"gcm.monmean.nc")
        cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                    gcm.file, gcm.mon.file)
        gcm <- zoo::coredata(retrieve(gcm.mon.file))
        dim(gcm) <- dim(ref) <- c(12,length(attr(ref,"longitude")),length(attr(ref,"latitude")))
        X[[store.name]]$global$rmse[[reference]] <- sqrt(sum(weights*(gcm-ref)^2)/sum(weights))
        file.remove(gcm.mon.file)
      }
    }
    for(j in 1:length(srex.regions)) {
      if(verbose) print(paste0("Region ",j,": ",srex.regions[j]))
      getPolCoords(j,shape=shape,destfile=mask)
      if(verbose) print("Calculate basic statistics")
      if("spatial.sd" %in% stats & 
         (!"spatial.sd" %in% names(X[[store.name]][[srex.regions[j]]]) | force)) {
        if(verbose) print("spatial st dev")
        X[[store.name]][[srex.regions[j]]]$spatial.sd <- 
          c(cdo.spatSd(gcm.file,period,mask=mask), 
            cdo.spatSd(gcm.file,period,mask=mask,monthly=TRUE))
      }
      if("mean" %in% stats & 
         (!"mean" %in% names(X[[store.name]][[srex.regions[j]]]) | force)) {
        if(verbose) print("mean value")
        X[[store.name]][[srex.regions[j]]]$mean <- 
          c(cdo.mean(gcm.file,period,mask=mask), 
            cdo.mean(gcm.file,period,mask=mask,monthly=TRUE))
      }
      if("corr" %in% stats) { 
        if(!reference %in% names(X[[store.name]][[srex.regions[j]]]$corr) | force) {
          if(verbose) print("spatial correlation")
          X[[store.name]][[srex.regions[j]]]$corr[[reference]] <- 
            c(cdo.gridcor(gcm.file,ref.corr,period,mask=mask), 
              cdo.gridcor(gcm.file,ref.corr,period,mask=mask,monthly=TRUE))
        }   
      }
      if("rmse" %in% stats) { 
        if(!reference %in% names(X[[store.name]][[srex.regions[j]]]$rmse) | force) {
          if(verbose) print("rmse")
          mask.j <- gen.mask.srex(destfile=gcm.file,mask.polygon=shape[j,])
          dim(gcm) <- dim(ref) <- c(12,length(attr(ref,"longitude"))*length(attr(ref,"latitude")))
          gcm.masked <- mask.zoo(gcm,mask.j)
          ref.masked <- mask.zoo(ref,mask.j)
          dim(gcm.masked) <- dim(ref.masked) <- 
            c(12,length(attr(ref,"longitude")),length(attr(ref,"latitude")))
          X[[store.name]][[srex.regions[j]]]$rmse[[reference]] <- 
            sqrt(sum(weights*(gcm.masked-ref.masked)^2,na.rm=TRUE)/
                 sum(weights[!is.na(gcm.masked)]))
        }
      }
    }
    if (variable=="tas") {
      if(max(abs(X[[store.name]]$global$mean),na.rm=TRUE)>273) {
        gcm.unit <- "K"
      } else {
        gcm.unit <- "degrees~Celsius"
      }
    } else if(variable=="pr") {
      if(!is.null(gcm.file)) {
        nc <- ncdf4::nc_open(gcm.file)
        gcm.unit <- nc$var[[1]]$units
        ncdf4::nc_close(nc)
      }
    }
    attr(X[[store.name]],"filename") <- basename(gcm.file)
    attr(X[[store.name]],"variable") <- variable
    attr(X[[store.name]],"unit") <- gcm.unit
    statistics[[variable]][[experiment]][[label.period]][[store.name]] <- X[[store.name]]
    save(file=store.file,statistics)
  }
  if("rmse" %in% stats) {
    if(verbose) print("Calculate CMPI for all regions and GCMs")
    X <- statistics[[variable]][[experiment]][[label.period]]
    median.rmse <- list()
    for(region in names(X[[1]])) median.rmse[[region]] <- 
      median(unlist(lapply(X[grep("gcm",names(X))], function(x) x[[region]]$rmse[[reference]])))
    gcms <- names(X)[grepl("gcm",names(X))]
    for(store.name in gcms) {
      for(region in names(X[[1]])) {
        X[[store.name]][[region]]$cmpi[[reference]] <- 
          (X[[store.name]][[region]]$rmse[[reference]] -
             median.rmse[[region]])/median.rmse[[region]]
      }
    }
    statistics[[variable]][[experiment]][[label.period]] <- X
    save(file=store.file,statistics)
  }
  if(verbose) print("Done!")
  invisible(statistics)
}

