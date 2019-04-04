## Calculate the mean annual cycle and spatial correlation for CMIP models
calculate.statistics.cmip <- function(reference="eraint", period=c(1981,2010),
                                      variable="tas", 
                                      experiment="rcp45", nfiles=5,
                                      path.gcm=NULL, continue=TRUE, 
                                      path=NULL, mask="coords.txt", 
                                      store.file="statistics.rda",
                                      stats=c("mean","spatial.sd","corr","rmse"),
                                      verbose=FALSE) {
  if(verbose) print("calculate.statistics.cmip")
  shape <- get.shapefile("referenceRegions.shp")
  srex.regions <- as.character(shape$LAB)
  if(max(period)>2015) reference <- NULL
  
  # Check unit of GCM files
  gcm.files <- list.files(path=path.gcm, paste0('GCM[0-9]{1,3}.',variable,'.',experiment,'.nc'))
  nc <- ncdf4::nc_open(file.path(path.gcm,gcm.files[1]))
  units <- nc$var[[length(nc$var)]]$units
  
  if(!is.null(reference)) {
    ref.file <- getReference(reference,variable)
    if(!is.character(ref.file)) {
      reference <- NULL
      print("Warning! Reference file not found. Continuing without reference data.")
      stats <- stats[!stats %in% c("corr","rmse")]
    } else {
      ## Check unit of reference data to unit of GCM data
      ## and create a function f.ref to convert the ref unit:
      nc <- ncdf4::nc_open(file.path(ref.file))
      ref.unit <- nc$var[[length(nc$var)]]$units
      ncdf4::nc_close(nc)
      f.ref <- function(x) x
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
          f.ref <- function(x) x*c.ref
        } else if(variable=="tas") {
          if(grepl(units,"K") & grepl(ref.unit,"C")) {
            f.ref <- function(x) x+273.15
          } else if(grepl(units,"C") & grepl(ref.unit,"K")) {
            f.ref <- function(x) x-273.15
          }
        }
      }
    }
  } else {
    stats <- stats[!stats %in% c("corr","rmse")]
  }
  
  if(!is.null(reference) && "corr" %in% stats) ref.corr <- ref.file
  
  label.period <- paste0("period.",paste(period, collapse="_"))
  if(is.null(path)) path <- getwd()
  store.file <- file.path(path,store.file)
  
  if(file.exists(store.file) & continue) {
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
    ref.mulc <- paste(reference,"mulc",variable,"nc",sep=".")
    if(!is.character(find.file(ref.mulc)[1])) ref.mulc <- file.path(path,ref.mulc)
    ref.mon.file <- paste(reference,"monmean",variable,"nc",sep=".")
    if(!is.character(find.file(ref.mon.file)[1])) ref.mon.file <- file.path(path,ref.mon.file)
    if(variable=="pr") {
      nc <- ncdf4::nc_open(file.path(ref.file))
      ref.unit <- nc$var[[length(nc$var)]]$units
      ncdf4::nc_close(nc)
      if(ref.unit %in% c("m","m/day","m day-1","m/s","m s-1")) {
        cdo.command("mulc", 1000, ref.file, "tmp.nc")
        cdo.command("chunit", paste0(ref.unit,",",gsub("m","mm",ref.unit)), "tmp.nc", ref.mulc)
        system("rm tmp.nc")
      }
    } else {
      if(!file.exists(ref.mulc)) ref.mulc <- ref.file
    }
    if(!file.exists(ref.mon.file)) {
      cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                  ref.mulc, ref.mon.file)
    }
    ref <- esd::retrieve.default(ref.mon.file)
    lon <- attr(ref,"longitude")
    lat <- attr(ref,"latitude")
    weights <- calculate.mon.weights(lon,lat)
  }
  
  if(!is.null(reference) & any(c("spatial.sd","mean") %in% stats)) {
    if(verbose) print("Global")
    if(verbose) print("Calculate basic statistics for reference data")
    if("spatial.sd" %in% stats & !"spatial.sd" %in% names(X[[reference]]$global)) {
      if(verbose) print("spatial st dev")
      X[[reference]]$global$spatial.sd <- f.ref(c(cdo.spatSd(ref.file,period),
                                            cdo.spatSd(ref.file,period,monthly=TRUE)))
    }
    if("mean" %in% stats & !"mean" %in% names(X[[reference]]$global)) {
      if(verbose) print("mean value")
      X[[reference]]$global$mean <- f.ref(c(cdo.mean(ref.file,period),
                                      cdo.mean(ref.file,period,monthly=TRUE)))
    }

    for(i in 1:length(srex.regions)) {
      if(verbose) print(paste0("Region ",i,": ",srex.regions[i]))
      getPolCoords(i,shape=shape,destfile=mask)
      if("spatial.sd" %in% stats & !"spatial.sd" %in% names(X[[reference]][[srex.regions[i]]])) {
        if(verbose) print("spatial st dev")
        X[[reference]][[srex.regions[i]]]$spatial.sd <-
          f.ref(c(cdo.spatSd(ref.file,period,mask=mask), 
            cdo.spatSd(ref.file,period,mask=mask,monthly=TRUE)))
      }
      if("mean" %in% stats & !"mean" %in% names(X[[reference]][[srex.regions[i]]])) {
        if(verbose) print("mean value")
        X[[reference]][[srex.regions[i]]]$mean <-
          f.ref(c(cdo.mean(ref.file,period,mask=mask), 
            cdo.mean(ref.file,period,mask=mask,monthly=TRUE)))
      }
    }

    attr(X[[reference]],"variable") <- variable
    attr(X[[reference]],"unit") <- units
    statistics[[variable]][[experiment]][[label.period]][[reference]] <- X[[reference]]
  }
  
  urls <- cmip5.urls(varid=variable,experiment=experiment)
  ngcm <- length(urls)
  start <- 1
  if(continue && file.exists(store.file)) {
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
    if("spatial.sd" %in% stats & !"spatial.sd" %in% names(X[[store.name]]$global)) {
      if(verbose) print("spatial st dev")
      X[[store.name]]$global$spatial.sd <- c(cdo.spatSd(gcm.file,period),
                                             cdo.spatSd(gcm.file,period,monthly=TRUE))
    }
    if("mean" %in% stats & !"mean" %in% names(X[[store.name]]$global)) {
      if(verbose) print("mean value")
        X[[store.name]]$global$mean <- c(cdo.mean(gcm.file,period),
                                         cdo.mean(gcm.file,period,monthly=TRUE))
    }
    if("corr" %in% stats & !reference %in% names(X[[store.name]]$global$corr)) {
      if(verbose) print("spatial correlation")
      res.ref <- resolution(ref.corr)
      res.gcm <- resolution(gcm.file)
      if(res.ref!=res.gcm) {
        ref.new <- gsub(".nc",paste0("_",sprintf("%.2f",res.gcm),"deg.nc"),ref.corr)
        if(file.exists(ref.new)) {
          ref.corr <- ref.new
        } else {
          cdo.regrid(ref.corr,ref.new,res.lon=res.gcm,res.lat=res.gcm,
                     remap="remapcon",verbose=verbose)
          ref.corr <- ref.new
        }
      } 
      X[[store.name]]$global$corr[[reference]] <- c(cdo.gridcor(gcm.file,ref.corr,period),
          cdo.gridcor(gcm.file,ref.corr,period,monthly=TRUE))
    }
    if("rmse" %in% stats & !reference %in% names(X[[store.name]]$global$rmse) ) {
      if(verbose) print("rmse")
      gcm.mon.file <- file.path(path,"gcm.monmean.nc")
      cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                  gcm.file, gcm.mon.file)
      gcm <- zoo::coredata(esd::retrieve.default(gcm.mon.file))
      dim(gcm) <- dim(ref) <- c(12,length(attr(ref,"longitude")),length(attr(ref,"latitude")))
      X[[store.name]]$global$rmse[[reference]] <- sqrt(sum(weights*(gcm-ref)^2)/sum(weights))
      file.remove(gcm.mon.file)
    }
    for(j in 1:length(srex.regions)) {
      if(verbose) print(paste0("Region ",j,": ",srex.regions[j]))
      getPolCoords(j,shape=shape,destfile=mask)
      if(verbose) print("Calculate basic statistics")
      if("spatial.sd" %in% stats & !"spatial.sd" %in% names(X[[store.name]][[srex.regions[j]]])) {
        if(verbose) print("spatial st dev")
        X[[store.name]][[srex.regions[j]]]$spatial.sd <- 
          c(cdo.spatSd(gcm.file,period,mask=mask), 
            cdo.spatSd(gcm.file,period,mask=mask,monthly=TRUE))
      }
      if("mean" %in% stats & !"mean" %in% names(X[[store.name]][[srex.regions[j]]])) {
        if(verbose) print("mean value")
        X[[store.name]][[srex.regions[j]]]$mean <- 
          c(cdo.mean(gcm.file,period,mask=mask), 
            cdo.mean(gcm.file,period,mask=mask,monthly=TRUE))
      }
      if("corr" %in% stats & !reference %in% names(X[[store.name]][[srex.regions[j]]]$corr)) {
        if(verbose) print("spatial correlation")
        X[[store.name]][[srex.regions[j]]]$corr[[reference]] <- 
          c(cdo.gridcor(gcm.file,ref.file,period,mask=mask), 
            cdo.gridcor(gcm.file,ref.file,period,mask=mask,monthly=TRUE))
      }
      if("rmse" %in% stats & !reference %in% names(X[[store.name]][[srex.regions[j]]]$rmse)) {
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

