calculate.statistics.cmip <- function(file.gcm, file.ref=NULL, period=c(1981,2010), 
                                      mask="coords.txt", stats=c("mean","spatial.sd","corr","rmse"),
                                      verbose=FALSE) {
  if(verbose) print("calculate.statistics.cmip")
  shape <- get.shapefile("referenceRegions.shp")
  srex.regions <- as.character(shape$LAB)
  if(min(period)>2010 | !any(c("corr","rmse") %in% stats)) file.ref <- NULL
  
  if(verbose) print("Find GCM file")
  if(inherits(file.gcm,"ncdf4")) {
    nc <- file.gcm
    file.gcm <- nc$filename
  } else if(inherits(file.gcm,"character") & file.exists(file.gcm)) {
    if(verbose) print(paste("Calculating statistics for file",file.gcm))
    #nc <- getncid(file.gcm)
  } else {
    warning(paste("File",file.gcm,"does not exist."))
    return()
  }

  if("mean" %in% stats) stats <- c(stats, paste("mean", c("gcm", "ref"), sep="."))
  if("spatial.sd" %in% stats) stats <- c(stats, paste("spatial.sd", c("gcm", "ref"), sep="."))
   
  X <- list()
  if(is.null(file.ref)) {
    stats <- stats[!stats %in% c("corr","rmse","spatial.sd.ref","mean.ref")]
  } else if(!is.character(file.ref)) {
    warning("Warning! Reference file not found. Continuing without reference data.")
    stats <- stats[!stats %in% c("corr","rmse","spatial.sd.ref","mean.ref")]
  } else {
    fn <- function(x) tolower(gsub("[[:punct:]]", "", x))
    if(grepl("era5", fn(file.ref))) {
      ref <- "era5"
    } else if(grepl("eraint", fn(file.ref))) {
      ref <- "eraint"
    } else if(grepl("gpcp", fn(file.ref))) {
      ref <- "gpcp"
    } else {
      ref <- "ref"
    }
    
    if(any(grepl("ref",stats))) {
      if(verbose) print("Global")
      if(verbose) print("Calculate basic statistics for reference data")
      if("spatial.sd.ref" %in% stats & !"spatial.sd" %in% names(X[[ref]]$global)) {
        if(verbose) print("spatial st dev")
        X[[ref]]$global$spatial.sd <- as.list(c(cdo.spatSd(file.ref,period),
                                                cdo.spatSd(file.ref,period,monthly=TRUE)))
      }
      if("mean.ref" %in% stats & !"mean" %in% names(X[[ref]]$global)) {
        if(verbose) print("mean value")
        X[[ref]]$global$mean <- as.list(c(cdo.mean(file.ref,period),
                                          cdo.mean(file.ref,period,monthly=TRUE)))
      }
      for(i in 1:length(srex.regions)) {
        if(verbose) print(paste0("Region ",i,": ",srex.regions[i]))
        getPolCoords(i,shape=shape,destfile=mask)
        if("spatial.sd.ref" %in% stats & !"spatial.sd" %in% names(X[[ref]][[srex.regions[i]]])) {
          if(verbose) print("spatial st dev")
          X[[ref]][[srex.regions[i]]]$spatial.sd <-
            as.list(c(cdo.spatSd(file.ref,period,mask=mask), 
                      cdo.spatSd(file.ref,period,mask=mask,monthly=TRUE)))
        }
        if("mean.ref" %in% stats & !"mean" %in% names(X[[ref]][[srex.regions[i]]])) {
          if(verbose) print("mean value")
          X[[ref]][[srex.regions[i]]]$mean <-
            as.list(c(cdo.mean(file.ref,period,mask=mask), 
                      cdo.mean(file.ref,period,mask=mask,monthly=TRUE)))
        }
      }
    }
  }
  
  if(verbose) print("Calculate basic statistics")
  if(verbose) print("Global")
  if("spatial.sd.gcm" %in% stats & !"spatial.sd" %in% names(X[["GCM"]]$global)) {
    if(verbose) print("spatial st dev")
    X[["GCM"]]$global$spatial.sd <- as.list(c(cdo.spatSd(file.gcm,period),
                                              cdo.spatSd(file.gcm,period,monthly=TRUE)))
  }
  if("mean.gcm" %in% stats & !"mean" %in% names(X[["GCM"]]$global)) {
    if(verbose) print("mean value")
      X[["GCM"]]$global$mean <- as.list(c(cdo.mean(file.gcm,period),
                                          cdo.mean(file.gcm,period,monthly=TRUE)))
  }
  if("corr" %in% stats) {
    if(verbose) print("spatial correlation")
    res.gcm <- resolution(file.gcm, dim=c("lat","latitude"))
    res.ref <- resolution(file.ref, dim=c("lat","latitude"))
    if(res.ref!=res.gcm) {
      browser()
      ref.new <- gsub(".nc",paste0("_",sprintf("%.2f",res.gcm),"deg.nc"),basename(file.ref))
      if(file.exists(ref.new)) {
        file.ref <- ref.new
      } else {
        if(verbose) print("Regrid reference data to GCM grid")
        cdo.regrid(file.ref,ref.new,res.lon=res.gcm,res.lat=res.gcm,
                   remap="remapcon",verbose=verbose)
        file.ref <- ref.new
      }
    }
    X[["GCM"]]$global$corr[[ref]] <- as.list(c(cdo.gridcor(file.gcm,file.ref,period),
                                               cdo.gridcor(file.gcm,file.ref,period,monthly=TRUE)))
  }
  if("rmse" %in% stats) { 
    if(verbose) print("rmse")
    r <- retrieve(file.ref)
    lon <- attr(r,"longitude")
    lat <- attr(r,"latitude")
    weights <- calculate.mon.weights(lon,lat)
    ref.mon.file <- "ref.monmean.nc"
    gcm.mon.file <- "gcm.monmean.nc"
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                file.ref, ref.mon.file)
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                file.gcm, gcm.mon.file)
    gcm.mon <- zoo::coredata(retrieve(gcm.mon.file))
    ref.mon <- zoo::coredata(retrieve(ref.mon.file))
    dim(gcm.mon) <- dim(ref.mon) <- c(12,length(attr(ref.mon,"longitude")),length(attr(ref.mon,"latitude")))
    X[["GCM"]]$global$rmse[[ref]] <- sqrt(sum(weights*(gcm.mon-ref.mon)^2)/sum(weights))#rmse.global
  }
  for(j in 1:length(srex.regions)) {
    if(verbose) print(paste0("Region ",j,": ",srex.regions[j]))
    getPolCoords(j,shape=shape,destfile=mask)
    if(verbose) print("Calculate basic statistics")
    if("spatial.sd.gcm" %in% stats & !"spatial.sd" %in% names(X[["GCM"]][[srex.regions[j]]])) {
      if(verbose) print("spatial st dev")
      X[["GCM"]][[srex.regions[j]]]$spatial.sd <- 
        as.list(c(cdo.spatSd(file.gcm,period,mask=mask), 
                  cdo.spatSd(file.gcm,period,mask=mask,monthly=TRUE)))
    }
    if("mean.gcm" %in% stats & !"mean" %in% names(X[["GCM"]][[srex.regions[j]]])) {
      if(verbose) print("mean value")
      X[["GCM"]][[srex.regions[j]]]$mean <- 
        as.list(c(cdo.mean(file.gcm,period,mask=mask), 
                  cdo.mean(file.gcm,period,mask=mask,monthly=TRUE)))
    }
    if("corr" %in% stats) { 
      if(!"corr" %in% names(X[["GCM"]][[srex.regions[j]]])) {
        if(verbose) print("spatial correlation")
        X[["GCM"]][[srex.regions[j]]]$corr[[ref]] <- 
          as.list(c(cdo.gridcor(file.gcm,file.ref,period,mask=mask), 
                    cdo.gridcor(file.gcm,file.ref,period,mask=mask,monthly=TRUE)))
      }   
    }
    if("rmse" %in% stats) { 
      if(!"rmse" %in% names(X[["GCM"]][[srex.regions[j]]])) {
        if(verbose) print("rmse")
        mask.j <- gen.mask.srex(destfile=file.gcm,mask.polygon=shape[j,])
        dim(gcm.mon) <- dim(ref.mon) <- c(12,length(attr(ref.mon,"longitude"))*length(attr(ref.mon,"latitude")))
        gcm.masked <- mask.zoo(gcm.mon,mask.j)
        ref.masked <- mask.zoo(ref.mon,mask.j)
        dim(gcm.masked) <- dim(ref.masked) <- 
          c(12,length(attr(ref.mon,"longitude")),length(attr(ref.mon,"latitude")))
        X[["GCM"]][[srex.regions[j]]]$rmse[[ref]] <- sqrt(sum(weights*(gcm.masked-ref.masked)^2,na.rm=TRUE)/
                                                          sum(weights[!is.na(gcm.masked)]))
      }
    }
  }

  if("rmse" %in% stats) {
    file.remove(gcm.mon.file)
    file.remove(ref.mon.file)
   }
  if(verbose) print("Done!")
  invisible(X)
}

