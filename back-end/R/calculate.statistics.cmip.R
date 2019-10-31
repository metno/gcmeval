cleanstr <- function(x) {
  return(tolower(gsub("[[:punct:]]","",x)))
}

## Calculate the mean annual cycle and spatial correlation for CMIP models
calculate.statistics <- function(files.in, meta=NULL, file.out="statistics.rda", ref=NULL,
                                 path.in=NULL, path.out=NULL, path.ref=NULL,
                                 stats=c("mean.gcm","spatial.sd.gcm","mean.ref","spatial.sd.ref","corr","rmse"),
                                 period=c(1981,2010), add=TRUE, force=FALSE, verbose=FALSE) {
  if(verbose) print("calculate.statistics")
  if(!is.null(path.in)) files.in <- file.path(path.in, files.in)
  if(!is.null(path.out)) file.out <- file.path(path.out, file.out)
  if(min(period)>2010) ref <- NULL
  label.period <- paste0("period.",paste(period, collapse="_"))
  
  if(is.null(ref)) stats <- stats[!stats %in% c("corr","rmse","cmpi","mean.ref","spatial.sd.ref")]
    
  M <- list()
  for(f in files.in) {
    if(!is.null(meta) & basename(f) %in% meta$filename) {
      meta.f <- meta[meta$filename==basename(f),]
    } else {
      nc <- getncid(filename=f, verbose=verbose)
      meta.f <- metaextract.cmip(nc,verbose=verbose)
    }
    M$filename <- c(M$filename, basename(f))
    for(x in c("var","unit","project_id","experiment","gcm.i")) {
      M[[x]] <- c(M[[x]], as.character(meta.f[[x]]))
    }
  }
  M$ref <- rep(NA, length(M$filename))
  if(!is.null(ref)) {
    if(verbose) print("Get reference data")
    for(var in unique(M$var)) {
      file.ref <- try(getReference(reference=ref, variable=var, path=path.ref))
      if(!inherits(file.ref,"try-error")) {
        res.ref <- resolution(file.ref, dim=c("lat","latitude"))
        if(res.ref<0) {
          if(verbose) print("Correct inverted latitudes of ref data")
          ref.nc <- gsub(".nc",".invertlat.nc",basename(file.ref))
          cdo.command("invertlat","",file.ref,"tmp.nc")
          cdo.command("copy","","tmp.nc",ref.nc)
          file.remove("tmp.nc")
          file.ref <- ref.nc
        }
        if(verbose) print("Check unit of reference data against unit of GCM data")
        nc <- getatt(file.ref)
        var.ref <- names(nc$var)[!grepl("time|lon|lat",names(nc$var))]
        units.ref <- nc$var[[var.ref]]$units
        for(units in unique(M$unit[M$var==var])) {
          if(units!=units.ref) {
            ref.nc <- gsub(".nc",".unitfix.nc",basename(file.ref))
            if(var=="pr") {
              c.ref <- 1
              if(grepl("/s|s-1",units) & !grepl("/s|s-1",units.ref)) {
                c.ref <- c.ref/(60*60*24)
              } else if(!grepl("/s|s-1",units) & grepl("/s|s-1",units.ref)) {
                c.ref <- c.ref*(60*60*24)
              }
              if(grepl("mm|kg m-2",units) & !grepl("mm|kg m-2",units.ref)) {
                c.ref <- c.ref*1E3
              } else if(!grepl("mm|kg m-2",units) & grepl("mm|kg m-2",units.ref)) {
                c.ref <- c.ref*1E-3
              }
              cdo.command("mulc", c.ref, file.ref, ref.nc)
            } else if(var=="tas") {
              c.ref <- 0
              if(grepl(units,"K") & grepl(units.ref,"C")) {
                c.ref <- 273.15
              } else if(grepl(units,"C") & grepl(units.ref,"K")) {
                c.ref <- -273.15
              }
              cdo.command("add", c.ref, file.ref, ref.nc)
            }
            file.ref <- ref.nc
          }
          for(i in which(M$var==var & M$unit==units)) M$ref[[i]] <- file.ref
        }
      }
    }
  }
  
  statistics <- list()
  if(add & file.exists(file.out)) load(file.out)
  for(f in files.in) {
    i <- which(M$filename==basename(f))
    ref.f <- M$ref[i]
    var.f <- M$var[[i]]
    pid.f <- M$project_id[[i]]
    exp.f <- cleanstr(M$experiment[[i]])
    gcm.f <- M$gcm.i[[i]]
    stats.f <- stats
    if(is.na(ref.f)) {
      ref.f <- NULL
      stats.f <- stats[!stats %in% c("corr","rmse")]
      if("mean" %in% stats) stats.f[[stats.f=="mean"]] <- "mean.gcm"
      if("spatial.sd" %in% stats) stats.f[[stats.f=="spatial.sd"]] <- "spatial.sd.gcm"
    } else {
      stats.f <- stats
      if("mean" %in% stats) stats.f <- c(stats.f[stats.f!="mean"], "mean.gcm", "mean.ref")
      if("spatial.sd" %in% stats) stats.f <- c(stats.f[stats.f!="spatial.sd"], "spatial.sd.gcm", "spatial.sd.ref")
    }
    if(is.null(ref)) {
      S.ref <- NULL
    } else {
      S.ref <- statistics[[var.f]][[ref]][[label.period]]
    }
    S.gcm <- statistics[[var.f]][[exp.f]][[label.period]][[gcm.f]]
    if(!force) {
      if(!is.null(S.ref)) {
        for(x in names(S.ref[[length(S.ref)]])) {
          stats.f <- stats.f[!stats.f %in% paste0(x,".ref")]
        }
      }
      if(!is.null(S.gcm)) {
        for(x in names(S.gcm[[length(S.gcm)]])) {
          if(x %in% c("corr","rmse")) {
            if(ref %in% names(S.gcm[[length(S.gcm)]][[x]])) {
              stats.f <- stats.f[!stats.f %in% x]
            }
          } else {
            stats.f <- stats.f[!stats.f %in% paste0(x,".gcm")]
          }
        }
      }
    }
    if(length(stats.f)>0) {
      S.new <- calculate.statistics.cmip(f, file.ref=ref.f, stats=stats.f, period=period, verbose=verbose)
      if("GCM" %in% names(S.new)) {
        if(verbose) print(paste("Add new statistics for GCM",f))
        for(region in names(S.new$GCM)) {
          if(verbose) print(paste("Region",region))
          for(st in names(S.new$GCM[[region]])) {
            if(verbose) print(paste("Statistic",st))
            if(is.null(ref)) {
              S.gcm[[region]][[st]] <- S.new$GCM[[region]][[st]]
            } else if(ref %in% names(S.new$GCM[[region]][[st]])) {
              S.gcm[[region]][[st]][[ref]] <- S.new$GCM[[region]][[st]][[ref]]
            } else {
              S.gcm[[region]][[st]] <- S.new$GCM[[region]][[st]]
            }
          }
        }
      }
      if(!is.null(ref)) {
        if(ref %in% names(S.new)) {
          if(verbose) print(paste("Add new statistics for reference",ref))
          for(region in names(S.new[[ref]])) {
            if(verbose) print(paste("Region",region))
            for(st in names(S.new[[ref]][[region]])) {
              if(verbose) print(paste("Statistic",st))
              S.ref[[region]][[st]] <- S.new[[ref]][[region]][[st]]
            }
          }
        }
      }
      if(!is.null(S.gcm)) statistics[[var.f]][[exp.f]][[label.period]][[gcm.f]] <- S.gcm
      if(!is.null(S.ref)) statistics[[var.f]][[ref]][[label.period]] <- S.ref
    }
  }
  
  if("cmpi" %in% stats & !is.null(statistics)) {
    if(verbose) print("Calculate CMPI for all regions and GCMs")
    for(var in names(statistics)) {
      X <- statistics[[var]]
      Y <- X[grep("rcp|ssp",names(X))]
      for(rcp in names(Y)) {
        Z <- Y[[rcp]][[label.period]]
        W <- Z[grepl("CMIP|gcm",names(Z))]
        if(is.null(W[[1]][[1]]$rmse)) {
          if(verbose) print("Warning! Cannot calculate CMPI because rmse hasn't been calculated yet.")
        } else if (!ref %in% names(W[[1]][[1]]$rmse)) {
          if(verbose) print("Warning! Cannot calculate CMPI because rmse hasn't been calculated yet.")
        } else {
          median.rmse <- list()
          for(region in names(W[[1]])) {
            med <- try(median(unlist(lapply(W, function(x) x[[region]]$rmse[[ref]]))))
            if(inherits(med,"try-error")) browser()
            median.rmse[[region]] <- med
          }
          for(gcm in names(W)) {
            for(region in names(W[[gcm]])) {
              if(is.null(W[[gcm]][[region]]$rmse[[ref]])) {
                if(verbose) print("Warning! Cannot calculate cmpi because rmse hasn't been calculated yet.")
              } else {
                cmpi <- (W[[gcm]][[region]]$rmse[[ref]] - median.rmse[[region]])/median.rmse[[region]]
                statistics[[var]][[rcp]][[label.period]][[gcm]][[region]]$cmpi[[ref]] <- cmpi
              }
            }
          }
        }
      }
    } 
  }
  if(!is.null(file.out)) save(statistics, file=file.out)
  invisible(statistics)
}

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

