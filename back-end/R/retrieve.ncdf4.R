retrieve.ncdf4 <- function (ncfile=ncfile, path=NULL , param="auto",
                            lon=NULL, lat=NULL, lev=NULL, it=NULL,
                            miss2na=TRUE, greenwich=FALSE,
                            plot=FALSE, verbose=FALSE, ...)  {
  if(verbose) print("retrieve.ncdf4")
  lon.rng  <- lon
  lat.rng  <- lat
  lev.rng  <- lev
  time.rng <- it
  
  ## check if file exists and type of ncfile object
  if (is.character(ncfile)) {
    if (!file.exists(ncfile)) {
      stop(paste("Sorry, the netcdf file '", ncfile,
                 "' does not exist or the path has not been set correctly!",sep =""))
    } else {
      ncid <- ncdf4::nc_open(ncfile)
    }
  } else if (class(ncfile) == "ncdf4") {
    ncid <- ncfile
  } else {
    stop("ncfile format should be a valid netcdf filename or a netcdf id of class 'ncdf4'")
  }
  
  ## Read and put attributes in model
  model <- ncdf4::ncatt_get(ncid,0)
  ## Get variable attributes in v1
  namevars <- names(ncid$var)
  if (tolower(param) == "auto") {
    if (ncid$nvars > 1) {
      i <- length(namevars)
    } else {
      i <- 1
    }
    param <- names(ncid$var)[i]
    v1 <- ncid$var[[i]] 
  } else {
    v1 <- NULL
    i <- grep(param,namevars)
    v1 <- eval(parse(text=paste("ncid$var[[",i,"]]",sep="")))
    if (is.null(v1)) {
      stop(paste("Variable ",param," could not be found!",sep=""))
    }
  }
  
  ## Get dimensions and dimension names
  dimnames <- rep(NA,v1$ndims)
  for (i in 1:v1$ndim) {
    dimnames[i] <- tolower(v1$dim[[i]]$name)
  }
  ## Get lon, lat, lev, time attr and values and update values if necessary
  ## Longitudes
  ilon <- which(tolower(dimnames) %in% c("x","i") | grepl("lon|ncells",tolower(dimnames)))
  if (length(ilon)==0) {
    ilon <- NULL
  } else if (length(ilon)>1) {
    stop("Error in dim lon")
  }
  if (!is.null(ilon)) {
    lon <- eval(parse(text=paste("v1$dim[[",as.character(ilon),"]]",sep="")))
  } else {
    lon <- NULL
  }
  if (!is.null(ilon)) {
    ilonunit <- grep("unit",names(lon))
    if (length(ilonunit>1)) {
      if (verbose) print(paste("Longitude unit is :",lon$unit,sep=" "))
      lonunit <- eval(parse(text = paste("lon$",names(lon)[ilonunit],sep="")))
      if (length(grep("degree.*.east",lonunit))<1) {
        stop("'retrieve.ncdf4' is not suited to extract longitude units different from 'degrees_east'")
      }
    }
  }
  
  ## Update longitude values if greenwich is FALSE
  if (!greenwich) {
    id <- lon$vals > 180
    if (sum(id) > 0) {
      if (verbose) print("Convert to non-Greenwich")
      lon$vals[id] <- lon$vals[id] - 360
    }
  } else {
    id <- lon$vals < 0
    if (sum(id) > 0) {
      if (verbose) print("Convert to Greenwich")
      lon$vals[id] <- lon$vals[id] + 360
    }
  }
  
  ## Latitudes
  ilat <- which(tolower(dimnames) %in% c("y","j") | grepl("lat",tolower(dimnames)))
  if (length(ilat) ==0) {
    ilat <- NULL
  } else if (length(ilat) > 1) {
    stop("Error in dim lat")
  }
  if (!is.null(ilat)) {
    lat <- eval(parse(text=paste("v1$dim[[",as.character(ilat),"]]",sep="")))
  } else {
    lat <- NULL
  }
  
  ## Pressure Level if pressure variable / not used for surface variables
  ilev <- grep("lev|hei", dimnames)
  if (length(ilev) ==0) {
    ilev <- NULL
  } else if (length(ilev)>1) {
    stop("Error in dim lev")
  }
  if (!is.null(ilev)) {
    lev <- eval(parse(text=paste("v1$dim[[",as.character(ilev),"]]",sep="")))
  } else {
    lev <- NULL
  }
  
  ## Time
  itime <- grep("tim", dimnames)
  if (length(itime) ==0) {
    itime <- NULL
  } else if (length(itime)>1) {
    stop("Error in dim time")
  }
  if (!is.null(itime)) {
    time <- eval(parse(text=paste("v1$dim[[",as.character(itime),"]]",sep="")))
  } else {
    time <- NULL
  }
  ## Check & update meta data from the data itself
  ncid2 <- check.ncdf4(ncid,param=param,verbose=verbose) 
  if (length(grep("model",ls())) > 0) model <- ncid2$model 
  if (!is.null(itime)) time <- ncid2$time
  rm(ncid2)
  
  if (verbose) print(model$frequency)
  ## Subselect a spatial and a temporal domain
  ## Single point extraction
  one.cell <- FALSE
  if ((length(lon.rng) == 1) & (length(lat.rng)==1)) {
    lons <- rep(as.vector(lon$vals),length(lat$vals))
    lats <- rep(as.vector(lat$vals),length(lon$vals))
    
    dmin <- distAB(lon=lon.rng,lat=lat.rng,lons=lons,lats=lats) 
    id <- which(dmin==min(dmin,na.rm=TRUE))
    lon.w <- which(lon$vals==lons[id])
    lat.w <- which(lat$vals==lats[id])
    if (verbose) {
      print(paste("Single point extraction"))
      print(paste("Selected nearest grid cell lon :",
                  as.character(lon$vals[lon.w]),lon$unit,sep=" "))
    }  
    one.cell <- TRUE
  }
  
  ## longitude extract range
  if (!is.null(ilon)) {
    if (!is.null(lon.rng)) {
      if (length(lon.rng) > 2) {
        stop("lon.rng should be in the form of c(x1,x2)")
      } else if (length(lon.rng) == 1) {
        lon.w <- which((lon$vals-lon.rng) == min(abs(lon$vals-lon.rng)))
        if (verbose) print(paste("Single point extraction / Selected nearest grid cell lon :",
                                 as.character(lon$vals[lon.w]),lon$unit,sep=" "))
      } else if (length(lon.rng)==2) {
        lon.w <- which((lon$vals >= lon.rng[1]) &
                         (lon$vals <= lon.rng[length(lon.rng)]))
        if (verbose) print(paste("Selected longitudes:",paste(as.character(sort(lon$vals[lon.w])),
                                                              collapse="/"),lon$units,sep=" "))
      }
    } else {
      lon.w <- seq(1,length(lon$vals),1)
    }
    lon$len <- length(lon.w)
  }
  
  ## latitude extract range
  if (!is.null(ilat)) {
    if (!is.null(lat.rng)) {
      if (length(lat.rng) > 2) {
        stop("lat.rng should be in the form of c(y1,y2)")
      } else if (length(lat.rng) == 1) {
        lat.w <- which((lat$vals-lat.rng) == min(abs(lat$vals-lat.rng)))
        if (verbose) print(paste("Single point extraction / Selected nearest grid cell lat :",
                                 as.character(lat$vals[lat.w]),lat$unit,sep=" "))
      } else if (length(lat.rng) == 2) { 
        lat.w <- which((lat$vals >= lat.rng[1]) &
                         (lat$vals <= lat.rng[length(lat.rng)]))
        if (verbose) print(paste("Selected Latitudes:",paste(as.character(lat$vals[lat.w]),
                                                             collapse="/"),lat$units,sep=" "))
      }
    } else {
      lat.w <- seq(1,length(lat$vals),1)
    }
    lat$len <- length(lat.w)
  }
  
  ## time extract range
  if (!is.null(itime)) {
    if (!is.null(time.rng)) {
      if (length(time.rng) > 2) {
        stop("time.rng should be in the form of c(year1,year2)")
      } else if (length(time.rng) == 1) {
        time.w <- which((time$vals-time.rng) == min(abs(time$vals-time.rng)))
        if (verbose) print(paste("Single time extraction:",as.character(time$vals[time.w]),
                                 time$unit,sep=" "))
      } else if (length(time.rng) == 2) {
        if (sum(is.element(time.rng,format.Date(time$vdate,"%Y"))) < 1) {
          stop("Selected time interval is outside the range of the data")
        }
        time.w <- which((format.Date(time$vdate,"%Y") >= time.rng[1]) &
                          (format.Date(time$vdate,"%Y") <= time.rng[length(time.rng)]))
        if (verbose) {
          if (model$frequency == "mon") {
            print(paste("Selected time values:",
                        paste(as.character(format.Date(time$vdate[time.w],"%Y-%m")),
                              collapse="/"),model$frequency,sep=" "))
          } else {
            print(paste("Selected time values:",
                        paste(as.character(time$vdate[time.w]),collapse="/"),
                        model$frequency,sep=" "))
          }
        }
        if ((length(grep("time.w",ls())) < 1) | (length(time.w)<1)) {
          stop("No time overlapping with selected time interval")
        }
      }
    } else {
      time.w <- seq(1,length(time$vals),1)
    }
    time$vdate <- time$vdate[time.w]
    time$len <- length(time.w)
  } 
  
  ## level extract range
  if (!is.null(ilev)) {
    if (is.null(lev.rng)) {
      lev.rng <- as.integer(readline(paste("Specify one level from the list and type 'Enter'",
                                           paste(param,"(",paste(lev$val,collapse="/"),lev$levelUnit,")",sep=""))))
    } else if (length(lev.rng)>1) {
      lev.rng <- as.integer(readline(paste("Enter a single level value from the list and type 'Enter'",
                                           paste(param,"(",paste(lev$val,collapse="/"),lev$levelUnit,")",sep=""))))
    } 
    if (length(lev.rng) == 1) {
      lev.w <- which((lev$vals-lev.rng) == min(abs(lev$vals-lev.rng)))
      if (verbose) print(paste("Single level extraction:",
                               as.character(lev$vals[lev.w]),
                               lev$unit,sep=" "))
    } else {
      lev.w <- seq(1,length(lev$vals),1)
    }
    lev$len <- length(lev.w)
  }
  
  ## Extract values and add Scale Factor and offset if any
  if (verbose) print(paste("Reading data for ",v1$longname,sep=""))
  if ((one.cell) & (!is.null(itime))) {
    if (!is.null(ilev)) {
      start <- c(lon.w,lat.w,lev.w[1],time.w[1])
      count <- c(1,1,length(lev.w),length(time.w))
      val <- ncdf4::ncvar_get(ncid,param,start,count)
    } else {
      start <- c(lon.w,lat.w,time.w[1])
      count <- c(1,1,length(time.w))
      val <- ncdf4::ncvar_get(ncid,param,start,count)
    }
    lon$vals <- lon$vals[lon.w]
    lat$vals <- lat$vals[lat.w]
  } else if ((!is.null(ilon)) & (!is.null(itime))) {  
    diff.lon.w <- diff(rank(lon$vals[lon.w]))
    id2 <- which(diff.lon.w!=1)
    if (!is.null(ilev)) {
      if ((sum(id) > 0) & (sum(id2)!=0)) { ## & !greenwich    
        count <- c(length(lon.w),length(lat.w),length(lev.w),length(time.w))
        lon.w1 <-lon.w[1:id2]
        lon.w2 <- lon.w[(id2+1):length(lon.w)]
        start1 <- c(lon.w1[1],lat.w[1],lev.w[1],time.w[1])
        count1 <- c(length(lon.w1),length(lat.w),length(lev.w),length(time.w))
        val1 <- ncdf4::ncvar_get(ncid,param,start1,count1,collapse_degen=FALSE)
        d1 <- dim(val1)
        dim(val1) <- c(d1[1],prod(d1[2:length(d1)]))
        start2 <- c(lon.w2[1],lat.w[1],lev.w[1],time.w[1])
        count2 <- c(length(lon.w2),length(lat.w),length(lev.w),length(time.w))
        val2 <- ncdf4::ncvar_get(ncid,param,start2,count2,collapse_degen=FALSE)
        d2 <- dim(val2)
        dim(val2) <- c(d2[1],prod(d2[2:length(d2)]))
        val <- rbind(val1,val2)
      } else {
        start <- c(lon.w[1],lat.w[1],lev.w[1],time.w[1])
        count <- c(length(lon.w),length(lat.w),length(lev.w),length(time.w))
        val <- ncdf4::ncvar_get(ncid,param,start,count,collapse_degen=FALSE)
      }
      dim(val) <- count
     lon$vals <- lon$vals[lon.w]
      lon.srt <- order(lon$vals)
      if (sum(diff(lon.srt)!=1)) {
        if (verbose) print("Sort Longitudes") 
        lon$vals <- lon$vals[lon.srt]
      }
      lat$vals <- lat$vals[lat.w]
      lat.srt <- order(lat$vals)
      if (sum(diff(lat.srt)!=1)) {
        if (verbose) print("Sort Latitudes") 
        lat$vals <- lat$vals[lat.srt]
      }
      val <- val[lon.srt,lat.srt,,]
      dim(val) <- count
    } else {
      if ((sum(id) > 0) & (sum(id2)!=0)) { ## & !greenwich
        count <- c(length(lon.w),length(lat.w),length(time.w))
        lon.w1 <-lon.w[1:id2]
        lon.w2 <- lon.w[(id2+1):lon$len]
        start1 <- c(lon.w1[1],lat.w[1],time.w[1])
        count1 <- c(length(lon.w1),length(lat.w),length(time.w))
        val1 <- ncdf4::ncvar_get(ncid,param,start1,count1,collapse_degen=FALSE)
        d1 <- dim(val1)
        dim(val1) <- c(d1[1],prod(d1[2:length(d1)]))
        start2 <- c(lon.w2[1],lat.w[1],time.w[1])
        count2 <- c(length(lon.w2),length(lat.w),length(time.w))
        val2 <- ncdf4::ncvar_get(ncid,param,start2,count2,collapse_degen=FALSE)
        d2 <- dim(val2)
        dim(val2) <- c(d2[1],prod(d2[2:length(d2)]))
        val <- rbind(val1,val2)
        stopifnot((d1[2]==d2[2]) | (d1[3]==d2[3]))
      } else {
        start <- c(lon.w[1],lat.w[1],time.w[1])
        count <- c(length(lon.w),length(lat.w),length(time.w))
        val <- ncdf4::ncvar_get(ncid,param,start,count)
      }
      dim(val) <- count   
      lon$vals <- lon$vals[lon.w]
      lon.srt <- order(lon$vals)
      if (sum(diff(lon.srt)!=1)!=0) {
        if (verbose) print("Sort Longitudes") 
        lon$vals <- lon$vals[lon.srt]
      } 
      lat$vals <- lat$vals[lat.w]
      lat.srt <- order(lat$vals)
      if (sum(diff(lat.srt)!=1)!=0) {
        if (verbose) print("Sort Latitudes") 
        lat$vals <- lat$vals[lat.srt]
      }
      val <- val[lon.srt,lat.srt,]
    }
  }
  
  ## Convert units
  iunit <- grep("unit",names(v1))
  if (length(iunit)>0) {
    text=paste("v1$",names(v1)[iunit],sep="")
    units <- eval(parse(text=text))
    # hebe added extra units test for unusual strings
    if (units=="") {
      try(tmp <- grep("unit",names(ncdf4::ncatt_get(ncid,param)),value=TRUE),silent = !verbose)
      if ((!inherits(tmp, "try-error")) & (length(tmp)!=0)) {
        units<-gsub(" ","",eval(parse(text=paste('ncdf4::ncatt_get(ncid, param)$',tmp,sep = ""))))
      }
    }
    if (((units=="K") | (units=="degK")) & !grepl("anom",v1$longname)) {
      val <- val - 273 
      units <- "degC"
    }
    if ((length(grep("pa",tolower(units)))>0) &
        (!grepl("vapo",tolower(v1$longname))) |
        (length(grep("N",tolower(units)))>0)) {
      val <- val/100 
      units <- "hPa"
    }
    ## 
    if ((units=="Kg/m^2/s") | (units=="kg m-2 s-1") | (max(abs(val),na.rm=TRUE)<0.001)) {
      val <- val * (24*60*60)
      units <- "mm/day"
    } 
    if (verbose) print(paste("Data converted to unit:",units, sep= " "))
  } else if (max(val,na.rm=TRUE)<0.001) {
    if (verbose) print('Variable is likely to be precipitation intensity!')
    val <- val * (24*60*60)
    units <- "mm/day"
  }
  
  ## replace missval by NA
  if (miss2na) {
    imissval <- grep("miss",names(v1))
    if (length(imissval)>0) {
      text=paste("v1$",names(v1)[imissval],sep="")
      missval <- eval(parse(text=text))
      val[val == missval] <- NA
    }
    if (verbose) print(paste(as.character(sum(is.na(val))),"missing values have been replaced by NA" , sep = " "))
  }
  
  ## 
  if (verbose) print("Done !")
  
  ## Copy "filename" attribute to model attributes
  model$filename <- ncid$filename
  
  ## close netcdf file
  ncdf4::nc_close(ncid)
  
  ## Create output and save attributes to the results # 
  d <- dim(val)
  if(is.null(d)) {
    d <- c(length(lon$vals),length(lat$vals),length(time$vals))
    d <- d[match(seq(length(d)),c(ilon,ilat,itime))]
  }
  if (verbose) {print("dimensions"); print(d)}
  
  if (!one.cell) {
    if (is.null(ilev)) {
      if ((length(d)==2) & (length(time$vdate)==1)) { 
        d<-c(d[ilon],d[ilat],1)
        dim(val) <- c(d[ilon]*d[ilat],1) 
      } else {
        dim(val) <- c(d[ilon]*d[ilat],d[itime])
      }
    } else {
      if (length(lev.w)==1) {
        dim(val) <- c(d[ilon]*d[ilat],d[itime]) 
        d <- d[-ilev]
      } else {
        dim(val) <- c(d[ilon]*d[ilat]*d[ilev],d[itime])
        print("Warning: Please select one level to retrieve the data (e.g. lev=1000)")
      }   
    }
  }
  
  ## Create a zoo object z
  if (one.cell) {
    z <- zoo::zoo(x=val,order.by=time$vdate)  
  } else {
    z <- zoo::zoo(x=t(val),order.by=time$vdate)
  }
  ## Add attributes to z
  if (!is.null(v1)) {
    attr(z,"variable") <- param
    attr(z,"longname") <- v1$longname
    attr(z,"units") <- units
    attr(z,"dimensions") <- d
  }  
  if (!is.null(ilon)) {
    attr(z,"longitude") <- lon$vals
    attr(z,"greenwich") <- greenwich
  }
  if (!is.null(ilat)) {
    attr(z,"latitude") <- lat$vals
  }
  if (!is.null(ilev)) {
    attr(z,"level") <- lev$vals[lev.w]
    attr(z,"levelUnit") <- lev$units
  }
  if (!is.null(itime)) {
    attr(z,"calendar") <- time$calendar
  }
  ## Add attributes
  attr(z, "file") <- model$filename
  attr(z, "source")         <- model$project_id
  attr(z, 'timeunit')       <- model$frequency
  attr(z, 'frequency')      <- 1
  mattr <- names(model)[!names(model) %in% c(names(attributes(z)),"project_id","filename")]
  for(a in mattr) attr(z, a) <- model[[a]]
  attr(z, "model_history") <- model$history
  attr(z, "call")           <- match.call()
  if(is.null(attr(z,"institution"))) attr(z, "institution") <- NA 
  if(is.null(attr(z,"reference"))) attr(z, "reference") <- NA
  if (one.cell) {
    class(z) <- c("station",model$frequency,"zoo")
    attr(z,'location') <- 'Grid cell'
  } else { 
    class(z) <- c("field",model$frequency,"zoo")
  }
  invisible(z)
} 
