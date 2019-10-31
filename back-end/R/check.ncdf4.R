## function adopted from the esd-package
check.ncdf4 <- function(ncid, param="auto", verbose=FALSE) {
  if(verbose) print("check.ncdf4")
  qf <- NULL
  if (tolower(param) == "auto") {
    if (ncid$nvars > 1) {
      i <- length(names(ncid$var))
    } else i <- 1
    param <- names(ncid$var)[i] # ; rm(i)
    v1 <- ncid$var[[i]]
  } else {
    v1 <- NULL
    i <- grep(param, names(ncid$var))
    v1 <- eval(parse(text=paste("ncid$var[[",i,"]]",sep="")))
    if (is.null(v1)) {
      stop(paste("Variable",param,"could not be found!"))
    }
  } 

  ndims <- eval(parse(text=paste("ncid$var[[",i,"]]$ndims",sep="")))
  dimnames <- rep(NA,ndims)
  if (ndims>0) {
    for (j in 1:ndims) {
      dimnames[j] <- eval(parse(text=paste("ncid$var[[",i,"]]$dim[[",j,"]]$name",sep="")))
    }
    if (verbose) print("Checking Dimensions --> [ok]")
    if (verbose) print(paste(as.character(ndims),
                             " dimension(s) has(have) been found :"))
    if (verbose) print(dimnames)
  } else {
    stop("Checking Dimensions --> [fail]")
    if (verbose) print("The variable has no dimensions. The file may be corrupted!")  
  }
  
  model <- ncdf4::ncatt_get(ncid,0)
  mnames <- names(model)
  history <- ncdf4::ncatt_get(ncid,0,"history")
  
  if (ncdf4::ncatt_get(ncid,0,"project_id")$hasatt) {
    if (model$project_id=="IPCC Fourth Assessment") {
      model$project_id <- "CMIP3"
      if (verbose) print("project_id IPCC Fourth Assessment set to CMIP3")
    }
  } else if (length(grep("cmip6|ssp",tolower(history$value)))>0) {
    model$project_id <- "CMIP6"
    if(grepl("ssp",history$value)) {
      ssp <- unlist(strsplit(substr(history$value,regexpr("ssp",history$value),
                                    nchar(history$value)),split="_"))[1]
      model$experiment <- toupper(ssp)
      if(grepl("historical",history$value)) {
        model$experiment_id <- paste("historical",tolower(ssp),sep="+")
      } else {
        model$experiment_id <- toupper(ssp)
      }
    } else if(grepl("historical",history$value)) {
      model$experiment <- "historical"
      model$experiment_id <- "historical"
    }
  } else if (length(grep("rcp|cmip5",tolower(history$value)))>0) {
    model$project_id <- "CMIP5"
  } else if (length(grep("sres|cmip3",tolower(history$value)))>0) {
    model$project_id <- "CMIP3"
  } else {
    if (verbose) print("project_id is missing from file attributes")
    model$project_id <- NULL
  }

  if (!ncdf4::ncatt_get(ncid,0,"model_id")$hasatt &
      (ncdf4::ncatt_get(ncid,0,"project_id")$hasatt |
       !is.null(model$project_id))) {   
    hist2 <- unlist(strsplit(history$value,split=c(" ")))
    #ih <- grep("tas",hist2)
    ih <- grep(paste0(param,".*.nc"),hist2)
    if (model$project_id=="CMIP3") {
      txt <- hist2[ih[1]] 
      model$model_id <- cmip3.model_id(txt)
    } else if (model$project_id=="CMIP5") {
      txt <- hist2[ih[2]]
      model$model_id <- cmip5.model_id(txt)
    } else if (model$project_id=="CMIP6") {
      txt <- hist2[ih[2]]
      model$model_id <- cmip6.model_id(txt)
    }
  }

  if (ncdf4::ncatt_get(ncid,0,"experiment_id")$hasatt &
      (ncdf4::ncatt_get(ncid,0,"project_id")$hasatt |
       !is.null(model$project_id))) {
    if (tolower(model$project_id)=="cmip3") {
      txt <- unlist(strsplit(tolower(ncdf4::ncatt_get(ncid,0,"history")$value),split="/"))
      model$experiment_id <- paste(txt[grep("20c3m",txt)],txt[grep("sres",txt)],sep="-")
    }
  }

  if (ncdf4::ncatt_get(ncid,0,"title")$hasatt) {
    title <- ncdf4::ncatt_get(ncid,0,"title")$value
    modelid <- unlist(strsplit(title,split=c(" ")))
    if (length(grep('-',tolower(modelid))>0) & is.null(model$model_id)) {
      model$model_id <- modelid[grep('-',modelid)]
    }
    if (length(grep('cmip',tolower(modelid))>0) &
        is.null(model$project_id)) {
      model$project_id <- modelid[grep('cmip',tolower(modelid))]
    }

    if (length(grep('rcp',tolower(modelid))>0) &
        is.null(model$experiment_id)) {
      model$experiment_id <- modelid[grep('rcp',tolower(modelid))]
    }
    
    if (length(grep('ssp',tolower(modelid))>0) &
      is.null(model$experiment_id)) {
      model$experiment_id <- modelid[grep('ssp',tolower(modelid))]
    }
    model$type <- modelid[grep('-',modelid)+1] 
  }
  
  a <- Sys.info()
  itime <- grep("tim", tolower(dimnames))
  if (length(itime) == 0) {
    itime <- NULL
  } else if (length(itime) > 1) {
    stop("Error in time dim")
  } else if (length(itime)==1) {
    time <- eval(parse(text=paste("v1$dim[[",as.character(itime),"]]",sep="")))
  }
  
  tatt <- tolower(names(time))
  itunit <- grep(c("unit"),tatt)
  itorigin <- grep(c("orig"),tatt)
  if (length(itunit)>0) {   
    tunit <- eval(parse(text = paste("time$",tatt[itunit],sep="")))
    if (verbose) print(paste("Time unit has been found in time$unit attribute (",tunit,")",sep=""))
  } else {
    tunit <- NULL
  }
  if (!is.null(tunit)) {
    if (verbose) print("Checking Time Unit --> [ok]")
  } else {
    if (verbose) print("Checking Time Unit --> [fail]")
  }
  if (!is.null(tunit) & (!is.null(grep("since",tunit)))) {
    if (verbose) print("Time unit and origin detected in time$unit attribute")
    tunit <- time$units
    tsplit <- unlist(strsplit(tunit,split=" "))
    torigin <- time$origin <- paste(tsplit[3:length(tsplit)],collapse=" ")
    tunit <- time$units <- unlist(strsplit(tunit,split=" "))[1]
    if (verbose) print(paste("Updating time$unit (",time$unit,") and creating time$origin (",time$origin,") attribute",sep= ""))
  } else if (length(itorigin)>0) {   
    torigin <- eval(parse(text = paste("time$",tatt[itorigin],sep="")))
    if (verbose) print(paste("Time origin has been found in time origin attribute and set to:",torigin,sep=" "))
  } else {
    torigin <- NULL
  }
  if (is.null(torigin)) {
    if (verbose) print(paste("Time units:", tunit, " l=", 
                             min(time$vals[is.finite(time$vals)]),"-", 
                             max(time$vals[is.finite(time$vals)])))
    if (verbose) warning("Cannot determine the time origin!")
    if (verbose) warning("Example format: '15-Dec-1949'")
    if (verbose) warning("NCEP reanalysis typically: 01-01-01")
    if (verbose) warning("ERA-40 typically: 1900-01-01")
    torigin <- readline("Please enter a valid time origin: ")
  }
  
  if (!is.null(torigin)) {
    if (torigin == "1-01-01 00:00:00") {
      if (verbose) print("bad time origin")
      torigin <- "0001-01-01 00:00:00"
      if (verbose) print(paste("Re-setting time origin (",torigin,")",sep=""))
    }
  } else {
    torigin <- readline("Give me the time origin (format='YYYY-MM-DD' as '1949-22-01'):")
    if (!is.null(torigin)) {
      if (verbose) print(paste("Time origin set to =", torigin))
    } else {
      stop("Could not determine the time origin. The processing has been stopped !")
    }
  }
  
  if (!is.null(torigin)) {
    yorigin <- format.Date(as.Date(torigin),format="%Y")
    morigin <- format.Date(as.Date(torigin),format="%m")
    dorigin <- format.Date(as.Date(torigin),format="%d")
    if (as.numeric(yorigin) == 0) {
      if (verbose) warning("There is no year zero (Press et al., Numerical recipies)")
      yorigin <- 0
      if (verbose) print(paste("Warning : Year origin has been set to:",as.character(1900),sep="->"))     
    }
    if (is.na(dorigin)) {
      if (verbose) warning("Warning : Day origin is missing !")
      dorigin <- 1
      if (verbose) warning("Warning : Day origin has been set to:",dorigin)
    }
    if (is.na(morigin)) {
      if (verbose) warning("Warning : Month origin is missing !")
      morigin <- 1
      if (verbose) warning("Warning : Month origin has been set to:",morigin)
    }
    torigin1 <- paste(yorigin,morigin,dorigin,sep="-")
    torigin <- paste(torigin1,unlist(strsplit(torigin,split=" "))[2],sep=" ") 
  }
  
  if (!is.null(torigin)) {
    if (verbose) print("Checking Time Origin --> [ok]")
  } else if (verbose) {
    print("Checking Time Origin --> [fail]")
  }
  
  type <- c("year","season","month","day","hour","minute","second")
  type.abb <- substr(tolower(type),1,3)

  freq.att <- NULL
  ifreq <- grep("freq",names(model))
  if (length(ifreq)>0) {  
    frq <- tolower(eval(parse(text=paste0("model$",names(model)[ifreq]))))
    frq2 <- sub('hr','hou',sub('[[:digit:]]','',frq))
    itype <- grep(frq2, type)
    if (length(itype>0)) {
      freq.att <- type[itype]
      if(grepl('[0-9]',frq) & !grepl('[0-9]',freq.att)) freq.att <- paste0(gsub("[a-z]","",frq), freq.att)
      if(verbose) print(paste0("Frequency has been found in model$frequency attribute (",freq.att,")"))
      if(verbose) print("Checking Frequency from attribute --> [ok]")
    }
  } else {
    if(verbose) print("Checking Frequency from attribute --> [fail]")
    if(verbose) print("Frequency has not been found in the attributes") 
  }
  
  frequency <- freq.data <- NULL
  if (length(time$vals) > 1) {
    freq.data <- datafrequency(data=as.vector(time$vals),unit=tunit,verbose=FALSE) 
  } else {
    freq.data <- 'none'
  }
  if (!is.null(freq.data)) {
    if (verbose) print("Checking Frequency from the data --> [ok]")
  } else {
    if (verbose) print("Checking Frequency from the data --> [fail]")
  }
 
  ical <- grep(c("calend"),tatt)

  if (length(ical)>0) {   
    calendar.att <- eval(parse(text = paste("time$",tatt[ical],sep="")))
    if (verbose) print("Checking Calendar from time attribute --> [ok]") 
    if (verbose) print(paste("Calendar attribute has been found in time$calendar (",time$calendar,")",sep =""))
  } else {
    if (verbose) print("Checking Calendar from time attribute --> [fail]")
    calendar.att <- NULL
    print("Warning : Calendar attribute has not been found in the meta data and will be set automatically.")
  }

  if (!is.null(torigin)) {
    yorigin <- as.numeric(format.Date(torigin,format="%Y"))
    morigin <- as.numeric(format.Date(torigin,format="%m"))
    dorigin <- as.numeric(format.Date(torigin,format="%d"))
    horigin <- as.numeric(format.Date(torigin,format="%H"))
  }
  
  if (!is.null(calendar.att)) {
    if (grepl("gregorian",calendar.att) | grepl("standard",calendar.att)) {
      if (grepl("mon",tunit)) {
        if (sum(round(diff(time$vals)) > 1) < 1) {
          year1 <- time$vals[1]%/%12 + yorigin
          month1 <- morigin
          torigin1 <- paste(as.character(year1),month1,"01",sep="-")
        }
      } 
      time$vdate <- switch(tunit,'seconds'= strptime(torigin,format="%Y-%m-%d %H%M%S") + time$vals,
                           'minutes'= strptime(torigin,format="%Y-%m-%d %H%M%S") + time$vals*60,
                           'hours'= strptime(torigin,format="%Y-%m-%d %H:%M:%S") + time$vals*3600,
                           'days'= as.Date(torigin) + time$vals,
                           'months'= seq(as.Date(torigin1),length.out=length(time$vals),by='month'),
                           'years'= as.numeric(format(as.Date(torigin), "%Y"))  + time$vals)
      
    } else if (!is.na(strtoi(substr(calendar.att, 1, 3))) | grepl("noleap",calendar.att)) {
      if (verbose) print(paste0(substr(calendar.att,1, 3), "-days model year found in calendar attribute"))
      if (grepl("noleap",calendar.att)) {
        time$daysayear <- 365
      } else {
        time$daysayear <- as.numeric(substr(calendar.att, 1, 3))
      }
      if (!is.null(time$daysayear)) {
        if(verbose) print(paste("Creating time$daysayear attribute and setting attribute to", time$daysayear))
        if (time$daysayear==365) {
          mndays <- c(31,28,31,30,31,30,31,31,30,31,30,31) # Number of days in each month
        } else if (time$daysayear==360) {
          mndays <- rep(30,12) # Number of days in each month
        } else {
          if(verbose) print('Warning! unknown calendar type')
        }
        if (!is.null(mndays)) {
          year1 <- time$vals[1]%/%time$daysayear + yorigin
          month1 <- morigin
          if (sum(diff(time$vals)%/%time$daysayear) > 1) {
            if(verbose) print("Warning : Jumps of years has been found in the time series ")
            qf <- c(qf,"jumps of years found in time series")
          }
          if (time$vals[1]%%time$daysayear > 27) {
            year1 <- year1 + 1
            month1 <- month1 + 1
          } 
          if (month1>12) month1 <- month1 - 12 
          months <- ((time$vals%%time$daysayear)%/%round(mean(mndays))) + 1
          years <- time$vals%/%time$daysayear + yorigin
          if(month1>1) mndays <- c(mndays[month1:length(mndays)],mndays[1:(month1-1)])
          days <- time$vals%%time$daysayear - (cumsum(mndays)-mndays)[months] + 1#rep(cumsum(mndays),time$len/12)
          if(freq.data=='month') {
            if ((sum(diff(months) > 1) > 1) | (sum(diff(years) > 1) > 1) | (sum(round(abs(diff(days)))>2)) > 1) {
              print("Warning : Jumps in data have been found !")
              print("Warning: Trust the first date and force a continuous vector of dates !")
              time$vdate <- seq(as.Date(paste(as.character(year1),month1,"01",sep="-")), by = "month",length.out=time$len)
              qf <- c(qf,"jumps in data found - continuous vector forced")
            } else {
              time$vdate <- as.Date(paste(years,months,"01",sep="-"))#"15",sep="-"))
            }
          } else if(freq.data %in% c('season','year')) {
            time$vdate <- as.Date(paste(years,months,"01",sep="-"))
	  }
        }
      }
    }
    if (verbose) print(paste("Starting date : ",time$vdate[1],"Ending date : ",time$vdate[length(time$vdate)], sep = " "))
  } else {
    if (verbose) print("warnings : Automatic detection of the calendar")
    calendar.detect <- "auto"
    if (grepl("sec",tunit)) time$vdate <- as.POSIXct(torigin,tz='UTC') + time$vals
    if (grepl("min",tunit)) time$vdate <- as.POSIXct(torigin,tz='UTC') + time$vals*60
    if (grepl("hou",tunit)) time$vdate <- as.POSIXct(torigin,tz='UTC') + time$vals*60*60
    if (grepl("day",tunit) & median(diff(time$vals))<1) time$vdate <- as.POSIXct(torigin,tz='UTC') + time$vals*60*60*24
    if (grepl("day",tunit) & median(diff(time$vals))>=1) time$vdate <- as.Date((time$vals),origin=as.Date(torigin))
    if (grepl("mon",tunit)) {
      if (sum(diff(time$vals)>1) < 1) {
        year1 <- time$vals[1]%/%12 + yorigin
        month1 <- morigin
        time$vdate <- seq(as.Date(paste(as.character(year1),month1,"01",sep="-")), by = "month",length.out=length(time$vals))
      } else print("Warning : Monthly data are mangeled") 
    } 
  }
  if ((length(time$vdate)>0) & (grepl("mon",tunit)) & (sum(diff(as.numeric(format.Date(time$vdate,"%m")))>1))) {
    if(verbose) stop("Vector date is mangeled! Need extra check!")
  }

  if (!is.null(time$vdate)) {
    dt <- as.numeric(rownames(table(diff(time$vdate))))
  } else {
    dt <- NULL
  }
  if (!is.null(time$vdate)) {
    if (verbose) print("Vector of date is in the form :")
    if (verbose) print(str(time$vdate))
    if (verbose) print(diff(time$vdate))
  } else {
    if (grepl("sec",tunit))
      dt <- as.numeric(rownames(table(diff(ncid$dim$time$vals/(24*60*60)))))
    if (grepl("min",tunit))
      dt <- as.numeric(rownames(table(diff(ncid$dim$time$vals/(24*60)))))
    if (grepl("day",tunit))
      dt <- as.numeric(rownames(table(diff(ncid$dim$time$vals))))
    if (grepl("hou",tunit))
      dt <- as.numeric(rownames(table(diff(ncid$dim$time$vals/24))))
    if (grepl("mon",tunit))
      dt <- as.numeric(rownames(table(diff(ncid$dim$time$vals))))
    if (length(dt)==1) {
      if (verbose) print("Regular frequency has been detected from the data")
    } else if (verbose) print("Irregular frequency has been detected from the data")
    if ((length(dt)==3) & grepl("day",tunit)) {
      if (verbose) print(paste("Calendar is likely to be a 365-",tunit,"with:",as.character(length(dt)),
                               "irregular frequencies",sep = ""))
      dt <- c(28,30,31)
      if (verbose) print(paste(as.character(dt),tunit,sep="-"))
    }
    if ((length(dt)==4) & grepl("day",tunit)) {
      ## 
      if (verbose) print(paste("Calendar is likely to be a Gregorian 365/366-",tunit,"with:",as.character(length(dt)),
                               "irregular frequencies : ",sep=""))
      dt <- c(28,29,30,31)
      if (verbose) print(paste(as.character(dt),tunit,sep="-")) 
    }
    if (!is.null(time$daysayear)) {
      if ((length(dt)==3) & (time$daysayear != 365) & grepl("day",tunit))
        warning("Calendar does not match with detected frequencies")
    }
    if (length(ical)>0)
      if ((length(dt)!=4) & (grepl("gregorian",calendar.att) | grepl("standard",calendar.att)) & grepl("day",tunit))
        warning("Calendar does not match with detected frequencies")
    if ((length(dt)==2) | (length(dt)>4)) {
      if (verbose) print(paste("Warning : Irregular frequencies have been detected - The data might be corrupted and needs extra Checking !"))   
      if (verbose) print(paste(as.character(dt),tunit,sep=" "))
    }
  }

  if (length(time$vals)>1) {
    if (!is.null(freq.att)) {
      model$frequency <- freq.att
      if (!is.null(freq.data)) {
        if (match(freq.att,freq.data,nomatch=FALSE)) {
          if (verbose) print("Frequency found in the attribute matches the frequency detected in data")
          model$frequency <- freq.data <- freq.att
        } else {
          print("Warning : Frequency found in the attribute does not match the frequency detected in data")
          model$frequency <- freq.data
          qf <- c(qf,paste("attribute frequency (",freq.att,") does not match data frequency (",freq.data,")",sep=""))
        }
      } 
    } else if (!is.null(freq.data)) {
      model$frequency <- freq.data
    }
  } else if (sum(is.element(tolower(substr(tunit,1,3)), # REB 2016-03-03
                            c('sec','hou','day','mon','yea')))>0) {
    warning(paste('Need to guess the frequency, based on reckognised time units',tunit))
    model$frequency <- 1
  } else {
    stop("Frequency could not be found, neither detected, the data might be corrupted !")
  }
  if (!is.null(model$frequency)) {
    if (verbose) print(paste("Frequency set to ",model$frequency,sep=""))
    if (model$frequency %in% c("month","season","year")) {
      yr <- as.numeric(format(as.Date(time$vdate),"%Y"))
      mo <- as.numeric(format(as.Date(time$vdate),"%m"))
      dy <- "01"
      time$vdate <- as.Date(paste(yr,mo,dy,sep="-"))
    }
  }

  if (verbose) print("Checking --> [Done!]")
  model$qf <- qf
  result <- list(model=model,time=time)
  invisible(result)

}
