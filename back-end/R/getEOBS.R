# Get daily EOBS data and convert it to monthly averages. Version and resolution
# selection not implemented yet.
getEOBS <- function(variable="tas", destfile=NULL, version="17",#resolution="0.50", 
                    lon=NULL, lat=NULL, verbose=FALSE) {
  if(verbose) print("getEOBS")
  url.path <- "http://www.ecad.eu/download/ensembles/data/Grid_0.50deg_reg"
  if(is.numeric(version)) version <- as.character(version)
  if(variable=="tas") {
    filename <- paste("tg_0.50deg_reg_v",version,".0.nc.gz",sep="")
  } else if(variable=="pr") {
    filename <- paste("rr_0.50deg_reg_v",version,".0.nc.gz",sep="")
  } else{
    return("Not implemented yet!")
  }
  if(!file.exists(filename)) download.file(paste(url.path,filename,sep="/"),destfile=filename)
  gunzip(filename)
  filename <- sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE)
  if(is.null(destfile)) destfile <- paste(sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE),"mon.nc",sep="_")
  #if(is.null(destfile)) destfile <- file.path(system("echo $EXTERNAL_DATA",intern=T),
  #                                            paste(sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE),"mon.nc",sep="_"))
  if(verbose) print(destfile)
  commands <- c("-f","nc","-copy","-monavg")
  input <- c("","","","")
  if(!file.exists(destfile)) cdo.command(commands,input,infile=filename,outfile=destfile)
  X <- esd::retrieve.default(destfile,lon=lon,lat=lat,verbose=verbose)
  cid <- getatt(destfile) 
  cid$url <- paste(url.path,filename,sep="/")
  cdo.command("-fldmean","",destfile,"tmp.nc")
  out <- as.numeric(cdo.command("output","","tmp.nc",NULL,intern=TRUE))
  cid$area.mean <- zoo::zoo(out, order.by=zoo::index(X))
  cdo.command("-fldstd","",destfile,"tmp.nc")
  out <- as.numeric(cdo.command("output","","tmp.nc",NULL,intern=TRUE))
  cid$area.sd <- zoo::zoo(out, order.by=zoo::index(X))
  system("rm tmp.nc")
  #cid$area.mean <- esd::aggregate.area(X,FUN='mean',na.rm=TRUE)
  #cid$area.sd <- esd::aggregate.area(X,FUN='sd',na.rm=TRUE)
  ncid <- ncdf4::nc_open(destfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncdf4::nc_close(ncid)
  cid$model <- model
  return(cid)
}
