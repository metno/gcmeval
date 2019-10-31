# Get daily EOBS data and convert it to monthly averages. Version and resolution
# selection not implemented yet.
getEOBS <- function(variable="tas", destfile=NULL, path=NULL, 
                    version="17",#resolution="0.50", 
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
  if(is.null(destfile)) destfile <- paste(sub("\\.[[:alnum:]]+$", "", 
                                          sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE), perl=TRUE),
                                          "mon.nc",sep="_")
  if(!is.null(path)) destfile <- file.path(path,destfile)
  if(!file.exists(destfile)) {
    f <- filename
    if(!is.null(path)) filename <- file.path(path,filename)
    if(!file.exists(filename)) download.file(paste(url.path,f,sep="/"),destfile=filename)
    gunzip(filename)
    if(verbose) print(destfile)
    commands <- c("-f","nc","-copy","-monavg")
    input <- c("","","","")
    cdo.command(commands,input,infile=filename,outfile=destfile)
  }
  cid <- getatt(destfile) 
  cid$url <- paste(url.path,f,sep="/")
  ncid <- ncdf4::nc_open(destfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncid2 <- check.ncdf4(ncid,param=names(cid$var))
  cid$dates <- paste(range(ncid2$time$vdate),collapse=",")
  ncdf4::nc_close(ncid)
  cid$model <- model
  invisible(cid)
}
