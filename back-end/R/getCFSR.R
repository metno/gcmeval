#Get monthly CFSR data and interpolate it to common 2.5 degree grid.
getCFSR <- function(variable="tas",destfile=NULL,lon=NULL,lat=NULL,
                    griddes="cmip_1.25deg_to_2.5deg.txt",verbose=FALSE) {
  if(verbose) print("getCFSR")
  url.path <- "http://climexp.knmi.nl/CFSR"
  griddes <- find.file(griddes)
  if(variable=="tas"){
    filename <- "cfsr_tmp2m.nc"
    commands <- c("-f","nc","-copy","-remapcon","-monavg","-chname")
    input <- c("","","",griddes,"","TMP_2maboveground,tas")
  } else if(variable=="pr"){
    filename <- "cfsr_prate.nc"
    commands <- c("-f","nc","-copy","-remapcon","-monavg","-chname")
    input <- c("","","",griddes,"","PRATE_surface,pr")
  }
  if(!file.exists(filename)) download.file(paste(url.path,filename,sep="/"),destfile=filename)
  if(is.null(destfile)) destfile <- paste(sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE),"mon.nc",sep="_")
  if(!file.exists(destfile)) cdo.command(commands,input, filename,destfile)
  X <- esd::retrieve(destfile,lon=lon,lat=lat,verbose=verbose)
  cid <- getatt(destfile) 
  cid$url <- paste(url.path,filename,sep="/")
  cid$area.mean <- esd::aggregate.area(X,FUN='mean',na.rm=TRUE)
  cid$area.sd <- esd::aggregate.area(X,FUN='sd',na.rm=TRUE)
  ncid <- ncdf4::nc_open(destfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncdf4::nc_close(ncid)
  cid$model <- model
  cid$srex <- get.srex.region(destfile,region=NULL,print.srex=FALSE,verbose=FALSE)
  return(cid)
}
