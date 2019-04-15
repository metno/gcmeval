getGPCP <- function(destfile='gpcp.nc',path=NULL,
                  lon=NULL,lat=NULL,force=FALSE,verbose=FALSE) {
  if(verbose) print("getGPCP")
  ## Retrieves the data
  url <- 'https://climexp.knmi.nl/GPCPData/gpcp.nc'
  if(!is.null(path)) destfile <- file.path(path,destfile)
  if (file.exists(destfile) & !force) {
    ncid <- try(ncdf4::nc_open(destfile))
    if (inherits(ncid,"try-error")) force <- TRUE # If downloaded file is incomplete, force new download
    #X <- try(esd::retrieve.default(destfile,lon=lon,lat=lat,verbose=verbose), silent=TRUE)
    #if (inherits(X,"try-error")) force <- TRUE # If downloaded file is incomplete, force new download
  }
  if (!file.exists(destfile) | force) {
    lok <- try(download.file(url=url, destfile=destfile), silent=TRUE)
    if(lok>0) {
      try(file.remove(destfile), silent=TRUE)
      return()
    }
    ncid <- try(ncdf4::nc_open(destfile))
    #X <- try(esd::retrieve.default(destfile,lon=lon,lat=lat,verbose=verbose), silent=TRUE)
  }
  ## Collect information stored in the netCDF header
  cid <- getatt(destfile)
  ## Extract a time series for the area mean for 
  cid$url <- url
  ## Collect information stored as model attributes
  model <- ncdf4::ncatt_get(ncid,0)
  ncid2 <- esd::check.ncdf4(ncid,param=names(cid$var))
  cid$dates <- paste(range(ncid2$time$vdate),collapse=",")
  #cid$dates <- paste(range(zoo::index(X)),collapse=",")
  ncdf4::nc_close(ncid)
  cid$model <- model
  cid$project_id <- model$title
  return(cid)
}