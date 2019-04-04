getGPCP <- function(destfile='gpcp.nc',path=NULL,
                  lon=NULL,lat=NULL,force=FALSE,verbose=FALSE) {
  if(verbose) print("getGPCP")
  ## Retrieves the data
  url <- 'https://climexp.knmi.nl/GPCPData/gpcp.nc'
  if(!is.null(path)) destfile <- file.path(path,destfile)
  if (file.exists(destfile) & !force) {
    X <- try(esd::retrieve.default(destfile,lon=lon,lat=lat,verbose=verbose), silent=TRUE)
    if (inherits(X,"try-error")) force <- TRUE # If downloaded file is incomplete, force new download
  }
  if (!file.exists(destfile) | force) {
    lok <- try(download.file(url=url, destfile=destfile), silent=TRUE)
    if(lok>0) {
      try(file.remove(destfile), silent=TRUE)
      return()
    }
    X <- try(esd::retrieve.default(destfile,lon=lon,lat=lat,verbose=verbose), silent=TRUE)
  }
  ## Collect information stored in the netCDF header
  cid <- getatt(destfile)
  ## Extract a time series for the area mean for 
  cid$url <- url
  cid$dates <- paste(range(zoo::index(X)),collapse=",")
  ## Collect information stored as model attributes
  ncid <- ncdf4::nc_open(destfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncdf4::nc_close(ncid)
  cid$model <- model
  cid$project_id <- model$title
  return(cid)
}