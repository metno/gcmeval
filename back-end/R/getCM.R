## Generic function to retrieve climate model (CM) file from the KNMI ClimateExplorer
getCM <- function(url=NULL,destfile='CM.nc',path=NULL,
                  lon=NULL,lat=NULL,force=FALSE,verbose=FALSE) {
  if(verbose) print("getCM")
  ## Retrieves the data
  if(is.null(url)) url <-
      'https://climexp.knmi.nl/CMIP5/monthly/tas/tas_Amon_ACCESS1-0_historical_000.nc'
  if(!is.null(path)) destfile <- file.path(path,destfile)
  if (file.exists(destfile) & !force) {
    if(verbose) print(paste("File",destfile,"exists. Check file."))
    X <- try(esd::retrieve.default(destfile,lon=lon,lat=lat,verbose=FALSE), silent=TRUE)
    if (inherits(X,"try-error")) force <- TRUE # If downloaded file is incomplete, force new download
  }
  if (!file.exists(destfile) | force) {
    if(verbose) print(paste("Download file from",url))
    lok <- try(download.file(url=url, destfile=destfile), silent=TRUE)
    if(lok>0) {
      try(file.remove(destfile), silent=TRUE)
      return()
    }
    X <- try(esd::retrieve.default(destfile,lon=lon,lat=lat,verbose=FALSE), silent=TRUE)
  }
  ## Collect information stored in the netCDF header
  cid <- getatt(destfile)
  ## Extract a time series for the area mean for 
  cid$url <- url
  #cid$dates <- paste(range(zoo::index(X)),collapse=",")
  ## Collect information stored as model attributes
  ncid <- ncdf4::nc_open(destfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncdf4::nc_close(ncid)
  cid$model <- model
  cid$project_id <- cid$model$project_id
  ## KMP 2017-11-29: One of the CMIP5 files (GCM36.tas.nc) is missing project_id in netCDF header:
  if(is.null(cid$project_id) & !is.null(cid$model$title)) {
    if(grepl("CMIP5",model$title)) cid$project_id <- "CMIP5"
  }
  return(cid)
}