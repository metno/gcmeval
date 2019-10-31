## Generic function to retrieve climate model (CM) file from the KNMI ClimateExplorer
getCM <- function(url=NULL,destfile='CM.nc',path=NULL,
                  lon=NULL,lat=NULL,force=FALSE,verbose=FALSE) {
  if(verbose) print("getCM")
  if(!is.null(path)) destfile <- file.path(path,destfile)
  if (file.exists(destfile) & !force) {
    if(verbose) print(paste("File",destfile,"exists. Check file."))
    ncid <- ncdf4::nc_open(destfile)
    if (inherits(ncid,"try-error")) force <- TRUE # If downloaded file is incomplete, force new download
  }
  if (!file.exists(destfile) | force) {
    if(is.null(url)) url <- 'https://climexp.knmi.nl/CMIP5/monthly/tas/tas_Amon_ACCESS1-0_historical_000.nc'
    if(verbose) print(paste("Download file from",url))
    lok <- try(download.file(url=url, destfile=destfile), silent=TRUE)
    if(lok>0) {
      try(file.remove(destfile), silent=TRUE)
      warning(paste("Failed to download file from url",url))
      return()
    }
    ncid <- ncdf4::nc_open(destfile)
  }
  cid <- getncid(destfile)
  if(!is.null(url)) cid$url <- url
  return(cid)
}