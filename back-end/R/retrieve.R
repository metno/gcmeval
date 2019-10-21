retrieve <- function(ncfile,param="auto",path=NULL,verbose=FALSE,...) {
  if (verbose) print('retrieve')
  X <- NULL
  qf <- NULL
  test <- NULL
  
  nc <- ncdf4::nc_open(ncfile)
  dimnames <- names(nc$dim)
  ilon <- tolower(dimnames) %in% c("x","i") | grepl("lon",tolower(dimnames))
  ilat <- tolower(dimnames) %in% c("y","j") | grepl("lat",tolower(dimnames))
  if(any(ilon) & any(ilat)) {
    lon <- ncdf4::ncvar_get(nc,dimnames[ilon])
    lat <- ncdf4::ncvar_get(nc,dimnames[ilat])
    ncdf4::nc_close(nc)
  } else {
    lon <- NULL
    lat <- NULL
  }
  if ( (length(dim(lon))==1) & (length(dim(lat))==1) )  {
    if (verbose) print(paste('Regular grid field found',ncfile))
    X <- retrieve.ncdf4(ncfile,path=path,param=param,verbose=verbose,...)
  } else {
    if (verbose) print('Irregular grid field found')
    X <- retrieve.rcm(ncfile,path=path,param=param,verbose=verbose,...)
  }
}