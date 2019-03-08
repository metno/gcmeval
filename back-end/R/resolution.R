resolution <- function(filename,dim=c("lon","longitude"), verbose=FALSE) {
  if(verbose) print("resolution")
  nc <- ncdf4::nc_open(filename)
  dimnames <- names(nc$dim)
  idim <- tolower(dimnames) %in% tolower(dim)
  if(sum(idim)==0) {
    print(paste("Warning! No dimension matching",paste(dim,collapse=", "),"was found"))
    res <- NULL
  } else if(sum(idim)>1) {
    res <- c()
    for(i in seq_along(idim)) {
      x <- ncdf4::ncvar_get(nc,dimnames[idim[i]])
      res[i] <- diff(x)[1]
    }
  } else {
    x <- ncdf4::ncvar_get(nc,dimnames[idim])
    res <- diff(x)[1]
  }
  ncdf4::nc_close(nc)
  return(res)
}
