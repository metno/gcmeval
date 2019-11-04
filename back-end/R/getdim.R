getdim <- function(filename, dim=c("lon","longitude"), verbose=FALSE) {
  if(verbose) print("resolution")
  nc <- ncdf4::nc_open(filename)
  dimnames <- names(nc$dim)
  idim <- tolower(dimnames) %in% tolower(dim)
  if(sum(idim)==0) {
    print(paste("Warning! No dimension matching",paste(dim,collapse=", "),"was found"))
    dim <- NULL
  } else if(sum(idim)>1) {
    dim <- list()
    for(i in seq_along(idim)) {
      x <- ncdf4::ncvar_get(nc,dimnames[idim[i]])
      dim[i] <- x
    }
  } else {
    dim <- ncdf4::ncvar_get(nc,dimnames[idim])
  }
  ncdf4::nc_close(nc)
  return(dim)
}
