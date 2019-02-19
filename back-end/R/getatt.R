getatt <- function(fname) {
  ## Reads and extracts the attribute information in a netCDF files and stores this in a list object## 
  ## This is part of the information stored in the metadatabase
  ncid <- ncdf4::nc_open(fname)
  ncdf4::nc_close(ncid)
  return(ncid)
}
