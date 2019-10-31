getatt <- function(filename) {
  ## Reads and extracts the attribute information in a netCDF files and stores this in a list object## 
  ## This is part of the information stored in the metadatabase
  ncid <- ncdf4::nc_open(filename)
  ncdf4::nc_close(ncid)
  return(ncid)
}
