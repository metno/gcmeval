## R-script that downloads data from the Copernicus Climate Data Store (CDS)
# You have to install python and install the CDS API key and client:
# https://cds.climate.copernicus.eu/api-how-to

getERA5 <- function(variable, start=1979, end=2018,
                    gridfile="cmip_1.25deg_to_2.5deg.txt",
                    path.gridfile=NULL, destfile=NULL, path=NULL,
                    force=FALSE, python="python3",verbose=FALSE){
  if(verbose) print("getERA5")
  if(!file.exists(gridfile)) gridfile <- find.file(gridfile, path=path.gridfile)[1]
  if(any(match(c("tas","tmp","temp","temperature","t2m"),variable,nomatch=0))) {
    if(verbose) print("variable: temperature")
    varID <- "167.128"
    stream <- "moda"
    type <- "an"
    cmd <- c("-chname","-remapcon")
    input <- c("2t,tas",gridfile)
  } else if(any(match(c("pre","prc","prec","precipitation","pr"),variable,nomatch=0))) {
    if(verbose) print("variable: precipitation")
    varID <- "228.128"
    stream <- "moda"
    type <- "fc"
    cmd <- c("-chname","-remapcon")
    input <- c("tp,pr",gridfile)
  }
  if(is.null(destfile)) destfile <- paste0("era5_monthly_",paste(start,end,sep="-"),"_",variable,".nc")
  outfile <- gsub('.nc$','.2.5deg.nc',destfile)
  if(!is.null(path)) outfile <- file.path(path,outfile)
  if(!file.exists(outfile)|force) {
    if(verbose) print("NetCDF file with 2.5deg data does not exist.")
    if(!is.null(path)) destfile <- file.path(path,destfile)
    if(!file.exists(destfile)) {
      if(verbose) print("NetCDF file does not exist. Download with cdsapi Python tool.")
      python.getEra5(start, end, varID, type, stream, destfile, 
                     python=python, verbose=verbose)
      if(!file.exists(destfile)) {
        warning(paste("Warning! File",destfile,"failed to download.",
                    "Make sure that you have installed the CDS API key and client.",
	                   "See https://cds.climate.copernicus.eu/api-how-to for further instructions."))
        return()
      }
    }
    if(verbose) print("Regrid with CDO and save as netCDF.")
    cdo.command(cmd,input,destfile,outfile)
  }
  if(verbose) print("Retrieve data from netCDF file.")
  cid <- getatt(outfile)
  if(verbose) print("Calculate area mean and sd.")
  cid$url <- NA
  if(verbose) print("Get information about the model and netCDF file.")
  ncid <- ncdf4::nc_open(outfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncid2 <- check.ncdf4(ncid,param=names(cid$var))
  cid$dates <- paste(range(ncid2$time$vdate),collapse=",")
  ncdf4::nc_close(ncid)
  cid$model <- model
  cid$project_id <- cid$model$project_id
  if(verbose) print("--- end getERA5 ---")
  invisible(cid)
}
