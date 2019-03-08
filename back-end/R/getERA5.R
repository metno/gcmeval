## R-script that downloads data from the Copernicus Climate Data Store (CDS)
# You have to install python and download CDS API:
# https://cds.climate.copernicus.eu/api-how-to

getERA5 <- function(variable,start=1979,end=2018,griddes="cmip_1.25deg_to_2.5deg.txt",
                   destfile=NULL,force=FALSE,python="python",verbose=FALSE){
  if(verbose) print("getERA5")
  griddes <- find.file(griddes)[1]
  if(any(match(c("tas","tmp","temp","temperature","t2m"),variable,nomatch=0))) {
    if(verbose) print("variable: temperature")
    varID <- "167.128"
    stream <- "moda"
    type <- "an"
    commands <- c("-f","nc","-copy","-remapcon","-chname")
    input <- c("","","",griddes,"2t,tas")
  } else if(any(match(c("pre","prc","prec","precipitation","pr"),variable,nomatch=0))) {
    if(verbose) print("variable: precipitation")
    varID <- "228.128"
    stream <- "moda"
    type <- "an"#"fc"
    commands <- c("-f","nc","-copy","-monmean","-remapcon","-chname")#"-monsum","-remapcon","-chname")
    input <- c("","","","",griddes,"2t,tas")
  }
  if(is.null(destfile)) destfile <- paste0("era5_monthly_",paste(start,end,sep="-"),"_",variable,".grib")
  outfile <- paste(gsub('.{5}$', '',destfile),"2.5deg",'nc',sep=".")
  if(!file.exists(outfile)|force) {
    if(verbose) print("NetCDF file with 2.5deg data does not exist.")
    if(!file.exists(destfile)) {
      if(verbose) print("GRIB file does not exist. Download with cdsapi Python tool.")
      python.getEra5(start, end, varID, type, stream, destfile, 
                     python=python, verbose=verbose)
    }
    if(verbose) print("Regrid with CDO and save as netCDF.")
    cdo.command(commands,input,destfile,outfile)
  }
  if(verbose) print("Retrieve data from netCDF file.")
  X <- esd::retrieve(outfile)
  cid <- getatt(outfile)
  if(verbose) print("Calculate area mean and sd.")
  #cid$area.mean <- esd::aggregate.area(X,FUN='mean')
  #cid$area.sd <- esd::aggregate.area(X,FUN='sd')
  cid$url <- NA
  cid$dates <- paste(range(zoo::index(X)),collapse=",")
  if(verbose) print("Get information about the model and netCDF file.")
  ncid <- ncdf4::nc_open(outfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncdf4::nc_close(ncid)
  cid$model <- model
  cid$project_id <- cid$model$project_id
  cid$srex <- get.srex.region(outfile,region=NULL,print.srex=FALSE,verbose=FALSE)
  if(verbose) print("--- end getERA5 ---")
  return(cid)
}
