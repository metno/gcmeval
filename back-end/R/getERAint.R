#Retrieve monthly data for 2m temperature and precipitation from the ECMWF public repository.
#The use of this function requires that the ECMWF key and python libraries are installed on the machine.
#See instructions in https://software.ecmwf.int/wiki/display/WEBAPI/Access+ECMWF+Public+Datasets
#The function also requires that cdo is installed on the operating computer.
getERAint <- function(variable,start=1979,end=2017,griddes="cmip_1.25deg_to_2.5deg.txt",
                   destfile=NULL,force=FALSE,verbose=FALSE){
  if(verbose) print("getERAint")
  griddes <- find.file(griddes)[1]
  if(any(match(c("tas","tmp","temp","temperature","t2m"),variable,nomatch=0))) {
    if(verbose) print("variable: temperature")
    varID <- "167.128"
    stream <- "moda"
    steps <- "0"
    type <- "an"
    commands <- c("-f","nc","-copy","-remapcon","-chname")
    input <- c("","","",griddes,"2t,tas")
  } else if(any(match(c("pre","prc","prec","precipitation","pr"),variable,nomatch=0))) {
    if(verbose) print("variable: precipitation")
    varID <- "228.128"
    stream <- "mdfa"
    # Step 0-12 is recommended for ERAinterim precipitation. Spin-up effect should not be an issue with ERAinterim forecasts.
    steps <- "0-12"
    type <- "fc"
    commands <- c("-f","nc","-copy","-monmean","-remapcon","-chname")#"-monsum","-remapcon","-chname")
    input <- c("","","","",griddes,"2t,tas")
  }
  if(is.null(destfile)) destfile <- paste("era-interim_monthly_",paste(start,end,sep="-"),"_",variable,".grib",sep="")
  outfile <- paste(gsub('.{5}$', '',destfile),"2.5deg",'nc',sep=".")
  if(!file.exists(outfile)|force) {
    if(verbose) print("NetCDF file with 2.5deg data does not exist.")
    if(!file.exists(destfile)) {
      if(verbose) print("GRIB file does not exist. Download with ECMWF Python tool.")
      python.getEraint(start, end, varID, steps, type, stream, destfile, verbose=verbose)
    }
    if(!file.exists(destfile)) {
      print(paste("Warning! File",destfile,"failed to download. Mak sure that you have installed the ECMWF API key and client.",
                  "See https://software.ecmwf.int/wiki/display/WEBAPI/Access+ECMWF+Public+Datasets for further instructions."))
      return()
    }
    if(verbose) print("Regrid with CDO and save as netCDF.")
    cdo.command(commands,input,destfile,outfile)
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
  if(verbose) print("--- end getERAint ---")
  invisible(cid)
}
