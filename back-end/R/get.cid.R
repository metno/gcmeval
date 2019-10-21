## Generic function to retrieve climate model (CM) file from the KNMI ClimateExplorer
get.cid <- function(file,path=NULL,verbose=FALSE) {
  if(verbose) print("get.cid")
  ## Retrieves the data
  if(!is.null(path)) file <- file.path(path,file)
  if (!file.exists(file)) {
    print(paste("File",file,"doesn't exist. Check filename and path."))
    cid <- NULL
  } else {
    ## Collect information stored in the netCDF header
    cid <- getatt(file)
    ## Extract a time series for the area mean for 
    cid$url <- url
    ## Collect information stored as model attributes
    ncid <- ncdf4::nc_open(file)
    ncid2 <- check.ncdf4(ncid,param=names(cid$var))
    cid$dates <- paste(range(ncid2$time$vdate),collapse=",")
    model <- ncdf4::ncatt_get(ncid,0)
    ncdf4::nc_close(ncid)
    cid$model <- model
    cid$project_id <- cid$model$project_id
    if(is.null(cid$project_id) & !is.null(cid$model$title)) {
      if(grepl("CMIP5",toupper(model$title))) {
        cid$project_id <- "CMIP5"
      } else if(grepl("CMIP6",toupper(model$title))) {
        cid$project_id <- "CMIP6"
      }
    }
  }
  return(cid)
}