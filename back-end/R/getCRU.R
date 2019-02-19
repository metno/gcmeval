#A test function to retrieve CRU data from CEDA databases.
getCRU <- function(username,passwd,variable="tmp",version="4.00",
                   griddes="cmip_1.25deg_to_2.5deg.txt",destfile=NULL,
                   time.resol=NULL, verbose=FALSE) {
  if(verbose) print("getCRU")
  if(any(match(c("tas","tmp","temp","temperature","t2m"),variable))){
    variable <- "tmp"
  } else if(any(match(c("pre","prc","prec","precipitation","pr"),variable))){
    variable <- "pre"
  }
  cert <- paste(username,passwd,sep=":")
  url <- paste("ftp.ceda.ac.uk/badc/cru/data/cru_ts",paste("cru_ts",version,sep="_"),"data",variable,sep="/")
  if(is.null(destfile)) destfile <- paste(paste("cru_ts",version,sep=""),"1901.2015",variable,"dat.nc.gz",sep=".")
  if(!file.exists(destfile)) try(download.file(url=paste(paste("ftp://",paste(cert,url,sep="@"),sep=""),destfile,sep="/"),
                                              destfile=destfile, mode="wb"), silent=TRUE)
  gunzip(destfile)
  destfile <- paste(gsub('.{5}$', '',destfile),"nc",sep="")
  outfile <- paste(gsub('.{5}$', '',destfile),"2.5deg.",'nc',sep="")
  if(!file.exists(outfile)){
    griddes <- find.file(griddes)
    commands <- c("-f","nc","-copy","-remapcon")
    input <- c("","","",griddes)
    cdo.command(commands,input,destfile,outfile) 
  }
  X <- esd::retrieve(outfile)
  cid <- getatt(outfile)
  cid$area.mean <- esd::aggregate.area(X,FUN='mean')
  cid$area.sd <- esd::aggregate.area(X,FUN='sd')
  cid$url <- NA
  cid$dates <- paste(range(zoo::index(X)),collapse=",")
  ncid <- ncdf4::nc_open(outfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncdf4::nc_close(ncid)
  cid$model <- model
  cid$project_id <- cid$model$project_id
  return(cid)
}
