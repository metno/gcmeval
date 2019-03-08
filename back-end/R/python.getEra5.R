# Call to a python script which downloads data from the public ECMWF data server
python.getEra5 <- function(start,end,varid,type,stream,outfile,python="python",
                           verbose=FALSE) {
  if(verbose) print("python.getEra5")
  path.era <- find.file("getMonthlyERA5.py")
  script <- paste(python,path.era[1])
  path <- dirname(outfile)
  if(path==".") path <- getwd()
  dates <- strftime(seq(as.Date(paste0(start,"-01-01")),
                    as.Date(paste0(end,"-12-01")),by="month"),
                    format="%Y%m%d")
  if(verbose) print("Run ECMWF Python script for downloading ERA data")
  for(year in seq(start,end)) {
    for(month in seq(1,12)) {
      mn <- if(month<10) mn <- paste0("0",month) else mn <- month
      out <- file.path(path,paste("era5",varid,type,stream,
                      paste0(year,mn,"01"),"grib",sep="."))
      if(!file.exists(out)) {
        browser()
        system.command <- paste(script," -y ",year," -m ",month," -v ",
                                varid," -t ",type," -r ",stream,
                                " -o ",out, sep="")
        system(system.command, wait=TRUE)
      }
    }
  }
  browser()
  system.command <- paste0("cdo mergetime ",
    paste("era5",varid,type,stream,"*","grib",sep="."))
  #system.command <- paste(script," -f ",start," -l ",end," -v ",varid,
  #                        " -t ",type," -r ",stream," -o ",outfile, sep="")
  system(system.command)
  system.command <- paste0("rm ",gsub("[0-9]{8}","*",out))
  if(verbose) print("--- end python.getEra5")
}
