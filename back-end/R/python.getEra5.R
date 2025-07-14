# Call to a python script which downloads data from the public ECMWF data server
python.getEra5 <- function(start,end,varid,type,stream,outfile,
                           python="python",verbose=FALSE) {
  if(verbose) print("python.getEra5")
  print(paste("Warning! The function python.getEra5 is deprecated and",
        "has not yet been updated to the new cdsapi.",
	"ERA5 data can be downloaded from ",
	"https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=download"))
  ## KMP 2025-07-14: This function does not work anymore!
  ## The python file needs to changed to work with the new cdsapi 
  #path.era <- find.file("getMonthlyERA5_new.py")
  path.era <- find.file("getMonthlyERA5.py")
  script <- paste(python,path.era[1])
  path <- dirname(outfile)
  if(path==".") path <- getwd()
  dates <- strftime(seq(as.Date(paste0(start,"-01-01")),
                    as.Date(paste0(end,"-12-01")),by="month"),
                    format="%Y%m%d")
  if(verbose) print("Run CDS Python script for downloading ERA5 data")
  #browser()
  system.command <- paste(script," -f ",start," -l ",end,
                          " -v ",varid," -t ",type," -r ",stream,
                          " -o ",outfile,sep="")
  system(system.command)
  # Remove old files:
  #system.command <- paste0("rm ","era5_",stream,"_",variable,"_",type,"_*.grib")
  #system(system.command)
  if(verbose) print("--- end python.getEra5")
}
