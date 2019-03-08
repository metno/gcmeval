cdo.regrid <- function(model.file,outfile=NULL,res.lon=2.5,res.lat=2.5,
                       remap="remapcon",bit=8,verbose=FALSE) {
  if(verbose) print("cdo.regrid")
  X <- esd::retrieve.default(model.file)
  lon <- attr(X,"longitude")
  lat <- attr(X,"longitude")
  lon.out <- seq(min(lon),max(lon),res.lon)
  lat.out <- seq(min(lat),max(lat),res.lat)
  grid.out <- file("targetgrid.txt")
  writeLines(c("gridtype = lonlat",
               paste("xsize =",length(lon.out)),
               paste("ysize =",length(lon.out)),
               paste("xfirst =",min(lon.out)),
               paste("xinc =",res.lon),
               paste("yfirst =",min(lat.out)),
               paste("yinc =",res.lat)), grid.out)
  cdo.command(remap, "targetgrid.txt", model.file, outfile, bit=bit)
}

# remapbil, remapcon, remapcon2