cdo.regrid <- function(model.file, outfile=NULL,
                       lon.out=NULL, lat.out=NULL,
                       res.lon=NULL, res.lat=NULL,
                       remap="remapcon",bit=8,verbose=FALSE) {
  if(verbose) print("cdo.regrid")
  if(is.null(lon.out)|is.null(lat.out)) X <- retrieve(model.file)
  if(is.null(lon.out)) {
    if(is.null(res.lon)) res.lon <- 2.5
    lon <- attr(X,"longitude")
    lon.out <- seq(min(lon),max(lon),res.lon)
  } else if(is.null(res.lon)) {
    res.lon <- diff(lon.out)[1]
  }
  if(is.null(lat.out)) {
    if(is.null(res.lat)) res.lat <- 2.5
    lat <- attr(X,"longitude")
    lat.out <- seq(min(lat),max(lat),res.lat)
  } else if(is.null(res.lat)) {
    res.lat <- diff(lat.out)[1]
  }
  grid.out <- file("targetgrid.txt")
  writeLines(c("gridtype = lonlat",
               paste("xsize =",length(lon.out)),
               paste("ysize =",length(lat.out)),
               paste("xfirst =",min(lon.out)),
               paste("xinc =",res.lon),
               paste("yfirst =",min(lat.out)),
               paste("yinc =",res.lat)), grid.out)
  cdo.command(remap, "targetgrid.txt", model.file, outfile, bit=bit)
}