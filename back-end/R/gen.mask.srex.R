# Create a raster mask for the selected SREX sub-region from the CMIP5 netcdf file.
gen.mask.srex <- function(destfile, mask.polygon=NULL, ind=FALSE, inverse=FALSE, 
                          mask.values=1, verbose=FALSE) {
  if(verbose) print("gen.mask.srex")
  if(verbose) print(destfile)
  r <- raster::raster(destfile)
  r <- raster::setValues(r,NA)
  extent.r <- raster::extent(r)
  if(extent.r[2]==360) raster::extent(r) <- c(-180,180,-90,90)
  indices <- raster::extract(r,mask.polygon,cellnumbers=TRUE)[[1]][,1]
  if(raster::extent(mask.polygon)[2]>180){
    raster::extent(r) <- c(180,540,-90,90)
    indices <- sort(c(indices,raster::extract(r,mask.polygon,cellnumbers=TRUE)[[1]][,1]))
  }
  if(inverse){
    tmp <- seq(1,length(raster::getValues(r)))
    indices <- tmp[which(is.na(match(tmp,indices)))]
  }
  mask.raster <- r
  raster::extent(mask.raster) <- c(0,360,-90,90)
  mask.raster[indices] <- mask.values
  if(ind) return(indices)
  return(mask.raster)
}
