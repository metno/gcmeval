#apply mask to a zoo object by setting values outside the mask to NA
mask.zoo <- function(zoo.object,mask) {
  mask <- raster::flip(mask,direction='y')
  zoo.object[,which(is.na(raster::getValues(mask)))] <- NA
  return(zoo.object)
}
