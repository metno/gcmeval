# Transform the coordinates of a region from Cartesian to polar. 
getPolCoords <- function(region,shape=NULL,destfile="coords.txt"){
  if(is.null(shape)) shape <- get.shapefile("referenceRegions.shp")
  if(is.character(region)) region <- which(as.character(shape$LAB)==region)
  pol.coords <- coordinates(shape@polygons[[region]]@Polygons[[1]])
  write(t(pol.coords),file=destfile,ncolumns = 2)
}