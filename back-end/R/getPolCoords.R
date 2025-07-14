# Transform the coordinates of a region from Cartesian to polar. 
getPolCoords <- function(region,shape=NULL,destfile="coords.txt"){
  if(is.null(shape)) shape <- get.shapefile("referenceRegions.shp")
  if(is.character(region)) region <- which(as.character(shape$LAB)==region)
  shape_sp <- sf::as_Spatial(shape)
  if(is.numeric(region)) {
    polygon <- shape_sp@polygons[[region]]@Polygons[[1]]
  } else if(is.character(region)) {
    polygon <- shape_sp@polygons[[which(shape$LAB==region)]]@Polygons[[1]]
  }
  pol.coords <- sp::coordinates(polygon)
  write(t(pol.coords),file=destfile,ncolumns = 2)
}