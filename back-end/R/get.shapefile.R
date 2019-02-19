#Search and read a shapefile
get.shapefile <- function(filename,with.path=FALSE){
  fullname <- filename
  if(!with.path){
    fullname <- find.file(filename)[1]
  }
  rgdal::readOGR(fullname,verbose=FALSE)
}
