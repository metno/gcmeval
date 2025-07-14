#Search and read a shapefile
get.shapefile <- function(filename,path=NULL,verbose=FALSE) {
  if(verbose) print("get.shapefile")
  if(filename!=basename(filename) & is.null(path)) {
    path <- dirname(filename)
    filename <- basename(filename)
  }
  fullname <- find.file(filename, path=path)[1]
  shape <- sf::st_read(fullname)
  invisible(shape)
}
