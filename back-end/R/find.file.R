## Helper function to find files linux environment (add Windows counterpart)
find.file <- function(filename, path=NULL) {
  if(file.exists(filename)) {
    if(dirname(filename)==".") path <- getwd() else path <- dirname(filename)
    fullpath <- file.path(path, basename(filename))
  } else {
    #browser()
    if(!is.null(path)) fullpath <- system(paste("find -L", path, "-name", filename), intern=TRUE) else
      fullpath <- system(paste("find -L $HOME -name", filename))[1]
  }
  if(length(fullpath)==0) return(FALSE) else return(fullpath)
}