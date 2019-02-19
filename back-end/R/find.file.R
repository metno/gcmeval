#Helper function to find files linux environment (add Windows counterpart)
find.file <- function(filename) {
  command <- paste("find -L $HOME -name",filename,sep=" ")
  fullpath <- system(command,intern=TRUE)
  if(length(fullpath)==0) return(FALSE)
  return(fullpath)
}