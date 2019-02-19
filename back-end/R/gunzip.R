#Unzip a gz package
gunzip <- function(filename) {
  system.command <- paste("gunzip",filename)
  system(system.command,wait=TRUE)
}
