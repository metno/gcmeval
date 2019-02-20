#Apply a set of cdo commands on a grib/netcdf file. Several commands can be piped.
cdo.command <- function(commands,input,infile,outfile,bit=8,intern=FALSE) {
  cdo.coms <- array()
  separators <- array(" ",dim=length(commands))
  separators[which(is.na(match(input,"")))] <- ","
  for(i in 1:length(separators)){
    cdo.coms[i]  <- paste(commands[i],input[i],sep=separators[i])
  }
  system.command <- paste("cdo -b",bit,paste(cdo.coms,collapse=" "),infile,outfile,sep=" ")
  if(intern) {
    output <- system(system.command,wait=TRUE,intern=TRUE)
    return(output)
  } else {
    system(system.command,wait=TRUE)
  }
}