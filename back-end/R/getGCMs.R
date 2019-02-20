## Specific function to retrieve GCMs
getGCMs <- function(select=1:9,varid='tas',experiment='rcp45',
                    destfile=NULL,path=NULL,force=FALSE,
                    verbose=FALSE) {
  if(verbose) print("getGCMs")
  ## Set destfile
  if(is.null(destfile)) destfile <- paste(rep('GCM',length(select)),select,'.',varid,'.',experiment,'.nc',sep='')
  if(!is.null(path)) destfile <- file.path(path,destfile)
  ## Get the urls
  url <- cmip5.urls(varid=varid,experiment=experiment)[select] ## Get the URLs of the 
  ## Set up a list variable to contain all the metadata in sub-lists.
  X <- list()
  for (i in seq_along(select)) {
    if(verbose) print(paste("Get gcm.",select[i],sep=''))
    xi <- getCM(url=url[i],destfile=destfile[i],force=force,verbose=verbose)
    if(!is.null(xi)) {
      X[[paste('gcm',varid,select[i],sep='.')]] <- xi
    } else{
      if(verbose) print(paste("Failed to download gcm.",select[i]))
    }
  }
  invisible(X)
}
