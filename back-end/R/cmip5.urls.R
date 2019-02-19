cmip5.urls <- function(experiment='rcp45',varid='tas',
                       url="http://climexp.knmi.nl/CMIP5/monthly/",#path=NULL,
                       force=FALSE,verbose=FALSE) {
  if(verbose) print("cmip5.urls")
  if(!requireNamespace("RCurl",quietly=TRUE)) {
    stop("Package 'RCurl' required to run 'cmip5.urls'. Please install it.")
  } else {
    urlfiles <- c()
    #if(is.null(path)) path <- getwd()
    for (iexp in experiment) {
      if(verbose) print(iexp)
      for (ivar in varid) {
        if(verbose) print(ivar)
        ## Loop on the number of experiments
        for (irun in 0:110) { ##
          if(verbose) print(paste(irun))
          ## Update experiment number
          if (irun < 10) run.id = paste("00",as.character(irun),sep="")
          else if (irun < 100) run.id = paste("0",as.character(irun),sep="")
          else run.id <- as.character(irun)
          
          urlfile  <- paste(url,ivar,sep="")             # add var directory
          urlfile  <- paste(urlfile,ivar,sep="/")        # add v.name
          urlfile  <- paste(urlfile,"_Amon_ens_",sep="") # add text
          urlfile  <- paste(urlfile,iexp,sep="")         # add exp.name
          urlfile  <- paste(urlfile,run.id,sep="_")      # add exp ID number
          urlfile  <- paste(urlfile,".nc",sep="")        # add file ext
          if(RCurl::url.exists(urlfile)) {
            if (verbose) print(urlfile)
            urlfiles <- c(urlfiles,urlfile)
          }
        }
      }
    }
  }
  return(urlfiles)
}
