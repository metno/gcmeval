## Function to extract the metadata from local NetCDF files
metaextract.v2 <- function(file.in,path=NULL,add=TRUE,file.out="meta.rda",verbose=FALSE) {
  if(verbose) print("metaextract.v2")
  Y <- NULL
  for(f in file.in) {
    if(verbose) print(paste("Extracting metadata from file",f))
    if(inherits(f,"character") & grep(".nc",f)) {
      if(!is.null(path)) f <- file.path(path,f)
      if(!file.exists(f)) {
        warning(paste("Warning! File",file.in,"does not exist."))
      } else {
        x <- getcid(filename=f, verbose=verbose)
        if(is.null(x$project_id)) {
          warning(paste("Warning! project_id is not specified in",x$filename))
          yi <- NULL
        } else if(grepl("cmip",tolower(x$project_id))) {
          yi <- metaextract.cmip(x,verbose=verbose)
        } #else if(grepl("cordex",tolower(x$project_id))) {
          #yi <- metaextract.cordex(x,verbose=verbose)
        #}
      
        if(is.null(Y)) {
          Y <- yi
        } else {
          n <- nrow(Y)
          Y.new <- Y
          cn.all <- unique(c(colnames(Y),colnames(yi)))
          Y.new <- matrix(NA,ncol=length(cn.all),nrow=n+1)
          colnames(Y.new) <- cn.all
          j <- sapply(colnames(Y),function(x) which(cn.all==x))
          Y.new[1:n,j] <- Y[1:n,]
          for(cn in colnames(yi)) {
            Y.new[n+1,colnames(Y.new)==cn] <- yi[colnames(yi)==cn]
          }
          Y <- Y.new
        }
      }
    }
  }
  Y <- as.data.frame(Y)
  if(add & file.exists(file.out)) {
    if(verbose) print(paste("Merge new metadata with exisiting metadata from",file.out))
    load(file.out)
    meta.old <- meta
    if(any(!colnames(Y)%in%colnames(meta.old))) {
      new.cols <- colnames(Y)[!colnames(Y)%in%colnames(meta.old)] 
      for(n in new.cols) meta.old[[n]] <- rep(NA,nrow(meta.old))
    }
    if(any(!colnames(meta.old)%in%colnames(Y))) {
      new.cols <- colnames(meta.old)[!colnames(meta.old)%in%colnames(Y)] 
      for(n in new.cols) Y[[n]] <- rep(NA,nrow(Y))
    }
    meta <- rbind(meta.old,Y)
    gcm.i <- paste(meta$project_id,meta$gcm,meta$gcm_rip,sep=".")
    meta$gcm.i <- gcm.i
    id <- paste(meta$project_id,meta$experiment,meta$var,meta$gcm,meta$gcm_rip,sep=".")
    meta <- meta[order(id),]
  } else {
    if(verbose) print("Create new metadata file")
    Y -> meta
  }
  meta <- meta[!duplicated(meta),]
  save(meta,file=file.out)
  return(meta)
}
