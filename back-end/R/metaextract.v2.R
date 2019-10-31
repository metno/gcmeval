## Function to extract the metadata from local NetCDF files
metaextract.v2 <- function(file.in,file.out="meta.rda",path.in=NULL,path.out=NULL,
                           add=TRUE,force=FALSE,verbose=FALSE) {
  if(verbose) print("metaextract.v2")
  if(!is.null(path.out)) file.out <- file.path(path.out,file.out)
  if(add & file.exists(file.out)) {
    if(verbose) print(paste("Merge new metadata with exisiting metadata from",file.out))
    load(file.out)
    Y <- as.matrix(meta)
  } else {
    if(verbose) print(paste("Creating new metadata file",file.out))
    Y <- NULL
  }
  if(!is.null(Y) & !force) file.in <- file.in[!basename(file.in) %in% Y[,colnames(Y)=="filename"]]
  for(f in file.in) {
    if(verbose) print(paste("Extracting metadata from file",f))
    if(inherits(f,"character") & grep(".nc",f)) {
      if(!is.null(path.in)) f <- file.path(path.in,f)
      if(!file.exists(f)) {
        warning(paste("Warning! File",f,"does not exist."))
      } else {
        x <- getncid(filename=f, verbose=verbose)
        if(is.null(x$project_id)) {
          warning(paste("Warning! project_id is not specified in",x$filename))
          yi <- NULL
        } else if(grepl("cmip",tolower(x$project_id))) {
          yi <- metaextract.cmip(x,verbose=verbose)
        } else {
          warning(paste("Warning! This is not CMIP data. I don't know what to do with",x$filename))
          yi <- NULL
        } #else if(grepl("cordex",tolower(x$project_id))) {
          #yi <- metaextract.cordex(x,verbose=verbose)
        #}
        if(is.null(Y)) {
          Y <- yi
        } else if(force & (yi[colnames(yi)=="filename"] %in% Y[,colnames(Y)=="filename"])) {
          i <- which(Y[,colnames(Y)=="filename"]==yi[colnames(yi)=="filename"])
          if(any(!colnames(yi) %in% colnames(Y))) {
            cn.all <- unique(c(colnames(Y),colnames(yi)))
            Y.new <- matrix(NA,ncol=length(cn.all),nrow=nrow(Y))
            colnames(Y.new) <- cn.all
            j <- sapply(colnames(Y), function(x) which(cn.all==x))
            Y.new[,j] <- Y[,]
          } else {
            Y.new <- Y
          }
          for(cn in colnames(yi)) {
            Y.new[i,colnames(Y.new)==cn] <- yi[colnames(yi)==cn]
          }
          Y <- Y.new
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
  meta <- as.data.frame(Y)
  gcm.i <- paste(meta$project_id,gsub("-","_",meta$gcm),meta$gcm_rip,sep=".")
  meta$gcm.i <- gcm.i
  id <- paste(meta$project_id,gsub("[.]","",tolower(meta$experiment)),
              meta$var,meta$gcm,meta$gcm_rip,sep=".")
  meta <- meta[order(id),]
  meta <- meta[!duplicated(meta),]
  save(meta,file=file.out)
  return(meta)
}
