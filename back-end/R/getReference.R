# Provides path to local files with reference data
getReference <- function(reference,variable,path=NULL,
                         filenames=NULL,verbose=FALSE) {
  if(verbose) print("getReference")
  generate_filename <- function(ref, var) switch(paste(ref,var,sep="."),
                                                 eraint.tas="era-interim_mon.*._tas.*.nc",
                                                 eraint.pr="era-interim_mon.*._pr.*.nc",
                                                 cfsr.tas="cfsr_tmp2m_mon.nc",
                                                 cfsr.pr="cfsr_prate_mon.nc",
                                                 eobs.tas="tg_0.50deg_reg_v17.0_mon.nc",
                                                 eobs.pr="rr_0.50deg_reg_v17.0_mon.nc",
                                                 era5.tas="era5_mon.*._tas.*.nc",
                                                 era5.pr="era5_mon.*._pr.*.nc",
                                                 gpcp.pr="gpcp.nc")
  if(is.null(filenames)) {
    file.name <- generate_filename(reference, variable)
  } else {
    i <- grepl(reference, names(filenames)) & grepl(variable, names(filenames))
    if(!any(i)) {
      warning(paste0(reference, ".", variable, 
                     " is not in the filename list"))
      file.name <- generate_filename(reference, variable)
    } else {
      file.name <- filenames[i][[1]]
    }
  }
  if(!is.null(path)) {
    file <- list.files(path = path, pattern = file.name, full.names = TRUE)
    if(!file.exists(file)) warning(paste("Can't find file", file.name))
  } else {
    file <- find.file(file.name)[1]
  }
  invisible(file)
}



