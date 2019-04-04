# Provides path to local files with reference data
getReference <- function(reference,variable,verbose=FALSE) {
  if(verbose) print("getReference")
  #path <- system("echo $EXTERNAL_DATA",intern=TRUE)
  file.name <- switch(paste(reference,variable,sep="."),
                      eraint.tas="era-interim_monthly_1979-2017_tas.2.5deg.nc",
                      eraint.pr="era-interim_monthly_1979-2017_pr.2.5deg.nc",
                      cfsr.tas="cfsr_tmp2m_mon.nc",
                      cfsr.pr="cfsr_prate_mon.nc",
                      eobs.tas="tg_0.50deg_reg_v17.0_mon.nc",
                      eobs.pr="rr_0.50deg_reg_v17.0_mon.nc",
                      era5.tas="era5_monthly_1979-2018_tas.2.5deg.nc",
                      era5.pr="era5_monthly_1979-2018_pr.2.5deg.nc",
                      gpcp.pr="gpcp.nc")
  file.name <- find.file(file.name)[1]
  invisible(file.name)
  #invisible(paste(path,file.name,sep="/"))
}
