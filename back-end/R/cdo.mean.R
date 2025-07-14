# Calculate the mean value over time with CDO. Faster than in R.
# Is this function affected by the error that Abdelkader discovered with -timmean? No.

cdo.mean <- function(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
                     monthly=FALSE,is.temp=TRUE,outfile=NULL,bit=8,verbose=FALSE) {
  
  commands <- c("-fldmean","-timmean","-selyear")
  input <- c("","",paste(period,collapse="/"))
  
  if(!is.null(mask)){
    commands <- append(commands,"-maskregion",after=2)
    input <- append(input,mask,after=2) 
  }
  if(monthly) {
    commands <- replace(commands,commands=="-timmean","-ymonmean")
  } else if(seasonal){
    commands <- replace(commands,commands=="-timmean","-yseasmean")
  }
  
  if(is.null(outfile)) {
    outfile <- "tmp.nc"
    save.file <- FALSE
  } else {
    save.file <- TRUE
  }
  
  cdo.command(commands,input,model.file,outfile,bit=bit)
  
  command <- ("output")
  input <- c("")
  
  out <- as.numeric(cdo.command(command,input,outfile,NULL,bit=bit,intern=TRUE))
  if(monthly) {
    names(out) <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  } else if(seasonal) {
    names(out) <- c("djf","mam","jja","son")
  } else {
    names(out) <- "ann"
  } 
  
  # If applying to e.g. slp data, set is.temp to FALSE to skip this correction:
  if(all(out>200) & is.temp) out <- out-273.15 
  if(!save.file) system(paste("rm",outfile,sep=" "))
  invisible(out)
}
