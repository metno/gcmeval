# Calculate the spatial standard deviation with cdo
cdo.spatSd <- function(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
                       monthly=FALSE,bit=8,verbose=FALSE) {
  if(verbose) print("cdo.spatSd")
  commands <- c("-fldstd","-timmean","-selyear")
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
  
  out.file <- "tmp.nc"
  cdo.command(commands,input,model.file,out.file,bit=bit)
  
  command <- ("output")
  input <- c("")
  out <- as.numeric(cdo.command(command,input,out.file,NULL,bit=bit,intern=TRUE))
  
  if(monthly) {
    names(out) <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  } else if(seasonal) {
    names(out) <- c("djf","mam","jja","son")
  } else {
    names(out) <- "ann"
  }
  system("rm tmp.nc")
  invisible(out)
}
