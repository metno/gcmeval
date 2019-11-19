# Calculate the spatial correlation of two gridded data sets with cdo
cdo.gridcor <- function(model.file,reference.file,period=c(1981,2010),mask=NULL,
                        seasonal=FALSE,monthly=FALSE,bit=8,verbose=FALSE) {
  if(verbose) print("cdo.gridcor")
  commands <- c("-timavg","-selyear")
  input <- c("",paste(period,collapse="/"))
  
  if(!is.null(mask)){
    commands <- append(commands,"-maskregion",after=2)
    input <- append(input,mask,after=2) 
  }
  if(monthly) {
    commands <- replace(commands,commands=="-timavg","-ymonavg")
  } else if(seasonal) {
    commands <- replace(commands,commands=="-timavg","-yseasavg")
  }
  
  out.file <- "tmp.nc"
  cdo.command(commands,input,model.file,out.file,bit=bit)
  
  out.file <- "tmp2.nc"
  cdo.command(commands,input,reference.file,out.file,bit=bit)
  
  commands <- c("fldcor")
  input <- c("")
  in.file <- c("tmp.nc tmp2.nc")
  out.file <- "tmp_cor.nc"
  cdo.command(commands,input,in.file,out.file,bit=bit)
  
  command <- ("output")
  input <- c("")
  out <- as.numeric(cdo.command(command,input,out.file,NULL,intern=TRUE))
  if(monthly) {
    names(out) <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  } else if(seasonal){
    names(out) <- c("djf","mam","jja","son")
  } else{
    names(out) <- "ann"
  }
  system("rm tmp.nc tmp2.nc tmp_cor.nc")
  invisible(out)
}
