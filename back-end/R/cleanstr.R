cleanstr <- function(x, tolower=TRUE, toupper=FALSE, ...) {
  y <- gsub("[[:punct:]]","",x)
  if(tolower) return(tolower(y)) else 
    if(toupper) return(toupper(y)) else return(y)
}
