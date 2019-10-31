cleanstr <- function(x) {
  return(tolower(gsub("[[:punct:]]","",x)))
}
