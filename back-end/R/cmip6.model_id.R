cmip6.model_id <- function(txt) {
  txt <- substr(txt,10,nchar(txt))
  txt2 <- unlist(strsplit(txt,split="_"))
  txt <- txt2[1]
  invisible(txt)
}