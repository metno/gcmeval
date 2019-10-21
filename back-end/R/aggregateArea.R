aggregateArea <- function(x,...,is=NULL,it=NULL,FUN='sum',
                           na.rm=TRUE,smallx=FALSE,verbose=FALSE,
                           a=6378, threshold=NULL) {
  # Estimate the area-aggregated values, e.g. the global mean (default)
  if (verbose) print(paste("aggregateArea",FUN))
  if (verbose) {
    if (FUN=='sum') print(rowSums(zoo::coredata(x),na.rm=TRUE)) else
                    print(rowMeans(zoo::coredata(x),na.rm=TRUE))
  }
  x <- subset(x,is=is,it=it,verbose=verbose)
  if ( (verbose) & (!is.null(is) | !is.null(it)) ) {
    if (FUN=='sum') print(rowSums(zoo::coredata(x),na.rm=TRUE)) else
                    print(rowMeans(zoo::coredata(x),na.rm=TRUE))
  }
  if (inherits(FUN,'function')) FUN <- deparse(substitute(FUN)) # REB140314
  if (!is.null(attr(x,'dimensions'))) d <- attr(x,'dimensions') else d <- c(dim(x),1)
  if (verbose) print(paste('dimensions',paste(d,collapse='-')))

  srtlat <- order(rep(attr(x,"latitude"),d[1]))
  dY <- a*diff(pi*attr(x,"latitude")/180)[1]
  dtheta <- diff(pi*attr(x,"longitude")/180)[1]
  aweights <- rep(dY * dtheta * a*cos(pi*attr(x,"latitude")/180),d[1])[srtlat]
  if (verbose) print(sum(aweights))
  if (FUN=='mean') {
    aweights <- aweights/sum(aweights,na.rm=TRUE)
    FUN <- 'sum'
  }
  if (verbose) print(paste('Sum of aweights should be area or 1:',round(sum(aweights))))
  
  if (FUN %in% c('sum','area','exceedance','exceedence','lessthan')) {
    if (FUN=='area') {
      ## Estimate the area of the grid boxes
      zoo::coredata(x) -> cx
      if (is.null(threshold)) {
        cx[is.finite(cx)] <- 1; cx[!is.finite(cx)] <- 0
      } else {
        cx[cx<threshold] <- 0; cx[cx >= threshold] <- 1
      }
      zoo::coredata(x) <- cx; rm('cx'); gc(reset=TRUE)
      FUN <- 'sum'
    } else if ( (FUN %in% c('exceedance','exceedence')) & !is.null(threshold) ) {
      # Estimate the sum of grid boxes with higher value than threshold
      zoo::coredata(x) -> cx
      cx[cx < threshold] <- NA
      zoo::coredata(x) <- cx; rm('cx'); gc(reset=TRUE)
      FUN <- 'sum'
    } else if ( (FUN == 'lessthan') & !is.null(threshold) ) {
      # Estimate the sum of grid boxes with lower value than threshold
      zoo::coredata(x) -> cx
      cx[cx >= threshold] <- NA
      zoo::coredata(x) <- cx; rm('cx'); gc(reset=TRUE)
      FUN <- 'sum'
    } 
    attr(x,'unit') <- paste(attr(x,'unit'),' * km^2')
  }
  
  if (smallx) {
    if (sum(!is.finite(x))==0) {
      X <- zoo::coredata(x)%*%diag(aweights)
    } else {
      if (verbose) print('Need to account for missing data in the area weighting')
      Aweights <- rep(aweights,length(zoo::index(x)))
      dim(Aweights) <- dim(x)
      print('This is incomplete - needs checking!')
      Aweights[!is.finite(zoo::coredata(x))] <- NA
      Aweights <- Aweights/apply(Aweights,1,FUN='sum',na.rm=TRUE)
      X <- zoo::coredata(X)*Aweights
    }
    y <- zoo::zoo(apply(X,1,FUN,na.rm=na.rm),order.by=zoo::index(x))
  } else {
    X <- zoo::coredata(x) 
    if (d[3]==1) dim(X) <- c(1,length(X)) ## If only one map, then set the dimensions right to get a matrix.
    if (verbose) {print(dim(X)); print(length(aweights))}
    for (i in 1:d[3]) {
      ## Temporary weights to account for variable gaps of missing data
      aweights2 <- aweights
      aweights2[!is.finite(zoo::coredata(x[i,]))] <- NA
      aweights2 <- aweights2/sum(aweights2,na.rm=TRUE)
      X[i,] <- X[i,]*aweights2
    }
    y <- zoo::zoo(apply(X,1,FUN,na.rm=na.rm),order.by=zoo::index(x))
  }
  if (verbose) print(y)
  Y <- y
  attr(Y,'location') <- paste('area',FUN,'of',attr(x,"source"))
  attr(Y,'longitude') <- range(attr(x,'longitude'))
  attr(Y,'latitude') <- range(attr(x,'latitude'))
  attr(Y,'variable') <- attr(x,'variable')
  attr(Y,'unit') <- paste(FUN,attr(x,'unit'))
  attr(Y,'longname') <- paste('area',FUN,'of',attr(x,'longname'))
  return(Y)
}
