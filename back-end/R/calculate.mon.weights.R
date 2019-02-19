## Calculate weights for the weighted average of rms, assuming monthly time step.
calculate.mon.weights <- function(lon,lat) {
  
  weights <- array(NA,dim=c(12,length(lon),length(lat)))
  time.weights <- c(31,28,31,30,31,30,31,31,30,31,30,31)/365
  lat.weights <- rep(cos(pi*lat/180),length(lon))
  dim(lat.weights) <- c(length(lat),length(lon))
  lat.weights <- t(lat.weights)
  image(lat.weights)
  for(i in 1:length(lat)){
    weights[,,i] <- time.weights
  }
  
  for(i in 1:12){
    weights[i,,] <- weights[i,,]*lat.weights
  }
  
  return(weights)
}
