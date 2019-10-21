## function adopted from the esd-package
distAB <- function (lon, lat, lons, lats, a = 6378000) {
  good <- is.finite(lons) & is.finite(lats)
  lons <- lons[good]
  lats <- lats[good]
  if ((length(lon) != 1) | (length(lat) != 1) | (length(lons) != 
    length(lats))) {
    print(paste("distAB [clim.pact]: length(lon)=", length(lon), 
          "length(lat)=", length(lat), "length(lons)=", length(lons), 
          "length(lats)=", length(lats)))
    print("length(lons) must equal length(lats) and lon and lat must have length=1")
    stop("Error in distAB - argument lengths do not match!")
  }
  theta <- pi * lon/180
  phi <- pi * lat/180
  dist <- rep(NA, length(lons))
  r1 <- c(cos(phi) * cos(theta), sin(phi), cos(phi) * sin(theta))
  dim(r1) <- c(3, length(lon))
  theta <- pi * lons/180
  phi <- pi * lats/180
  r2 <- cbind(cos(phi) * cos(theta), sin(phi), cos(phi) * sin(theta))
  angle <- acos(r2 %*% r1)
  dist <- rep(NA, length(lons))
  dist[good] <- a * angle
  return(dist)
}
