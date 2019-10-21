#Get grid boxes belonging to a SREX region and calculate some basic statistics for it.
get.srex.region <- function(destfile,region=NULL,print.srex=FALSE,verbose=FALSE) {
  if(verbose) print("get.srex.region")
  home <- system("echo $HOME",intern=TRUE)
  shape <-  get.shapefile("referenceRegions.shp")
  X <- retrieve(destfile,lon=NULL,lat=NULL,verbose=verbose)
  srex <- list()
  if(is.null(region)){
    for (i in 1:length(levels(shape$LAB))){
      polygon <- shape[i,]
      mask <- gen.mask.srex(destfile=destfile, mask.polygon=polygon, 
                            ind=FALSE, inverse=FALSE, mask.values=1)
      if(verbose){
        if(i==1){
          plot(shape)
        }
        plot.mask <- mask
        raster::extent(plot.mask) <- c(-180,180,-90,90)
        raster::projection(plot.mask) <- raster::projection(shape)
        plot(plot.mask,col=rainbow(100, alpha=0.35)[sample(1:100,1)],
             legend=FALSE,add=TRUE)
      }
      name <- levels(shape$NAME)[i]
      X.region <- mask.zoo(X,mask)
      srex[[name]]$name <- name
      srex[[name]]$label <- levels(shape$LAB)[i]
      srex[[name]]$area.mean <- aggregateArea(X.region,FUN="mean",na.rm=TRUE)
      srex[[name]]$area.sd <- aggregateArea(X.region,FUN="sd",na.rm=TRUE)
    }  
  } else {
    i <- which(levels(shape$LAB)==region)
    polygon <- shape[i,]
    mask <- gen.mask.srex(destfile=destfile, mask.polygon=polygon, ind=FALSE, 
                          inverse=FALSE, mask.values=1)
    if(verbose) {
      plot(shape)
      plot.mask <- mask
      raster::extent(plot.mask) <- c(-180,180,-90,90)
      raster::projection(plot.mask) <- raster::projection(shape)
      plot(plot.mask,col=rainbow(100, alpha=0.35)[sample(1:100,1)],
           legend=FALSE,add=TRUE)
    }
    name <- levels(shape$NAME)[i]
    X.region <- mask.zoo(X,mask)
    srex[[name]]$name <- name
    srex[[name]]$label <- levels(shape$LAB)[i]
    srex[[name]]$area.mean <- aggregateArea(X.region,FUN="mean",na.rm=T)
    srex[[name]]$area.sd <- aggregateArea(X.region,FUN="sd",na.rm=T) 
  }
  if(print.srex) {
    print("Region names in alphabetical order and the corresponding label to be used when selecting the region:")
    print(data.frame(NAME=gsub("\\[[^\\]]*\\]", "", levels(shape$NAME), perl=TRUE),
                     LABEL=levels(shape$LAB)))
    return()
  }
  return(srex)
}
