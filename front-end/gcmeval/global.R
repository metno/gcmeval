## global.R - Load libraries and define help functions for the shiny app "gcmeval"
library(plotrix)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(sp)
library(DT)
library(esd)
library(fields)
library(plotly)
library(gcmeval) # back-end package

jsrefocus <- "
shinyjs.refocus = function(e_id) {
  document.getElementById(e_id).focus();
}"

jsresetclick <- "
shinyjs.resetClick = function() { 
  Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); 
}"

regionlist <- c(
  "Global",
  "Alaska/N.W. Canada [ALA:1]",
  "Amazon [AMZ:7]",
  "Central America/Mexico [CAM:6]",
  "small islands regions Caribbean",
  "Central Asia [CAS:20]",
  "Central Europe [CEU:12]",
  "Canada/Greenland/Iceland [CGI:2]",
  "Central North America [CNA:4]",
  "East Africa [EAF:16]",
  "East Asia [EAS:22]",
  "East North America [ENA:5]",
  "South Europe/Mediterranean [MED:13]",
  "North Asia [NAS:18]",
  "North Australia [NAU:25]",
  "North-East Brazil [NEB:8]",
  "North Europe [NEU:11]",
  "Southern Africa [SAF:17]",
  "Sahara [SAH:14]",
  "South Asia [SAS:23]",
  "South Australia/New Zealand [SAU:26]",
  "Southeast Asia [SEA:24]",
  "Southeastern South America [SSA:10]",
  "Tibetan Plateau [TIB:21]",
  "West Africa [WAF:15]",
  "West Asia [WAS:19]",
  "West North America [WNA:3]",
  "West Coast South America [WSA:9]",
  "Antarctica",
  "Arctic",
  "Pacific Islands region[2]",
  "Southern Topical Pacific",
  "Pacific Islands region[3]",
  "West Indian Ocean"
)

# Load meta data
data(package="gcmeval", "metaextract", envir=environment())

clean <- function(x) {
  gsub("[[:punct:]]|[[:space:]]","",tolower(x))
}

gcm.i <- clean(substr(meta$filename,1,
  regexpr("\\.[a-z]{2,3}",meta$filename)-1))
meta$gcmtag <- gcm.i

## Load geographical data for map
data(package="esd", "geoborders", envir=environment())

# Load stats and remove not common GCMs from stats
metaPrep <- function(rcp="rcp45") {
  if("both" %in% rcp) rcp <- c("rcp45","rcp85")
  gcmnames <- list()
  im <- list()
  if(length(rcp)==1) {
    im.pr <- meta$project_id=="CMIP5" &
      clean(meta$experiment)==clean(rcp) &
      meta$var=="tas"
    im.tas <- meta$project_id=="CMIP5" &
      clean(meta$experiment)==clean(rcp) &
      meta$var=="pr"
    gcm.pr <- paste(meta$gcm[im.pr], meta$gcm_rip[im.pr], sep=".")
    gcm.tas <- paste(meta$gcm[im.tas], meta$gcm_rip[im.tas], sep=".")
    synch <- which(!is.na( match(gcm.tas, gcm.pr) ))
    gcmnames <- gcm.tas[synch]
    attr(gcmnames,"im") <- list("tas"=which(gcm.tas %in% gcmnames),
                                "pr"=which(gcm.pr %in% gcmnames))
  } else {
    for(exp in rcp) {
      im.rcp <- meta$project_id=="CMIP5" & clean(meta$experiment)==exp
      im.tas <- im.rcp & meta$var=="tas"
      im.pr <- im.rcp & meta$var=="pr"
      gcm.tas <- paste(meta$gcm[im.tas], meta$gcm_rip[im.tas], sep=".")
      gcm.pr <- paste(meta$gcm[im.pr], meta$gcm_rip[im.pr], sep=".")
      synch <- which(!is.na( match(gcm.tas, gcm.pr) ))
      gcmnames[[exp]] <- gcm.tas[synch]
    }
    for(i in seq(2,length(gcmnames))) {
      synch <- which(!is.na(match(gcmnames[[1]], gcmnames[[i]]) ))
      gcmnames[[1]] <- gcmnames[[1]][synch]
    }
    gcmnames <- gcmnames[[1]]
    im <- list("tas"=list(), "pr"=list())
    for(exp in rcp) {
      im.rcp <- meta$project_id=="CMIP5" & clean(meta$experiment)==exp
      im.tas <- im.rcp & meta$var=="tas"
      im.pr <- im.rcp & meta$var=="pr"
      gcm.tas <- paste(meta$gcm[im.tas], meta$gcm_rip[im.tas], sep=".")
      gcm.pr <- paste(meta$gcm[im.pr], meta$gcm_rip[im.pr], sep=".")
      im$tas[[exp]] <- which(gcm.tas %in% gcmnames)
      im$pr[[exp]] <- which(gcm.pr %in% gcmnames)
    }
    attr(gcmnames,"im") <- im
  }
  return(gcmnames)
}

# Load stats and remove not common GCMs from stats
dataPrep <- function(rcp="rcp45") {
  ## Load statistics calculated with script 'calculate_statistics.R'
  # Remove when new statistics files with RCP8.5 have been added
  stats <- NULL
  gcmnames <- metaPrep(rcp=rcp)
  period.yr <- c("1981-2010","2021-2050","2071-2100")
  period.nm <- c("present","nf","ff")
  if("both" %in% rcp) rcp <- c("rcp45","rcp85")
  for(var in c("tas","pr")) {
    for(i in seq_along(period.yr)) {
      for(exp in rcp) {
        if(period.nm[i]=="present") {
          eval(parse(text=paste('data(package="gcmeval", "statistics.cmip.era.',var,".",
                                period.yr[i],".",clean(exp),
                                '", envir=environment())',sep="")))
        } else {
          eval(parse(text=paste('data(package="gcmeval", "statistics.cmip.',var,".",
                                period.yr[i],".",clean(exp),
                                '", envir=environment())',sep="")))
        }
        stats[[exp]][[var]][[period.nm[i]]] <- store
      }
    }
  }
  for(exp in rcp) {
    for(varid in c("tas","pr")) {
      i.var <- meta$var==varid & meta$project_id=="CMIP5" & clean(meta$experiment)==exp
      gcm.var <- paste(meta$gcm[i.var], clean(meta$gcm_rip[i.var]), sep=".")
      gcm.i <- meta$gcmtag[i.var]
      reject <- which(!(gcm.var %in% gcmnames))
      for (i in reject) {
        j <- which(clean(names(stats[[exp]][[varid]]$nf))==gcm.i[i])
        stats[[exp]][[varid]]$nf[[j]] <- NULL
        j <- which(clean(names(stats[[exp]][[varid]]$ff))==gcm.i[i])
        stats[[exp]][[varid]]$ff[[j]] <- NULL
        j <- which(clean(names(stats[[exp]][[varid]]$present))==gcm.i[i])
        stats[[exp]][[varid]]$present[[j]] <- NULL
      }
    }
  }
  attr(stats,"gcmnames") <- gcmnames
  return(stats)
}

#stats.rcp45 <- dataPrep(rcp="rcp45")
#stats.rcp85 <- dataPrep(rcp="rcp85")
stats.both <- dataPrep(rcp=c("rcp45","rcp85"))
gcmnames <- attr(stats.both,"gcmnames")
gcmnames <- paste(seq(gcmnames),gcmnames,sep=": ")

regions <- function(type=c("srex","prudence"),region=NULL) {
  if(is.null(type) | length(type)>1) region <- NULL
  if(is.null(type) | "srex" %in% tolower(type)) {
    f <- "../../back-end/inst/extdata/SREX_regions/referenceRegions.shp"
    x <- get.shapefile(f,with.path=TRUE)
    ivec <- 1:nrow(x)
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% x$LAB) {
        ivec <- sapply(region, function(y) which(y==x$LAB))
      } else if(region %in% x$NAME) {
        ivec <- sapply(region, function(y) which(y==x$NAME))
      } else {
        print(paste("Unknown region",region))
      }
    }
    y <- list(name=as.character(x$NAME[ivec]), 
              label=as.character(x$LAB[ivec]), 
              usage=as.character(x$USAGE[ivec]),
              type=rep("srex",length(ivec)),
              coords=lapply(ivec, function(i) t(coordinates(x@polygons[[i]]@Polygons[[1]]))))
  } else {
    y <- NULL
  }
  if(is.null(type) | "prudence" %in% tolower(type)) {
    f <- "../../back-end/inst/extdata/PRUDENCE_regions/RegionSpecifications.csv"
    x <- read.table(f,sep=",")
    ivec <- 2:nrow(x)
    names <- as.character(x[2:nrow(x),1])
    labels <- as.character(x[2:nrow(x),2])
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% labels) {
        ivec <- sapply(region, function(y) which(y==labels)+1)
      } else if(region %in% names) {
        ivec <- sapply(region, function(y) which(y==names)+1)
      } else {
        print(paste("Unknown region",region))
      }
    }
    prudence <- list(name=as.character(x[ivec,1]),
                  label=as.character(x[ivec,2]),
                  usage=rep("land",length(ivec)),
                  type=rep("prudence",length(ivec)),
                  coords=lapply(ivec, function(i) 
                    t(matrix(sapply(c(4,5,5,4,4,6,6,7,7,6), 
                             function(j) factor2numeric(x[i,j])),
                             nrow=5,ncol=2))))
    if(is.null(y)) {
      y <- prudence 
    } else {
      y <- mapply(c, y, prudence, SIMPLIFY=FALSE)
    }
  }
  invisible(y)
}

## Function 'regions' is defined in global.R
srex <- regions("srex")

#model ranks for seasons, metrics and selected focus regions
ranking <- function(stats=NULL,measure="bias",varid="tas",season="ann",
                    region="global",rcp=NULL,im=NULL) {
  if(is.null(stats)) stats <- dataPrep(rcp=rcp)
  X <- switch(varid, tas=stats$tas$present, pr=stats$pr$present)
  if(tolower(region)=="global") {
    region <- "global"
  } else {
    i.srex <- which(srex$name==region)
    region <- srex$label[i.srex]
  }
  if (!season %in% names(X$gcm.1$global$mean)) {
    season <- switch(season, "ann"="ann", "Annual mean"="ann", 
                     "djf" = c("dec","jan","feb"), "Winter" = c("dec","jan","feb"), 
                     "mam"=c("mar","apr","may"), "Spring"=c("mar","apr","may"),
                     "jja" = c("jun","jul","aug"), "Summer" = c("jun","jul","aug"), 
                     "son"=c("sep","oct","nov"), "Autumn"=c("sep","oct","nov"))
  }
  gcms <- names(X)[grepl("gcm",names(X))]
  ref <- names(X)[!grepl("gcm",names(X))]
  if(is.null(im)) im <- 1:length(gcms)
  if(ref=="era.pr") {
    X.ref <- X[ref]
    X.ref <- lapply(X[ref],function(x) lapply(x, function(y) lapply(y, function(z) z*1E3/(60*60*24))))
    X[ref] <- X.ref
  }
  if(measure=="bias") {
    skill <- rank(abs(sapply(gcms[im], function(gcm) mean(sapply(season, function(s)
      X[[gcm]][[region]][["mean"]][[s]]-X[[ref]][[region]][["mean"]][[s]])))))
  } else if (measure=="sd.ratio") {
    skill <- rank(abs(1-sapply(gcms[im], function(gcm) mean(sapply(season, function(s)
      X[[gcm]][[region]][["spatial.sd"]][[s]]/X[[ref]][[region]][["spatial.sd"]][[s]])))))
  } else if (measure=="corr") {
    skill <- rank(1-sapply(gcms[im], function(gcm) mean(sapply(season, function(s)
      X[[gcm]][[region]][[measure]][[s]]))))
  } else if (tolower(measure) %in% c("cmpi","e")) {
    skill <- try(rank(-1*sapply(gcms[im], function(gcm) X[[gcm]][[region]][[measure]])))
  }
  return(skill)
}

ranking.all <- function(stats=NULL,varid="tas",Regions=list("global","Amazon [AMZ:7]"),
                        Seasons=c("ann","djf","mam","jja","son"),rcp=NULL,im=NULL) {
  if(is.null(stats)) stats <- dataPrep(rcp=rcp)
  gcms <- names(stats[[varid]]$present)
  gcms <- gcms[grepl("gcm",gcms)]
  if(!is.null(im)) im <- 1:length(gcms)
  Ranks <- array(NA,c(length(gcms),length(Seasons),4,length(Regions)))
  for (si in 1:length(Seasons)) {
    for (ri in 1:length(Regions)) {
      Ranks[,si,1,ri] <- ranking(stats, measure="bias", varid=varid,
                                 season=Seasons[si], region=Regions[ri], im=im)
      Ranks[,si,2,ri] <- ranking(stats, measure="sd.ratio",varid=varid,
                                 season=Seasons[si], region=Regions[ri], im=im)
      Ranks[,si,3,ri] <- ranking(stats, measure="corr", varid=varid,
                                 season=Seasons[si], region=Regions[ri], im=im)
      Ranks[,si,4,ri] <- ranking(stats, measure="e", varid=varid,
                                 season=Seasons[si], region=Regions[ri], im=im)
    }
  }
  return(Ranks)
}

ranking.weighted <- function(stats=NULL,regionwm1="global",regionwm2="Amazon [AMZ:7]",wmreg1=1,wmreg2=1,
                          wmdt=1,wmdp=1,wmann=1,wmdjf=1,wmmam=1,wmjja=1,wmson=1,
                          wmbias=1,wmsd=1,wmsc=1,wmcmpi=1,rcp=NULL) {
  
  if(is.null(stats)) stats <- dataPrep(rcp=rcp)
  Regionlist <- list(regionwm1,regionwm2)
  Regionlist <- Regionlist[which(Regionlist != "---")]

  tasRanks <- ranking.all(stats, varid="tas", Regions=Regionlist, gcms=gcmst)
  prRanks <- ranking.all(stats, varid="pr", Regions=Regionlist, gcms=gcmsp)

  #seasonal ranks, weighted for temp and precip
  seas_varweightedranks <- as.numeric(wmdt)*tasRanks+as.numeric(wmdp)*prRanks

  #seasonal weights
  seasweightvec <- as.numeric(c(wmann,wmdjf,wmmam,wmjja,wmson))

  #region weights
  regweightvecin <- as.numeric(c(wmreg1,wmreg2))
  regweightvec <- regweightvecin[which(Regionlist != "---")]

  #calculate weighted ranks for selection
  weightedranks_all <- array(NA,c(length(gcmst),4))

  for (i in 1:length(gcmst)) {
    for (j in 1:4) {
      weightedranks_all[i,j] <- (seasweightvec %*% seas_varweightedranks[i,,j,] %*% regweightvec)
    }
  }
  
  #weight metrics
  metweightvec <- as.numeric(c(wmbias,wmsd,wmsc,wmcmpi))
  weightedrank_all <- rank(weightedranks_all %*% metweightvec)
  
  return(weightedrank_all)
}

spread <- function(stats=NULL, varid="tas", season="ann", region="global",
                   period="ff", rcp=NULL, im=NULL) {
  if(is.null(stats)) stats <- dataPrep(rcp=rcp)
  X <- switch(varid, tas=stats$tas, pr=stats$pr)
  if(tolower(region)=="global") {
    region <- "global"
  } else {
    i.srex <- which(srex$name==region)
    region <- srex$label[i.srex]
  }
  if (!season %in% names(X[[period]]$gcm.1$global$mean)) {
    season <- switch(season, "ann"="ann", "Annual mean"="ann", 
                     "djf" = c("dec","jan","feb"), "Winter" = c("dec","jan","feb"), 
                     "mam"=c("mar","apr","may"), "Spring"=c("mar","apr","may"),
                     "jja" = c("jun","jul","aug"), "Summer" = c("jun","jul","aug"), 
                     "son"=c("sep","oct","nov"), "Autumn"=c("sep","oct","nov"))
  }
  gcms <- names(X[[period]])[grepl("gcm",names(X[[period]]))]
  if(is.null(im)) im <- 1:length(gcms)
  dX <- diff(range(sapply(gcms[im], function(gcm) mean(sapply(season, function(s)
            X[[period]][[gcm]][[region]][["mean"]][[s]])) - 
              mean(sapply(season, function(s) 
                X$present[[gcm]][[region]][["mean"]][[s]]))),na.rm=TRUE))
  return(dX)
}

spread.all <- function(stats=NULL, varid="tas", Regions=list("global","Amazon [AMZ:7]"),
                       Seasons=list("ann","djf","mam","jja","son"), 
                       Periods=list("nf","ff"), rcp=NULL, im=NULL) {
  if(is.null(stats)) stats <- dataPrep(rcp=rcp)
  dX <- array(NA,c(length(Seasons),length(Periods),length(Regions)))
  dX.gcms <- array(NA,c(length(Seasons),length(Periods),length(Regions)))
  for (si in 1:length(Seasons)) {
    for (peri in 1:length(Periods)) {
      for (ri in 1:length(Regions)) {
        dX[si,peri,ri] <- spread(stats=stats, varid=varid, season=Seasons[[si]], 
                                 region=Regions[[ri]], period=Periods[peri], im=im)
      }
    }
  }
  return(dX)
}

spread.weighted <- function(stats=NULL,regionwm1="global",regionwm2="Amazon [AMZ:7]",wmreg1=1,wmreg2=1,
                            wmdt=1,wmdp=1,wmann=1,wmdjf=1,wmmam=1,wmjja=1,wmson=1,
                            wmbias=1,wmsd=1,wmsc=1,wmcmpi=1,rcp=NULL,im=NULL) {
  
  if(is.null(stats)) stats <- dataPrep(rcp=rcp)
  Regionlist <- list(regionwm1,regionwm2)
  Regionlist <- Regionlist[which(Regionlist != "---")]
  
  regweightvec <- as.numeric(c(wmreg1,input$wmreg2))
  regweightvec[which(list(regionwm1,regionwm2) != "---")] 
  
  dtasSpread <- spread.all(stats=stats,varid="tas",Regions=Regionlist,im=NULL)
  dtasSelSpread <- spread.all(stats=stats,varid="tas",Regions=Regionlist,im=im)
  dprSpread <- spread.all(stats=stats,varid="pr",Regions=Regionlist,im=NULL)
  dprSelSpread <- spread.all(stats=stats,varid="pr",Regions=Regionlist,im=im)
  
  dtasRelSpread <- dtasSelSpread/dtasSpread
  dprRelSpread <- dprSelSpread/dprSpread
  seas_varweightedspread <- (as.numeric(wmdt)*dtasRelSpread + 
                             as.numeric(wmdp)*dprRelSpread)/
                            (as.numeric(wmdt)+as.numeric(wmdp))
  
  weightedspread_nf <- (seasweightvec %*% seas_varweightedspread[,1,] %*% regweightvec)/
      sum(seasweightvec)/sum(regweightvec)
  weightedspread_ff <- (seasweightvec %*% seas_varweightedspread[,2,] %*% regweightvec)/
      sum(seasweightvec)/sum(regweightvec)
  
  return(list(ff=weightedspread_ff, nf=weightedspread_nf))
}





