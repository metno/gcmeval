## global.R - Load libraries and define help functions for the shiny app "gcmeval"
library(plotrix)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(sp)
library(DT)
#library(esd)
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

file.shape <- "../back-end/inst/extdata/SREX_regions/referenceRegions.shp"

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

# Load metadata and statistics
data(package="gcmeval", "meta", envir=environment())
data(package="gcmeval", "statistics", envir=environment())

## Load geographical data for map
#data(package="esd", "geoborders", envir=environment())
data(package="gcmeval", "geoborder", envir=environment())

clean <- function(x) {
  gsub("[[:punct:]]|[[:space:]]","",tolower(x))
}

contactus <- function(name="", org="", from="", to="", body="") {
  if(nchar(body)>0 & nchar(to)>0) {
    subject <- paste0("New GCMeval comment from ",name," <",from,">")
    system.command <- paste0('echo "',body,'" | mail -s "',subject,'" ',to)
    system(system.command, wait=TRUE)
  }
}

period2label <- function(x="period.1981_2010") {
  plab <- switch(x, "period.1981_2010"="present",
                 "period.2071_2100"="ff",
                 "period.2021_2050"="nf")
  return(plab)
}

label2period <- function(x="present") {
  period <- switch(x, "present"="period.1981_2010",
                 "ff"="period.2071_2100",
                 "nf"="period.2021_2050")
  return(period)
}

gcmlabel <- function(x) {
  gcm <- paste0(gsub("rcp[0-9]{1,3}.|ssp[0-9]{1,3}.|CMIP[0-9].|.r[0-9]{1,2}i[0-9]{1,2}.*.","",x))
  cmip <- substr(x, regexpr("CMIP",x), regexpr("CMIP",x)+4)
  rip <- sapply(x, function(y) { if(grepl("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0-9]{1,2}", y)) {
      i <- regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0-9]{1,2}",y)
    } else {
      i <- regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}",y)
    }
    return(substr(y, i, i+attr(i,"match.length")-1)) 
  })
  exp <- sapply(x, function(y) { if(grepl("rcp[0-9]{1,2}", y)) {
      i <- regexpr("rcp[0-9]{1,2}",y)
    } else {
      i <- regexpr("ssp[0-9]{1,3}",y)
    }
    return(substr(y, i, i+attr(i,"match.length")-1)) 
  })
  return(list("gcm"=gcm, "cmip"=cmip, "rip"=rip, "exp"=exp))
}

# Load stats and remove not common GCMs from stats
metaPrep <- function(rcp="rcp45") {
  exp <- clean(unique(meta$experiment))
  if("all" %in% rcp | "both" %in% rcp) rcp <- exp
  if("CMIP5" %in% rcp) rcp <- c(rcp[rcp!="CMIP5"], exp[grepl("rcp",exp)])
  if("CMIP6" %in% rcp) rcp <- c(rcp[rcp!="CMIP6"], exp[grepl("ssp",exp)])
  gcmnames <- list()
  #im <- list("tas"=list(), "pr"=list())
  for(exp in rcp) {
    im.pr <- grepl("CMIP",meta$project_id) & clean(meta$experiment)==clean(exp) & meta$var=="pr"
    im.tas <- grepl("CMIP",meta$project_id) & clean(meta$experiment)==clean(exp) & meta$var=="tas"
    gcm.pr <- meta$gcm.i[im.pr]
    gcm.tas <- meta$gcm.i[im.tas]
    gcmnames[[exp]] <- gcm.tas[gcm.tas %in% gcm.pr & !is.na(gcm.tas)]
    #im$tas[[exp]] <- which(im.tas)[gcm.tas %in% gcmnames[[exp]]]
    #im$pr[[exp]] <- which(im.pr)[gcm.pr %in% gcmnames[[exp]]]
  }
  #attr(gcmnames,"im") <- im
  return(gcmnames)
}

# Load stats and remove not common GCMs from stats
dataPrep <- function(rcp="rcp45") {
  ## Load statistics calculated with script 'calculate_statistics.R'
  ## and remove data that will not be used in front-end
  gcmnames <- metaPrep(rcp=rcp)
  stats <- statistics
  exp <- clean(unique(meta$experiment))
  if("all" %in% rcp | "both" %in% rcp) rcp <- exp
  if("CMIP5" %in% rcp) rcp <- c(rcp[rcp!="CMIP5"], exp[grepl("rcp",exp)])
  if("CMIP6" %in% rcp) rcp <- c(rcp[rcp!="CMIP6"], exp[grepl("ssp",exp)])
  for(exp in rcp) {
    gcm.meta <- gcmnames[[exp]] # meta
    gcm.tas <- list() # stats
    for(period in names(stats$tas[[exp]])) {
      gcm.tas[[period]] <- names(stats$tas[[exp]][[period]])
    }
    gcm.pr <- list() # stats
    for(period in names(stats$pr[[exp]])) {
      gcm.pr[[period]] <- names(stats$pr[[exp]][[period]])
    }
    ## Find and exclude GCMS that are missing in metadata or statistics for some period or variable:
    gcm.both <- lapply(1:length(gcm.tas), function(i) gcm.tas[[i]][gcm.tas[[i]] %in% gcm.pr[[i]]])
    gcm.stats <- gcm.both[[1]]
    for(i in 2:length(gcm.both)) gcm.stats <- gcm.stats[gcm.stats %in% gcm.both[[i]]]
    gcm.stats <- gcm.stats[gcm.stats %in% gcm.meta]
    ## Find and exclude data in metadata:
    gcmnames[[exp]] <- gcm.meta[gcm.meta %in% gcm.stats]
    #attr(gcmnames,"im")$tas[[exp]] <- attr(gcmnames,"im")$tas[[exp]][gcm.meta %in% gcm.stats]
    #attr(gcmnames,"im")$pr[[exp]] <- attr(gcmnames,"im")$pr[[exp]][gcm.meta %in% gcm.stats]
    # Find and exclude data in statistics:
    for(var in c("tas","pr")) {
      for(period in names(stats[[var]][[exp]])) {
        gcm.var <- names(stats[[var]][[exp]][[period]])
        nok <- gcm.var[!gcm.var %in% gcm.stats]
        if(length(nok)>0) {
          for(gcm in nok) {
            i <- which(names(stats[[var]][[exp]][[period]])==gcm)
            stats[[var]][[exp]][[period]][[i]] <- NULL
          }
        }
      }
    }
  }
  attr(stats,"gcmnames") <- gcmnames
  return(stats)
}

regions <- function(type=c("srex","prudence"),region=NULL) {
  if(is.null(type) | length(type)>1) region <- NULL
  if(is.null(type) | "srex" %in% tolower(type)) {
    x <- get.shapefile(filename=file.shape)
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
              coords=lapply(ivec, function(i) t(coordinates(sf::as_Spatial(x)@polygons[[i]]@Polygons[[1]]))))
  } else {
    y <- NULL
  }
  if(is.null(type) | "prudence" %in% tolower(type)) {
    f <- "../back-end/inst/extdata/PRUDENCE_regions/RegionSpecifications.csv"
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

stats.all <- dataPrep(rcp="all")
gcmnames.all <- attr(stats.all,"gcmnames")
gcmnames <- unique(unlist(gcmnames.all))
#gcmnames <- paste(seq(gcmnames),gcmnames,sep=": ")
srex <- regions("srex")

#model ranks for seasons, metrics and selected focus regions
ranking <- function(stats=NULL, measure="bias", varid="tas", season="ann",
                    region="global", ref=NULL, rcp="rcp45", im=NULL) {
  if(is.null(stats)) stats <- stats.all
  exp <- clean(unique(names(stats[[varid]])))
  exp <- exp[grepl("ssp|rcp",exp)]
  if(is.null(rcp)) {
    rcp <- exp
  } else {
    rcp <- clean(rcp)
    if("all" %in% rcp | "both" %in% rcp) rcp <- exp
    if("cmip5" %in% rcp) rcp <- c(rcp[rcp!="cmip5"], exp[grepl("rcp",exp)])
    if("cmip6" %in% rcp) rcp <- c(rcp[rcp!="cmip6"], exp[grepl("ssp",exp)])
    rcp <- rcp[rcp %in% exp]
  }
  
  present <- label2period("present")
  X <- lapply(rcp, function(exp) stats[[varid]][[exp]][[present]])
  names(X) <- rcp
  
  references <- names(stats[[varid]])[!grepl("ssp|rcp",names(stats[[varid]]))]
  if(is.null(ref)) {
    ref <- references[[1]]
  } else {
    ref <- clean(ref)
    if(grepl(ref,"eraint")) ref <- "eraint"
    if(!ref %in% clean(references)) {
      print(paste0("Warning! Can't find statistics for reference ",ref,"!"))
      ref <- references[[1]]
    } else {
      ref <- references[sapply(references, function(x) grepl(clean(x),ref))][1]
    }
  }
  ref.stats <- stats[[varid]][[ref]][[present]]
  
  if(tolower(region)=="global") {
    region <- "global"
  } else {
    i.srex <- which(srex$name==region)
    region <- srex$label[i.srex]
  }
  if (!season %in% names(X[[1]][[1]]$global$mean)) {
    season <- switch(season, "ann"="ann", "Annual mean"="ann", 
                     "djf" = c("dec","jan","feb"), "Winter" = c("dec","jan","feb"), "Winter (DJF)" = c("dec","jan","feb"), 
                     "mam"=c("mar","apr","may"), "Spring"=c("mar","apr","may"), "Spring (MAM)"=c("mar","apr","may"),
                     "jja" = c("jun","jul","aug"), "Summer" = c("jun","jul","aug"), "Summer (JJA)" = c("jun","jul","aug"),
                     "son"=c("sep","oct","nov"), "Autumn"=c("sep","oct","nov"), "Autumn (SON)"=c("sep","oct","nov"))
  }
  
  gcms <- lapply(X, names)
  gcms.all <- unique(unlist(gcms))
  if(is.null(im)) im <- gcms.all
  if(is.numeric(im)) im <- gcms.all[im]
  
  Z <- list()
  for(i in 1:length(X)) {
    if(any(gcms[[i]] %in% im)) {
      gcm.i <- gcms[[i]][gcms[[i]] %in% im]
      if(measure=="bias") {
        y <- abs(sapply(gcm.i, function(gcm) mean(sapply(season, function(s)
          X[[i]][[gcm]][[region]][["mean"]][[s]]-ref.stats[[region]][["mean"]][[s]]))))
      } else if (tolower(measure)=="sd.ratio") {
        y <- abs(1-sapply(gcm.i, function(gcm) mean(sapply(season, function(s)
          X[[i]][[gcm]][[region]][["spatial.sd"]][[s]]/ref.stats[[region]][["spatial.sd"]][[s]]))))
      } else if (tolower(measure)=="corr") {
        y <- 1-sapply(gcm.i, function(gcm) mean(sapply(season, function(s)
          X[[i]][[gcm]][[region]][[measure]][[ref]][[s]])))
      } else if (tolower(measure) %in% c("cmpi","rmse")) {
        y <- sapply(gcm.i, function(gcm) X[[i]][[gcm]][[region]][[measure]][[ref]])
      }
      names(y) <- gcm.i
      Z[[names(X)[[i]]]] <- y 
    }
  }
  gcms <- unlist(lapply(Z, names))
  Z <- unlist(Z)
  if(any(duplicated(gcms))) {
    for(gcm in gcms[duplicated(gcms)]) {
      Z[gcms==gcm] <- mean(Z[gcms==gcm])
    }
    skill <- rep(NA,length(gcms))
    skill[!duplicated(gcms)] <- rank(Z[!duplicated(gcms)])
    for(gcm in gcms[duplicated(gcms)]) {
      skill[gcms==gcm] <- skill[gcms==gcm][[1]]
    }
    names(skill) <- names(Z)
  } else {
    skill <- rank(Z)
  }
  skill <- skill[order(names(skill))]
  return(skill)
}

ranking.all <- function(stats=NULL, varid="tas", Regions=list("global","Amazon [AMZ:7]"),
                        Seasons=c("ann","djf","mam","jja","son"),
                        ref=NULL, rcp=NULL, im=NULL) {
  R1 <- ranking(measure="bias", varid=varid, ref=ref, rcp=rcp,
                season=Seasons[1], region=Regions[1], im=im)
  gcms <- names(R1)
  Ranks <- array(NA,c(length(gcms),length(Seasons),4,length(Regions)))
  Ranks[,1,1,1] <- R1
  for (si in 1:length(Seasons)) {
    for (ri in 1:length(Regions)) {
      if(!identical(c(1,1),c(si,ri))) {
        Ranks[,si,1,ri] <- ranking(stats, measure="bias", varid=varid, ref=ref,  rcp=rcp,
                                   season=Seasons[si], region=Regions[ri], im=im)
      }
      Ranks[,si,2,ri] <- ranking(stats, measure="sd.ratio",varid=varid, ref=ref,  rcp=rcp,
                                 season=Seasons[si], region=Regions[ri], im=im)
      Ranks[,si,3,ri] <- ranking(stats, measure="corr", varid=varid, ref=ref, rcp=rcp,
                                 season=Seasons[si], region=Regions[ri], im=im)
      Ranks[,si,4,ri] <- ranking(stats, measure="rmse", varid=varid, ref=ref, rcp=rcp,
                                 season=Seasons[si], region=Regions[ri], im=im)
    }
  }
  rownames(Ranks) <- gcms
  return(Ranks)
}

spread <- function(stats=NULL, varid="tas", season="ann", region="global",
                   period="ff", rcp=NULL, im=NULL) {
  
  if(is.null(stats)) stats <- stats.all
  
  exp <- clean(unique(names(stats[[varid]])))
  exp <- exp[grepl("ssp|rcp",exp)]
  if(is.null(rcp)) {
    rcp <- exp
  } else {
    rcp <- clean(rcp)
    if("all" %in% rcp | "both" %in% rcp) rcp <- exp
    if("CMIP5" %in% rcp) rcp <- c(rcp[rcp!="CMIP5"], exp[grepl("rcp",exp)])
    if("CMIP6" %in% rcp) rcp <- c(rcp[rcp!="CMIP6"], exp[grepl("ssp",exp)])
    rcp <- rcp[rcp %in% exp]
  }

  p <- label2period(period)
  X <- lapply(rcp, function(exp) stats[[varid]][[exp]][[p]])
  names(X) <- rcp
  Present <- lapply(rcp, function(exp) stats[[varid]][[exp]][[label2period("present")]])
  names(Present) <- rcp
  
  if(tolower(region)=="global") {
    region <- "global"
  } else {
    i.srex <- which(srex$name==region)
    region <- srex$label[i.srex]
  }
  if (!season %in% names(X[[period]]$gcm.1$global$mean)) {
    season <- switch(season, "ann"="ann", "Annual mean"="ann",
                     "djf" = c("dec","jan","feb"), "Winter" = c("dec","jan","feb"), "Winter (DJF)" = c("dec","jan","feb"),
                     "mam"=c("mar","apr","may"), "Spring"=c("mar","apr","may"), "Spring (MAM)"=c("mar","apr","may"),
                     "jja" = c("jun","jul","aug"), "Summer" = c("jun","jul","aug"), "Summer (JJA)" = c("jun","jul","aug"),
                     "son"=c("sep","oct","nov"), "Autumn"=c("sep","oct","nov"), "Autumn (SON)"=c("sep","oct","nov")) 
  }
  
  gcms.all <- unique(unlist(lapply(X,names)))
  if(is.null(im)) im <- gcms.all
  if(is.numeric(im)) im <- gcms.all[im]
  
  dX <- list()
  for(i in 1:length(X)) {
    gcms <- names(X[[i]])
    if(any(im %in% gcms)) {
      j <- which(gcms %in% im)
      dX[[names(X)[i]]] <- sapply(gcms[j], 
        function(gcm) { 
          mean(sapply(season, function(s) { X[[i]][[gcm]][[region]][["mean"]][[s]] - Present[[i]][[gcm]][[region]][["mean"]][[s]]}))
        })
    }
  }
  return(diff(range(unlist(dX))))
}

spread.all <- function(stats=NULL, varid="tas", Regions=list("global","Amazon [AMZ:7]"),
                       Seasons=list("ann","djf","mam","jja","son"), 
                       Periods=list("nf","ff"), rcp=NULL, im=NULL) {
  dX <- array(NA,c(length(Seasons),length(Periods),length(Regions)))
  for (si in 1:length(Seasons)) {
    for (peri in 1:length(Periods)) {
      for (ri in 1:length(Regions)) {
        dX[si,peri,ri] <- spread(stats, varid=varid, season=Seasons[[si]], rcp=rcp,
                                 region=Regions[[ri]], period=Periods[[peri]], im=im)
      }
    }
  }
  return(dX)
}

# spread.weighted <- function(regionwm1="global",regionwm2="Amazon [AMZ:7]",wmreg1=1,wmreg2=1,
#                             wmdt=1,wmdp=1,wmann=1,wmdjf=1,wmmam=1,wmjja=1,wmson=1,
#                             wmbias=1,wmsd=1,wmsc=1,wmcmpi=1,rcp=NULL,im=NULL) {
#   
#   if(is.null(stats)) stats <- dataPrep(rcp=rcp)
#   Regionlist <- list(regionwm1,regionwm2)
#   Regionlist <- Regionlist[which(Regionlist != "---")]
#   
#   regweightvec <- as.numeric(c(wmreg1,wmreg2))
#   regweightvec[which(list(regionwm1,regionwm2) != "---")] 
#   
#   dtasSpread <- spread.all(varid="tas",Regions=Regionlist,rcp=rcp,im=NULL)
#   dprSpread <- spread.all(varid="pr",Regions=Regionlist,rcp=rcp,im=NULL)
#   dtasSelSpread <- spread.all(varid="tas",Regions=Regionlist,rcp=rcp,im=im)
#   dprSelSpread <- spread.all(varid="pr",Regions=Regionlist,rcp=rcp,im=im)
#   
#   dtasRelSpread <- dtasSelSpread/dtasSpread
#   dprRelSpread <- dprSelSpread/dprSpread
#   seas_varweightedspread <- (as.numeric(wmdt)*dtasRelSpread + 
#                              as.numeric(wmdp)*dprRelSpread)/
#                             (as.numeric(wmdt)+as.numeric(wmdp))
#   
#   weightedspread_nf <- (seasweightvec %*% seas_varweightedspread[,1,] %*% regweightvec)/
#       sum(seasweightvec)/sum(regweightvec)
#   weightedspread_ff <- (seasweightvec %*% seas_varweightedspread[,2,] %*% regweightvec)/
#       sum(seasweightvec)/sum(regweightvec)
#   
#   return(list(ff=weightedspread_ff, nf=weightedspread_nf))
# }
#
# ranking.weighted <- function(regionwm1="global", regionwm2="Amazon [AMZ:7]",
#                              wmreg1=1, wmreg2=1, wmdt=1, wmdp=1,
#                              wmann=1, wmdjf=1, wmmam=1, wmjja=1, wmson=1,
#                              wmbias=1, wmsd=1, wmsc=1, wmcmpi=1,
#                              rcp=NULL, ref=NULL) {
#   
#   Regionlist <- list(regionwm1,regionwm2)
#   Regionlist <- Regionlist[which(Regionlist != "---")]
#   tasRanks <- ranking.all(varid="tas", Regions=Regionlist, rcp=rcp, ref=ref)#, gcms=gcmst)
#   prRanks <- ranking.all(varid="pr", Regions=Regionlist, rcp=rcp, ref=ref)#, gcms=gcmsp)
#   tasRanks <- tasRanks[rownames(tasRanks) %in% rownames(prRanks),,,]
#   prRanks <- prRanks[rownames(prRanks) %in% rownames(tasRanks),,,]
#   prRanks <- prRanks[order(rownames(prRanks)),,,]
#   tasRanks <- tasRanks[order(rownames(tasRanks)),,,]
# 
#   #seasonal ranks, weighted for temp and precip
#   seas_varweightedranks <- as.numeric(wmdt)*tasRanks+as.numeric(wmdp)*prRanks
# 
#   #seasonal weights
#   seasweightvec <- as.numeric(c(wmann,wmdjf,wmmam,wmjja,wmson))
# 
#   #region weights
#   regweightvecin <- as.numeric(c(wmreg1,wmreg2))
#   regweightvec <- regweightvecin[which(Regionlist != "---")]
# 
#   #calculate weighted ranks for selection
#   gcms <- rownames(tasRanks)
#   weightedranks_all <- array(NA,c(length(gcms),4))
# 
#   for (i in 1:length(gcms)) {
#     for (j in 1:4) {
#       weightedranks_all[i,j] <- (seasweightvec %*% seas_varweightedranks[i,,j,] %*% regweightvec)
#     }
#   }
#   
#   #weight metrics
#   metweightvec <- as.numeric(c(wmbias,wmsd,wmsc,wmcmpi))
#   weightedrank_all <- rank(weightedranks_all %*% metweightvec)
#   names(weightedrank_all) <- gcms
#   
#   return(weightedrank_all)
# }

