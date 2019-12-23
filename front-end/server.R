
## Define a server for the Shiny app
shinyServer(function(input, output, session) {

  stats <- reactive({
    Y <- list()
    for(var in names(stats.all)) {
      for(rcp in input$rcp) { #c("rcp45","rcp85","ssp585")) {
        X <- stats.all[[var]][[clean(rcp)]]
        for(period in names(X)) {
          plab <- period2label(period)
          n <- names(X[[period]])
          gcms <- n[n %in% input$baseensemble]
          for(gcm in gcms[order(gcms)]) {
            Y[[var]][[plab]][[paste(clean(rcp),gcm,sep=".")]] <- X[[period]][[gcm]]
          }
        }
      }
    }
    return(Y)
  })

  gcmst <- reactive({
    names(stats()$tas$ff)
  })
  
  gcmsp <- reactive({
    names(stats()$pr$ff)
  })

  # Region selection for ranking
  Regionlist <- reactive({
    rl <- list(input$regionwm1,input$regionwm2)
    rl[which(rl != "---")]
  })
  
  gcmsSelected <- reactive({
    input$gcms
  })

  ## Region selection for scatterplot
  get.region <- function(x) {
    if(tolower(x)=="global") {
      y <- "global"
    } else {
      i.srex <- which(srex$name==x)
      y <- srex$label[i.srex]
    }
    return(y)
  }
  Region1 <- reactive({ 
    get.region(input$regionwm1)
  })
  Region2 <- reactive({ 
    get.region(input$regionwm2) 
  })

  ## Season selection for scatterplot
  Season <- reactive({switch(tolower(as.character(input$season)),
                             'annual mean'='ann',
                             'winter'=c('dec','jan','feb'),
                             'spring'=c('mar','apr','may'),
                             'summer'=c('jun','jul','aug'),
                             'autumn'=c('sep','oct','nov'))})
  ## Period selection for scatterplot
  Period <- reactive({switch(tolower(as.character(input$period)),
                             "far future (2071-2100)"='ff',
                             "near future (2021-2050)"='nf')})

  ## Weighted rank calculations
  tasRanks <- reactive({
    #rcp <- NULL
    #if(any(grepl("CMIP5",input$rank.ensemble))) {
    #  rcp <- c(rcp,"rcp45","rcp85")
    #}
    #if(any(grepl("CMIP6",input$rank.ensemble))) {
    #  rcp <- c(rcp,"ssp585")
    #}
    rcp <- input$rcp
    ranking.all(varid="tas",ref=input$tasref,rcp=rcp,Regions=Regionlist(),im=input$baseensemble)
  })
  
  prRanks <- reactive({
    #rcp <- NULL
    #if(any(grepl("CMIP5",input$rank.ensemble))) {
    #  rcp <- c(rcp,"rcp45","rcp85")
    #}
    #if(any(grepl("CMIP6",input$rank.ensemble))) {
    #  rcp <- c(rcp,"ssp585")
    #}
    rcp <- input$rcp
    ranking.all(varid="pr",ref=input$prref,rcp=rcp,Regions=Regionlist(),im=input$baseensemble)
  })

  gcmnamesRanks <- reactive({gsub("ssp[0-9]{1,3}.|rcp[0-9]{1,3}.","",rownames(tasRanks()))})
  seasVarweightedranks <- reactive({as.numeric(input$wmdt)*tasRanks()+as.numeric(input$wmdp)*prRanks()})
  seasweightvec <- reactive({as.numeric(c(input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson))})
  regweightvec <- reactive({
    rw <- as.numeric(c(input$wmreg1,input$wmreg2))
    rw[which(list(input$regionwm1,input$regionwm2) != "---")] 
  })
  
  weightedranksAll <- reactive({
    X <- seasVarweightedranks()
    sw <- seasweightvec()
    rw <- regweightvec()
    W <- array(NA,c(nrow(X),4))
    for (i in 1:nrow(X)) {
      for (j in 1:4) {
        W[i,j] <- sw %*% X[i,,j,] %*% rw
      }
    }
    invisible(W)
  })
  
  metweightvec <- reactive({as.numeric(c(input$wmbias,input$wmsd,input$wmsc,input$wmrmse))})
  weightedrank <- reactive({
    wr_all <- weightedranksAll() %*% metweightvec()
    x <- gcmlabel(rownames(tasRanks()))
    y <- paste(x$cmip,x$gcm,x$rip,sep=".")
    ranks_all <- rep(0,length(wr_all))
    ranks_all[!duplicated(y)] <- rank(wr_all[!duplicated(y)])
    for(gcm in y[duplicated(y)]) {
      ranks_all[y==gcm] <- ranks_all[y==gcm][1]
    }
    return(ranks_all)
  })
  
  best <- reactive({
    wr <- weightedrank()
    wrmax <- sort(wr[!duplicated(wr)])[input$ngcm]
    i.best <- which(wr<=wrmax & !duplicated(wr))
    return(i.best)
  })

  ## Temperature and precip. spread for scatterplot
  # Region 1
  dtas1 <- reactive({
    sapply(gcmst(), function(gcm) mean(sapply(Season(), function(s)
    stats()$tas[[Period()]][[gcm]][[Region1()]][["mean"]][[s]])) - 
      mean(sapply(Season(), function(s) 
        stats()$tas$present[[gcm]][[Region1()]][["mean"]][[s]]))) })
  dpr1 <- reactive({(60*60*24)*sapply(gcmsp(), function(gcm) mean(sapply(Season(), function(s)
    stats()$pr[[Period()]][[gcm]][[Region1()]][["mean"]][[s]])) - 
      mean(sapply(Season(), function(s) 
        stats()$pr$present[[gcm]][[Region1()]][["mean"]][[s]])))})
  # Region 2
  dtas2 <- reactive({
    sapply(gcmst(), function(gcm) mean(sapply(Season(), function(s)
      stats()$tas[[Period()]][[gcm]][[Region2()]][["mean"]][[s]])) - 
        mean(sapply(Season(), function(s) 
          stats()$tas$present[[gcm]][[Region2()]][["mean"]][[s]]))) })
  dpr2 <- reactive({(60*60*24)*sapply(gcmsp(), function(gcm) mean(sapply(Season(), function(s)
    stats()$pr[[Period()]][[gcm]][[Region2()]][["mean"]][[s]])) - 
      mean(sapply(Season(), function(s) 
        stats()$pr$present[[gcm]][[Region2()]][["mean"]][[s]])))})
  
  # Statistics used in the app
  # Region 1
  spreadPr1 <- reactive({60*60*24*spread(varid="pr", rcp=input$rcp, season=input$season, 
                                   region=input$regionwm1, period=Period(), im=input$baseensemble)})
  spreadPrSel1 <- reactive({60*60*24*spread(varid="pr", rcp=input$rcp, season=input$season, 
                                      region=input$regionwm1, period=Period(), im=gcmsSelected())})
  spreadTas1 <- reactive({spread(varid="tas", rcp=input$rcp, season=input$season, 
                           region=input$regionwm1, period=Period(), im=input$baseensemble)})
  spreadTasSel1 <- reactive({spread(varid="tas", rcp=input$rcp, season=input$season, 
                                    region=input$regionwm1, period=Period(), im=gcmsSelected())})
  spreadPrRel1 <- reactive({spreadPrSel1()/spreadPr1()})
  spreadPrIndx1 <- reactive({as.integer(mean(spreadPrRel1())*10)+1 })
  spreadTasRel1 <- reactive({spreadTasSel1()/spreadTas1()})
  spreadTasIndx1 <- reactive({as.integer(mean(spreadTasRel1())*10)+1 })
  # Region 2
  spreadPr2 <- reactive({60*60*24*spread(varid="pr", rcp=input$rcp, season=input$season, 
                                        region=input$regionwm2, period=Period(), im=input$baseensemble)})
  spreadPrSel2 <- reactive({60*60*24*spread(varid="pr", rcp=input$rcp, season=input$season, 
                                           region=input$regionwm2, period=Period(), im=gcmsSelected())})
  spreadTas2 <- reactive({spread(varid="tas", rcp=input$rcp, season=input$season, 
                                region=input$regionwm2, period=Period(), im=input$baseensemble)})
  spreadTasSel2 <- reactive({spread(varid="tas", rcp=input$rcp, season=input$season,
                                    region=input$regionwm2, period=Period(), im=gcmsSelected())})
  spreadPrRel2 <- reactive({spreadPrSel2()/spreadPr2()})
  spreadPrIndx2 <- reactive({as.integer(mean(spreadPrRel2())*10)+1 })
  spreadTasRel2 <- reactive({spreadTasSel2()/spreadTas2()})
  spreadTasIndx2 <- reactive({as.integer(mean(spreadTasRel2())*10)+1 })
  
  # Generate table with selected GCMs and their ranking
  gcmtable <- reactive({
    if(any(imSelected())) {
      #x <- gcmlabel(gcmst())
      #i <- x$exp %in% clean(input$rcp)
      #gcms <- paste(x$cmip[i],x$gcm[i],x$rip[i],sep=".")[imSelected()]
      gcms <- gcmnamesRanks()[imSelected()]
      wr <- weightedrank()[imSelected()]
      wr <- wr[!duplicated(gcms)]
      gcms <- gcms[!duplicated(gcms)]
      Z <- data.frame(gcms, wr)
    } else {
      Z <- data.frame("no selected models","-")
    }
    colnames(Z) <- c("Model name","Rank")
    return(Z)
  })
  
  gcmtableBest <- reactive({
    wr <- weightedrank()
    ngcm <- 20
    wrmax <- sort(wr[!duplicated(wr)])[ngcm]
    best <- which(wr<=wrmax & !duplicated(wr))
    gcms <- gcmnamesRanks()[best]
    wr <- weightedrank()[best]
    wr <- wr[!duplicated(gcms)]
    gcms <- gcms[!duplicated(gcms)]
    Z <- data.frame(gcms, wr)
    colnames(Z) <- c("Model name","Rank")
    return(Z)
  })

  gcmtableAll <- reactive({
    gcms <- gcmnamesRanks()
    wr <- weightedrank()
    wr <- wr[!duplicated(gcms)]
    gcms <- gcms[!duplicated(gcms)]
    Z <- data.frame(gcms, wr)
    colnames(Z) <- c("Model name","Rank")
    return(Z)
  })
  
  weightstable <- reactive({
    Z <- cbind(c(paste("primary region:<br>",input$regionwm1),
                 paste("secondary region:<br>",input$regionwm2),
                 "temperature", "precipitation",
                 "all year","dec-feb","mar-apr","jun-jul","sep-nov",
                 "bias","spatial correlation","spatial sd ratio","RMSE of annual cycle"),
               c(input$wmreg1,
                 input$wmreg2,
                 input$wmdt,input$wmdp,
                 input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson,
                 input$wmbias,input$wmsc,input$wmsd,input$wmrmse))
    colnames(Z) <- c("Parameter","Weight")
    return(Z)
  })

  ## Calculations used for text colors
  #legcols <- two.colors(n=11,start="red",end="green",middle = "orange") #colors for percentage number
  #legcols <- colorRampPalette(c("#d01c8b","#d196ba","#d7d7d7","#98c166","#4dac26"))(11)
  legcols <- rep("#2c7fb8",11)

  # Color list for scatterplot
  #colorlist <- c("red","orange","yellow","lime","green")
  colorlist <- list("#601A4A", "#EE442F", "#63ACBE", "#F9F4EC")

  output$IntroText  <- renderText({
    paste("This is a tool to help you evaluate subsets of climate models from the CMIP5 and CMIP6 ensembles.<br><br>",
          "Step 1) Select which emission scenarios to include in the base ensemble in the <i>'Model Selection'</i> menu.<br><br>", 
          "Step 2) In <i>'Settings for skill evaluation'</i>, select two focus regions ",
          "and set the weights for the regions and various meteorological parameters (temperature and precipitation at the moment), ",
	  "seasons, and skill scores. Based on your choices, a weighted <b>model skill evaluation</b> is performed and ",
          "the climate models are ranked according to their representation of the climate of the past. ",
	  "Note that small changes in weights and regions can significantly change the ranking.<br><br>",
          "Step 3) In <i>'Settings for scatterplots'</i>, select a season, time horizon, and emission scenario for the",
          "scatterplots which show the <b>spread of the regional mean climate change</b> among the models. ",
	  "Now study the scatterplots to evaluate how well the selected subset represents the full CMIP5 and/or CMIP6 ensemble. ",
	  "You can also show the model ranking as a color scale (tick the box above the first scatterplot!). ",
	  "Some additional information will be provided when you hover over the points.<br><br>",
          "Step 4) You can change the subset of models in <i>'Model Selection'</i> ",
	  "or add them by clicking the corresponding points in the scatterplots (doesn't work on mobile phones).",
	  "Our suggested approach if you are picking a subset of models is to exclude climate models that represent the climate of the past very poorly, ",
	  "and try to find a subset out of the well performing models that preserves the statistical characteristics ",
	  "(e.g., spread and mean) of climate change of the full ensemble.<br><br>",
	  "In <i>'Model selection'</i> you can further define the ensemble size and select the top ranked or a random set of GCMs.<br>",
	  "The <i>'Advanced settings'</i> let you choose the reference data sets and exclude specific models from the base ensemble.")
  })
  
  output$DisclaimerText <- renderText({
    paste("<i>Disclaimer: This is a prototype and should not be used as a basis for decision making.</i>")
  })
  
  output$RankingText  <- renderText({
    paste("<i>Start out by picking <b>two focus regions</b>. Then select <b>weights</b> (i.e., the importance) of ",
          "the regions, seasons, variables, and skill scores.",
          "A <b>performance score and model ranking</b> is then calculated for the model ensemble ",
          "based on your choices.</i>")
  })
  
  output$WeightsTable <- DT::renderDataTable({
    datatable(weightstable(), 
              caption="",#HTML("<font size=+0><b>Summary of weights</b></font>"),
              options=list(dom='t',
              pageLength=15,
              rownames=FALSE),
              escape=FALSE)
  })
  
  bg <- reactive({
    styleEqual(seq(1,length(input$baseensemble),0.5), 
                   #two.colors(n=length(input$baseensemble)*2-1, start="green",
                   #           end="red", middle = "orange"))
                   colorRampPalette(rev(c("#d01c8b","#d196ba","#d7d7d7","#98c166","#4dac26")))(length(input$baseensemble)*2-1))
  })
  
  output$ModelsTable <- DT::renderDataTable({
    
    if(input$tabletype=="Selected models") {
      datatable(gcmtable(), caption=HTML("<font size=+1><b>Ranking of the selected models</b></font>"), 
                rownames=FALSE,
                options=list(dom='t', pageLength=input$ngcm)) %>%
                formatStyle('Rank', target = 'row', backgroundColor = bg())
    } else if (input$tabletype=="Best performing models") {
      datatable(gcmtableBest(), caption=HTML("<font size=+1><b>Ranking of the best performing models</b></font>"),
                rownames=FALSE,
                options=list(dom='t', pageLength=max(input$ngcm,10))) %>%
        formatStyle('Rank', target = 'row',  backgroundColor = bg())
    } else {
      datatable(
        gcmtableAll(), caption=HTML("<font size=+1><b>Ranking of all models</b></font>"),
                rownames=FALSE, 
                options=list(dom='t',
                             pageLength=length(input$baseensemble)
                            )) %>% 
        formatStyle('Rank', target = 'row',  backgroundColor = bg())
    }
  })

  output$ModelsTableBest <- DT::renderDataTable({
    datatable(gcmtableBest(), caption=HTML("<font size=+1><b>Best performing models</b></font>"), 
              options=list(dom='t',pageLength=input$ngcm), 
              rownames=FALSE
    )
  })
  
  # Region 1
  textOut <- reactive({
      txt <- paste0("The spread of the selected models compared to the spread ",
                   "of the whole ensemble is (for the selected season, time horizon, and scenario):<br><br>",
                   " <table style='width:100%'>
                    <tr>
                    <th>Primary region (",Region1(),")</th>
                    <th>Secondary region (",Region2(),")</th>
                    <th></th>
                    <tr>
                    <td><font size = +1, font color=\"",legcols[spreadTasIndx1()],"\"><b>",
                   round(spreadTasRel1()*100),"%</b> </font></td>
                    <td><font size = +1, font color=\"",legcols[spreadTasIndx2()],"\"><b>",
                   round(spreadTasRel2()*100),"%</b> </font></td>
                    <td><b>for temperature</b></td>
                    </tr>
                    <tr>
                    <td><font size = +1, font color=\"",legcols[spreadPrIndx1()],"\"><b>",
                   round(spreadPrRel1()*100),"%</b> </font></td>
                    <td><font size = +1, font color=\"",legcols[spreadPrIndx2()],"\"><b>",
                   round(spreadPrRel2()*100),"%</b> </font></td>
                    <td><b>for precipitation</b></td>
                    </tr>
                    </table><br>")
    return(txt)
  })
  
  output$SpreadText  <- renderText({
    textOut()
  })
  
  Labeldtdpr1 <- reactive({
    paste(paste0(input$season," climate change in ",input$regionwm1,"<br>",
                 "Present day (1981-2010) to ",tolower(input$period)))
  })
  
  Labeldtdpr2 <- reactive({
    paste(paste0(input$season," climate change in ",input$regionwm2,"<br>",
                 "Present day (1981-2010) to ",tolower(input$period)))
  })
  
  output$dtdpr1Text  <- renderText({
    Labeldtdpr1()
  })

  output$dtdpr2Text  <- renderText({
    Labeldtdpr2()
  })
  
  ## Output: map 1
  output$mapm1 <- renderPlot({
    if(tolower(input$regionwm1)=="global") {
      region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
    } else {
      i.srex <- which(srex$name==input$regionwm1)
      region <- list(lon=srex$coord[[i.srex]][1,],
                     lat=srex$coord[[i.srex]][2,])
    }
    par(mgp=c(1,0.5,0),mar=c(0.2,0.2,0.2,0.2))
    plot(geoborders$x,geoborders$y,col="grey30",type="l",lwd=0.5,
         xlim=c(-180,180),ylim=c(-90,90),
         xlab="Longitude",ylab="Latitude",xaxt="n",yaxt="n")
    par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
        cex.lab=0.7,cex.axis=0.7)
    axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
    axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
    grid()
    lines(region$lon,region$lat,col="magenta",lwd=3,lty=1)
  }, width=190,height=130)
  
  ## Output: map 2
  output$mapm2 <- renderPlot({
    if(input$regionwm2 != "---"){
      if(tolower(input$regionwm2)=="global") {
        region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
      } else {
        i.srex <- which(srex$name==input$regionwm2)
        region <- list(lon=srex$coord[[i.srex]][1,],
                       lat=srex$coord[[i.srex]][2,])
      }
      par(mgp=c(1,0.5,0),mar=c(0.2,0.2,0.2,0.2))
      plot(geoborders$x,geoborders$y,col="grey30",type="l",lwd=0.5,
           xlim=c(-180,180),ylim=c(-90,90),
           xlab="Longitude",ylab="Latitude",xaxt="n",yaxt="n")
      par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
          cex.lab=0.7,cex.axis=0.7)
      axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
      axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
      grid()
      lines(region$lon,region$lat,col="magenta",lwd=3,lty=1)
    }}, width=190,height=130)

  ## Output: scatterplot of temperature and precip. change 
  
  imSelected <- reactive({
    #return(which(gcmst() %in% paste(clean(input$rcp),gcmsSelected(),sep=".")))
    return(which(gcmnamesRanks() %in% gcmsSelected()))
  })
  
  clr1 <- reactive({
    wr <- weightedrank()
    colvec <- colorRampPalette(rev(c("#d01c8b","#d196ba","#d7d7d7","#98c166","#4dac26")))(max(wr))
    colrank <- colvec[wr]
    show.ranking <- FALSE
    if(!is.null(input$show)) {
      if(any(grepl("rank",input$show))) {
        show.ranking <- TRUE
      }
    }
    if(show.ranking) {
      x <- adjustcolor(colrank, alpha.f=0.8)
    } else {
      y <- rep("black", length(colrank))
      exp <- gcmlabel(names(dtas1()))$exp
      y[exp=="rcp45"] <- colorlist[[1]]
      y[exp=="rcp85"] <- colorlist[[2]]
      y[exp=="ssp585"] <- colorlist[[3]]
      #y[imSelected()] <- colorlist[[4]]
      x <- adjustcolor(y, alpha.f=0.7)
    }
    return(x)
  })
  
  clr2 <- reactive({
    wr <- weightedrank()
    colvec <- colorRampPalette(rev(c("#d01c8b","#d196ba","#d7d7d7","#98c166","#4dac26")))(max(wr))
    colrank <- colvec[wr]
    show.ranking <- FALSE
    if(!is.null(input$show)) {
      if(any(grepl("rank",input$show))) {
        show.ranking <- TRUE
      }
    }
    if(show.ranking) {
      x <- adjustcolor(colrank, alpha.f=0.95)
    } else {
      y <- rep("black", length(colrank))
      exp <- gcmlabel(names(dtas1()))$exp
      y[exp=="rcp45"] <- colorlist[[1]]
      y[exp=="rcp85"] <- colorlist[[2]]
      y[exp=="ssp585"] <- colorlist[[3]]
      y[imSelected()] <- colorlist[[4]]
      x <- adjustcolor(y, alpha.f=0.95)
    }
    return(x)
  })
  
  sz <- reactive({
    x <- rep(8, length(gcmnamesRanks()))
    x[imSelected()] <- 12
    return(x)
  })
  
  smbl <- reactive({
    x <- gcmlabel(names(dtas1()))
    s <- rep("circle-open", length(x))
    s[x$exp=="rcp45"] <- "circle"
    s[x$exp=="rcp85"] <- "diamond"
    s[x$cmip=="CMIP6"] <- "cross"
    return(s)
  })
  
  i_rcp45 <- reactive({
    x <- gcmlabel(names(dtas1()))
    return(x$exp=="rcp45")
  })

  myPlotlyGraph <- reactive({
    ggplotly(qplot(1:10))
  })
    
  plab <- reactive({
    x <- gcmlabel(names(dtas1()))
    y <- paste0(x$gcm,".",x$rip," (",toupper(x$cmip)," ",toupper(x$exp),")",
                "\nWeighted rank: ",weightedrank())
    return(y)
  })

  output$dtdpr1 <- renderPlotly({
    dtdpr1()
  })
    
  dtdpr1 <- reactive({
    x <- gcmlabel(names(dtas1()))
    im.rcp45 <- x$exp=='rcp45'
    im.rcp85 <- x$exp=='rcp85'
    im.ssp585 <- x$exp=='ssp585'
    show.distribution <- FALSE
    if(!is.null(input$show)) {
      if(any(grepl("distribution",input$show))) {
        show.distribution <- TRUE
      }
    }
    if(show.distribution) {
      im.list <- list(imSelected(), im.rcp45, im.rcp85, im.ssp585)
      clr.list <- list(colorlist[[4]], colorlist[[1]], colorlist[[2]], colorlist[[3]])
      ln.list <- list("black", colorlist[[1]], colorlist[[2]], colorlist[[3]])
      nm.list <- list("selected models", "RCP4.5", "RCP8.5", "SSP5 8.5")
      ivec <- which(sapply(im.list, function(x) sum(x)>0))
      px <- NULL
      py <- NULL
      for(i in ivec) {
        if(is.null(px)) {
          px <- plot_ly(x=dtas1()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
              line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])
        } else {
          px <- px %>% add_trace(x=dtas1()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
              line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])
        }
        if(is.null(py)) {
          py <- plot_ly(y=dpr1()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
              line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])          
        } else {
          py <- py %>% add_trace(y=dpr1()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
              line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])          
        }
      }
    }
    pscatter <- plot_ly(data.frame(x=dtas1()[im.rcp45],y=dpr1()[im.rcp45]), x=~x, y=~y, type="scatter", mode="markers",
                        marker=list(color=clr1()[im.rcp45], size=sz()[im.rcp45], symbol="circle", 
                                    line=list(color=clr1()[im.rcp45], width=1.2)),
                        text=plab()[im.rcp45], source="A", name="RCP4.5") %>%
      event_register("plotly_click") %>%
      add_trace(x=dtas1()[im.rcp85], y=dpr1()[im.rcp85], name="RCP8.5", text=plab()[im.rcp85],
                marker=list(color=clr1()[im.rcp85], size=sz()[im.rcp85], symbol="diamond", 
                            line=list(color=clr1()[im.rcp85], width=1.2))) %>%
      add_trace(x=dtas1()[im.ssp585], y=dpr1()[im.ssp585], name="SSP5 8.5", text=plab()[im.ssp585],
                marker=list(color=clr1()[im.ssp585], size=sz()[im.ssp585], symbol="cross", 
                            line=list(color=clr1()[im.ssp585], width=1.2))) %>%
      add_trace(x=dtas1()[imSelected()], y=dpr1()[imSelected()], name="selected models", text=plab()[imSelected()],
                marker=list(color=clr2()[imSelected()], 
                            size=sz()[imSelected()], symbol=smbl()[imSelected()], 
                            line=list(color="black", width=1.2))) %>%
      layout(p, font=list(size=15),
             xaxis=list(title="Temperature change (deg C)",range=input$xlim,
                        zerolinecolor="#bdbdbd", zerolinewidth=1),
             yaxis=list(title="Precipitation change (mm/day)",range=input$ylim,
                        zerolinecolor="#bdbdbd", zerolinewidth=1),
             showlegend=TRUE, 
             legend=list(orientation="h",  xanchor="left", x = 0.1, y=-0.2, sz=4))
    if(show.distribution) {
      p <- subplot(
        px, plotly_empty(type="scatter", mode="markers"),
        pscatter, py,
        nrows = 2, heights = c(0.1, 0.9), widths = c(0.9, 0.1), margin = 0,
        shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
    } else {
      p <- pscatter
    }
  })
  
  output$dtdpr2 <- renderPlotly({
    dtdpr2()
  })


  # Region 2
  dtdpr2 <- reactive({
    x <- gcmlabel(names(dtas1()))
    im.rcp45 <- x$exp=='rcp45'
    im.rcp85 <- x$exp=='rcp85'
    im.ssp585 <- x$exp=='ssp585'
    show.distribution <- FALSE
    if(!is.null(input$show)) {
      if(any(grepl("distribution",input$show))) {
        show.distribution <- TRUE
      }
    }
    if(show.distribution) {
      im.list <- list(imSelected(), im.rcp45, im.rcp85, im.ssp585)
      clr.list <- list(colorlist[[4]], colorlist[[1]], colorlist[[2]], colorlist[[3]])
      ln.list <- list("black", colorlist[[1]], colorlist[[2]], colorlist[[3]])
      nm.list <- list("selected models", "RCP4.5", "RCP8.5", "SSP5 8.5")
      ivec <- which(sapply(im.list, function(x) sum(x)>0))
      px <- NULL
      py <- NULL
      for(i in ivec) {
        if(is.null(px)) {
          px <- plot_ly(x=dtas2()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
                        line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])
        } else {
          px <- px %>% add_trace(x=dtas2()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
                                 line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])
        }
        if(is.null(py)) {
          py <- plot_ly(y=dpr2()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
                        line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])          
        } else {
          py <- py %>% add_trace(y=dpr2()[im.list[[i]]], type="box", color=I(clr.list[[i]]), 
                                 line=list(color=ln.list[[i]]), showlegend=FALSE, boxmean=TRUE, name=nm.list[[i]])          
        }
      }
    }
    pscatter <- plot_ly(data.frame(x=dtas2()[im.rcp45],y=dpr2()[im.rcp45]), x=~x, y=~y, type="scatter", mode="markers",
                        marker=list(color=clr1()[im.rcp45], size=sz()[im.rcp45], symbol="circle", 
                        line=list(color=clr1()[im.rcp45], width=1.2)),
                        text=plab()[im.rcp45], source="A", name="RCP4.5") %>%
      event_register("plotly_click") %>%
      add_trace(x=dtas2()[im.rcp85], y=dpr2()[im.rcp85], name="RCP8.5", text=plab()[im.rcp85],
                marker=list(color=clr1()[im.rcp85], size=sz()[im.rcp85], symbol="diamond", 
                            line=list(color=clr1()[im.rcp85], width=1.2))) %>%
      add_trace(x=dtas2()[im.ssp585], y=dpr2()[im.ssp585], name="SSP5 8.5", text=plab()[im.ssp585],
                marker=list(color=clr1()[im.ssp585], size=sz()[im.ssp585], symbol="cross", 
                            line=list(color=clr1()[im.ssp585], width=1.2))) %>%
      add_trace(x=dtas2()[imSelected()], y=dpr2()[imSelected()], name="selected models", text=plab()[imSelected()],
                marker=list(color=clr2()[imSelected()],#colorlist[[4]] 
                            size=sz()[imSelected()], symbol=smbl()[imSelected()], 
                            line=list(color="black", width=1.2))) %>%
      layout(p, font=list(size=15),
             xaxis=list(title="Temperature change (deg C)",range=input$xlim,
                        zerolinecolor="#bdbdbd", zerolinewidth=1),
             yaxis=list(title="Precipitation change (mm/day)",range=input$ylim,
                        zerolinecolor="#bdbdbd", zerolinewidth=1),
             showlegend=TRUE, 
             legend=list(orientation="h",  xanchor="left", x = 0.1, y=-0.2, sz=4))
    if(show.distribution) {
      p <- subplot(
        px, plotly_empty(type="scatter", mode="markers"),
        pscatter, py,
        nrows = 2, heights = c(0.1, 0.9), widths = c(0.9, 0.1), margin = 0,
        shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
    } else {
      p <- pscatter
    }
  })
  
  output$clickevent <- renderPrint({
    event_data("plotly_click", source="A")
  })

  # Link to skill evaluation from introduction text
  observeEvent(input$link_to_selection, {
    js$refocus("ngcm")
  })
  
  # When selecting GCMs in plotly scatterplot, update gcms and ngcm
  observe({
    d <- event_data(event="plotly_click", source="A")
    if(!is.null(d)) {
      exp <- names(gcmnames.all)[d$curveNumber+1]
      x <- gcmlabel(names(dtas1()))
      y <- names(dtas1())[x$exp==exp]
      z <- y[d$pointNumber+1]
      selected <- unique(c(input$gcms, gsub("ssp[0-9]{3}.|rcp[0-9]{2}.","",z)))
      choices <- gcmnamesRanks()
      choices <- choices[!duplicated(choices)]
      updateCheckboxGroupInput(session, inputId = "gcms", 
                               choices = choices, selected = selected)
      updateNumericInput(session, inputId = "ngcm", value=length(selected), 
                         min=1, max=length(choices))
    }
  })

  ## This doesn't work. Don't know how to access the plot (p) after rendering it
  ## When changing range in plotly scatterplot, update xlim and ylim
  #selectedPlot <- reactive({
  #  p <- plotlyOutput('dtdpr1')
  #  plotly_data()
  #  output$dtdpr
  #})
  #
  #observe({
  #  d <- event_data(event="plotly_relayout", source="A")
  #  if(!is.null(d)) {
  #    p <- selectedPlot()
  #    xlim <- p$x$layoutAttrs[[1]]$xaxis$range
  #    ylim <- p$x$layoutAttrs[[1]]$yaxis$range
  #    updateSliderInput(session, inputId = "xlim", value = xlim)
  #    updateSliderInput(session, inputId = "ylim", value = ylim)
  #  }
  #})
  
  # When changing RCP, change list of GCMs. 
  observeEvent(input$rcp, {
    #x <- gcmlabel(gcmst())
    #i <- x$exp %in% clean(input$rcp)
    #choices <- paste(x$cmip[i],x$gcm[i],x$rip[i],sep=".")
    choices <- gcmnamesRanks()
    choices <- choices[!duplicated(choices)]
    selected <- NULL
    if(length(input$gcms)>0) {
      if(any(input$gcms %in% choices)) {
        selected <- choices[choices %in% input$gcms]
      }
    }
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices, 
                             selected = selected)
  })

  # When selecting GCMs from the checkboxes, update ngcm
  observeEvent(input$gcms,{
    #x <- gcmlabel(gcmst())
    #i <- x$exp %in% clean(input$rcp)
    #choices <- paste(x$cmip[i],x$gcm[i],x$rip[i],sep=".")
    choices <- gcmnamesRanks()
    choices <- choices[!duplicated(choices)]
    updateNumericInput(session, inputId = "ngcm", 
                       value=length(input$gcms), min=1, max=length(choices))
  })
  
  # When clicking 'best' button, select best performing GCMs
  observeEvent(input$best, {
    #x <- gcmlabel(gcmst())
    #i <- x$exp %in% clean(input$rcp)
    #choices <- paste(x$cmip[i],x$gcm[i],x$rip[i],sep=".")
    choices <- gcmnamesRanks()
    selected <- choices[best()]
    choices <- choices[!duplicated(choices)]
    selected <- selected[!duplicated(selected)]
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices,
                             selected = selected)
  })

  # When clicking 'random' button, select random GCMs
  observeEvent(input$randomize, {
    #x <- gcmlabel(gcmst())
    #i <- x$exp %in% clean(input$rcp)
    #choices <- paste(x$cmip[i],x$gcm[i],x$rip[i],sep=".")
    choices <- gcmnamesRanks()
    choices <- choices[!duplicated(choices)]
    i <- sample(1:length(choices),input$ngcm,replace=FALSE)
    selected <- choices[i]
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices,
                             selected = selected)
  })
  
  # When clicking 'deselect' button, deselect all GCMs
  observeEvent(input$deselect, {
    #x <- gcmlabel(gcmst())
    #i <- x$exp %in% clean(input$rcp)
    #choices <- paste(x$cmip[i],x$gcm[i],x$rip[i],sep=".")
    choices <- gcmnamesRanks()
    choices <- choices[!duplicated(choices)]
    selected <- NULL
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices,
                             selected = selected)
  })
  
  # Reset plotly clicks when changing GCM selection (gcms)  
  observeEvent(input$gcms,{
    js$resetClick()
  })
  
  # Update gcmnames when changing the base ensemble
  observeEvent(input$baseensemble,{
    choices <- gcmnamesRanks()
    choices <- choices[!duplicated(choices)]
    selected <- NULL
    if(length(input$gcms)>0) {
      if(any(choices %in% input$gcms)) {
        selected <- choices[choices %in% input$gcms]
      }
    }
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices, 
                             selected = selected)
  })

  # Contact us - send email
  observeEvent(input$goButton, {
    if(input$goButton==0) {
      return(NULL)
    } else {
      isolate({
        contactus(name=input$name, org=input$org, from=input$email,
                  to="kajsamp@met.no", body=input$body)
      })
      session$sendCustomMessage(type = 'testmessage',
        message = 'Thank you for your comment!')
    }
  })
  
})
