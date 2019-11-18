## Load libraries and define functions: 
source("global.R")

## Define a server for the Shiny app
shinyServer(function(input, output, session) {

  #input <- list("rcp"=c("RCP 4.5", "RCP 8.5", "SSP585"), 
  #              "regionwm1"="Alaska/N.W. Canada [ALA:1]", "wmreg1"=2, 
  #              "regionwm2" = "global", "wmreg2"=1,
  #              "wmdt"=1, "wmdp"=1, "wmann"=1, "wmdjf"=1, "wmmam"=1, "wmjja"=1, "wmson"=1,
  #              "wmbias"=1, "wmsc"=1, "wmsd"=1, "wmrmse"=1, 
  #              "season"="Annual mean", "period"="Far future (2071-2100)", "rcp"=c("RCP 4.5", "RCP 8.5", "SSP858"),
  #              "xlim"=c(-8,8), "ylim"=c(-1.2,1.2), "ngcm"=10, "gcms"=gcmnames[1:10],
  #              "tasref"="ERA5", "prref"="GPCP", "baseensemble"=gcmnames)

  stats <- reactive({
    Y <- list()
    for(var in names(stats.all)) {
      for(rcp in input$rcp) {
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
  
  gcms.selected <- reactive({
    input$gcms
  })

  im <- reactive({
    which(input$baseensemble %in% input$gcms)
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
  tasRanks <- reactive({ranking.all(varid="tas",ref=input$tasref,rcp=input$rcp,Regions=Regionlist())})
  prRanks <- reactive({ranking.all(varid="pr",ref=input$prref,rcp=input$rcp,Regions=Regionlist())})

  gcmnames_all <- reactive({rownames(tasRanks())})
  seas_varweightedranks <- reactive({
    as.numeric(input$wmdt)*tasRanks()+as.numeric(input$wmdp)*prRanks()})
  seasweightvec <- reactive({as.numeric(c(input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson))})
  regweightvec <- reactive({
    rw <- as.numeric(c(input$wmreg1,input$wmreg2))
    rw[which(list(input$regionwm1,input$regionwm2) != "---")] 
  })
  
  weightedranks_all <- reactive({
    X <- seas_varweightedranks()
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
  weightedrank_all <- reactive({rank(weightedranks_all() %*% metweightvec())})
  
  best <- reactive({
    order(weightedrank_all())[1:input$ngcm]
  })

  ## Weighted spread calculations
  dtasSpread <- reactive({spread.all(varid="tas",rcp=input$rcp,Regions=Regionlist(),im=NULL)})
  dtasSelSpread <- reactive({spread.all(varid="tas",rcp=input$rcp,Regions=Regionlist(),im=gcms.selected())})
  dprSpread <- reactive({spread.all(varid="pr",rcp=input$rcp,Regions=Regionlist(),im=NULL)})
  dprSelSpread <- reactive({spread.all(varid="pr",rcp=input$rcp,Regions=Regionlist(),im=gcms.selected())})
  dtasRelSpread <- reactive({dtasSelSpread()/dtasSpread()})
  dprRelSpread <- reactive({dprSelSpread()/dprSpread()})
  seas_varweightedspread <- reactive({(as.numeric(input$wmdt)*dtasRelSpread() + 
                                         as.numeric(input$wmdp)*dprRelSpread())/
                                      (as.numeric(input$wmdt)+as.numeric(input$wmdp))})
  
  weightedspread_nf <- reactive({(seasweightvec() %*% seas_varweightedspread()[,1,] %*% regweightvec())/
      sum(seasweightvec())/sum(regweightvec()) })
  weightedspread_ff <- reactive({(seasweightvec() %*% seas_varweightedspread()[,2,] %*% regweightvec())/
      sum(seasweightvec())/sum(regweightvec()) })

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
  spreadPr1 <- reactive({60*60*24*spread(varid="pr", season=input$season, 
                                   region=input$regionwm1, period=Period(), im=NULL)})
  spreadPrSel1 <- reactive({60*60*24*spread(varid="pr", season=input$season, 
                                      region=input$regionwm1, period=Period(), im=gcms.selected())})
  spreadTas1 <- reactive({spread(varid="tas", season=input$season, 
                           region=input$regionwm1, period=Period(), im=NULL)})
  spreadTasSel1 <- reactive({spread(varid="tas", season=input$season, region=input$regionwm1,
                              period=Period(), im=gcms.selected())})
  spreadPrRel1 <- reactive({spreadPrSel1()/spreadPr1()})
  spreadPrIndx1 <- reactive({as.integer(mean(spreadPrRel1())*10)+1 })
  spreadTasRel1 <- reactive({spreadTasSel1()/spreadTas1()})
  spreadTasIndx1 <- reactive({as.integer(mean(spreadTasRel1())*10)+1 })
  # Region 2
  spreadPr2 <- reactive({60*60*24*spread(varid="pr", season=input$season, 
                                        region=input$regionwm2, period=Period(), im=NULL)})
  spreadPrSel2 <- reactive({60*60*24*spread(varid="pr", season=input$season, 
                                           region=input$regionwm2, period=Period(), im=gcms.selected())})
  spreadTas2 <- reactive({spread(varid="tas", season=input$season, 
                                region=input$regionwm2, period=Period(), im=NULL)})
  spreadTasSel2 <- reactive({spread(varid="tas", season=input$season, region=input$regionwm2,
                                   period=Period(), im=gcms.selected())})
  spreadPrRel2 <- reactive({spreadPrSel2()/spreadPr2()})
  spreadPrIndx2 <- reactive({as.integer(mean(spreadPrRel2())*10)+1 })
  spreadTasRel2 <- reactive({spreadTasSel2()/spreadTas2()})
  spreadTasIndx2 <- reactive({as.integer(mean(spreadTasRel2())*10)+1 })
  
  # Generate table with selected GCMs and their ranking
  gcmtable <- reactive({
    Z <- cbind(im(), gsub(".*:","",gcms.selected()),
               as.character(weightedrank()))
    #Z <- cbind(as.character(im()),
    #           gsub(".*:","",input$baseensemble[im()]),
    #           as.character(weightedrank()))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name","Rank")
    return(Z)
  })
  
  gcmtableBest <- reactive({
    Z <- cbind(as.character(best()),
               gsub(".*:","",input$baseensemble[best()]),
               as.character(seq(1,input$ngcm)))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name","Rank")
    return(Z)
  })

  gcmtableAll <- reactive({
    Z <- cbind(gsub(":.*","",input$baseensemble),
               gsub(".*:","",input$baseensemble),
               as.character(weightedrank_all()))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name","Rank")
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
  weightedrank <- reactive({weightedrank_all()[im()]})
  mean_weightedrank <- reactive({mean(weightedrank(),na.rm=TRUE)})
  
  legcolsrank <- two.colors(n=107,start="green",end="red",middle = "orange") #colors for ranks
  meanRelMetricsIndx <- reactive({as.integer(mean_weightedrank())}) #color index based on weighted rank
  
  legcols <- two.colors(n=11,start="red",end="green",middle = "orange") #colors for percentage number
  meanRelSpreadIndx_nf <- reactive({as.integer(weightedspread_nf()*10)+1}) #color index based on weighted mean rel. spread for near future
  meanRelSpreadIndx_ff <- reactive({as.integer(weightedspread_ff()*10)+1}) #color index based on weighted mean rel. spread for far future
  
  # Color list for summary boxes 
  colorlist <- c("red","orange","yellow","lime","green")

  output$IntroText  <- renderText({
    paste("This is a tool to help you evaluate subsets of climate models from the CMIP5 ensemble.<br><br>",
          "Step 1) In the <i>'Settings for skill evaluation'</i> menu, select two focus regions ",
          "and set the weights for the regions and various meteorological parameters (temperature and precipitation at the moment), ",
	  "seasons, and skill scores. Based on your choices, a weighted <b>model skill evaluation</b> is performed and ",
          "the climate models are ranked according to their representation of the climate of the past. ",
	  "Note that small changes in weights and regions can significantly change the ranking.<br><br>",
          "Step 2) In the <i>'Settings for scatterplots'</i> menu, select a season, time horizon, and emission scenario for the",
          "scatterplots which show the <b>spread of the regional mean climate change</b> among the models. ",
	  "Now study the scatterplots to evaluate how well the selected subset represents the full CMIP5 ensemble. ",
	  "You can also show the model ranking as a color scale (tick the box above the first scatterplot!). ",
	  "Some additional information will be provided when you hover over the points.<br><br>",
          "Step 3) You can change the subset of models in the <i>'Model Selection'</i> menu ",
	  "or add them by clicking the corresponding points in the scatterplots (doesn't work on mobile phones).",
	  "Our suggested approach if you are picking a subset of models is to exclude climate models that represent the climate of the past very poorly, ",
	  "and try to find a subset out of the well performing models that preserves the statistical characteristics ",
	  "(e.g., spread and mean) of climate change of the full ensemble.<br><br>",
	  "In <i>'Model selection'</i> you can further define the ensemble size and select the top ranked or a random set of GCMs.<br>",
	  "The <i>'Advanced settings'</i> let you choose the reference data sets and define the base ensemble.")
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
                   two.colors(n=length(input$baseensemble)*2-1, start="green",
                              end="red", middle = "orange"))
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
                options=list(dom='t',pageLength=input$ngcm)) %>% 
        formatStyle('Rank', target = 'row',  backgroundColor = bg())
    } else {
      datatable(
      #DT::renderDataTable # use with buttons
        gcmtableAll(), caption=HTML("<font size=+1><b>Ranking of all models</b></font>"),
                rownames=FALSE, 
                options=list(dom='t',
                             pageLength=length(input$baseensemble)#,
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
    lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
  }, width=190,height=130)#width=250, height=175)
  
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
      lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
    }}, width=190,height=130)#width=250, height=175)

  ## Output: scatterplot of temperature and precip. change 
  
  clr <- reactive({
    browser()
    colvec <- two.colors(n=length(weightedrank_all()), start="green", end="red", middle="orange")
    colrank <- colvec[weightedrank_all()]
    c1 <- rgb(116,196,215,150,maxColorValue=255)
    c2 <- rgb(0,144,168,255,maxColorValue=255)
    if(input$show.ranking) {
      x <- adjustcolor(colrank, alpha.f=0.4)
      x[im()] <- adjustcolor(colrank[im()], alpha=0.9)
    } else {
      x <- rep(c1,length(wr))
      x[im()] <- c2
    }
    return(x)
  })

  clr.line <- reactive({
    x <- clr()
    x[im()] <- "black"
    return(x)
  })
  
  sz <- reactive({
    x <- rep(8, length(input$baseensemble))
    x[im()] <- 11
    return(x)
  })
  
  myPlotlyGraph <- reactive({
    ggplotly(qplot(1:10))
  })
  
  observeEvent(input$download1, {
    filename <- paste("gcmeval",gsub("[::punct::]","",gsub(".*\\[|\\].*","",input$regionwm1)),
                      clean(input$season),clean(input$rcp),
                      gsub("[0-9]","",clean(input$period)),"png",sep=".")
    orca(dtdpr1(), file=filename, scale=3, width=1000, height=700)
  })
  
  # Region 1
  output$dtdpr1 <- renderPlotly({
    dtdpr1()
  })
  
  dtdpr1 <- reactive({
    p <- plot_ly(data.frame(x=dtas1(),y=dpr1()), x=~x, y=~y, type="scatter", mode="markers",
            marker=list(color=clr(), size=sz(), line=list(color=clr.line(), width=1.2)),
            text=paste(gcmnames_all(),"\nWeighted rank:",weightedrank_all()), source="A",
            name="Selected models") %>%
    add_trace(x=mean(dtas1()), y=mean(dpr1()), name="mean of all",
              marker=list(symbol="star", color='yellow', size=10, 
                          line=list(color='black', width=1))) %>%
    add_trace(x=mean(dtas1()[gcmnames_all() %in% im()]), y=mean(dpr1()[gcmnames_all() %in% im()]), 
              name="mean of selection",
              marker=list(symbol='star', color='red', size=10,
                          line=list(color='black', width=1))) %>%
    layout(p, font=list(size=15),
           xaxis=list(title="Temperature change (deg C)",range=input$xlim),
           yaxis=list(title="Precipitation change (mm/day)",range=input$ylim),
           showlegend=TRUE, 
           legend=list(orientation="h",  xanchor="left", x = 0.1, y=-0.2, sz=4),
	   annotations = list(yref="paper", xref="paper", y=1.07, x=0.02,
                                text=paste(paste0(input$season," climate change in ",input$regionwm1," for ",input$rcp,
                                "\nPresent day (1981-2010) to ",tolower(input$period))),
                                showarrow=FALSE, font=list(size=13,color = 'grey'),
				align="left"))
  })
  
  observeEvent(input$download2, {
    filename <- paste("gcmeval",gsub("[::punct::]","",gsub(".*\\[|\\].*","",input$regionwm2)),
                      clean(input$season),clean(paste(input$rcp,collapse="")),
                      gsub("[0-9]","",clean(input$period)),"png",sep=".")
    orca(dtdpr2(), file=filename, scale=3, width=1000, height=700)
  })
  
  # Region 1
  output$dtdpr2 <- renderPlotly({
    dtdpr2()
  })
  
  im2 <- gcmst() %in% im()
    
  # Region 2
  dtdpr2 <- reactive({
    p <- plot_ly(data.frame(x=dtas2(),y=dpr2()), x=~x, y=~y, type="scatter", mode="markers",
                 marker=list(color=clr(), size=sz(), line=list(color=clr.line(), width=1.2)),
                 text=paste(gcmst(),"\nWeighted rank:",weightedrank_all()), source="A",
                 name="Selected models") %>%
      add_trace(x=mean(dtas2()), y=mean(dpr2()), name="mean of all",
                marker=list(symbol="star", color='yellow', size=10, 
                            line=list(color='black', width=1))) %>%
      add_trace(x=mean(dtas2()[gcmst() %in% im()]), y=mean(dpr2()[gcmnames_all() %in% im()]), 
                name="mean of selection",
                marker=list(symbol='star', color='red', size=10,
                            line=list(color='black', width=1))) %>%
      layout(p, font=list(size=15),
             xaxis=list(title="Temperature change (deg C)",range=input$xlim),
             yaxis=list(title="Precipitation change (mm/day)",range=input$ylim),
             showlegend=TRUE, 
             legend=list(orientation="h",  xanchor="left", x = 0.1, y=-0.2, sz=4),
	     annotations = list(yref="paper", xref="paper", y=1.07, x=0.02,
                                text=paste(paste0(input$season," climate change in ",input$regionwm2," for ",input$rcp,
                                "\nPresent day (1981-2010) to ",tolower(input$period))),
                                showarrow=FALSE, font=list(size=13,color = 'grey'),
				align="left"))
      browser()
  })
  
  ## KMP 2019-11-18: temporarily removed plotly_click
  #output$clickevent <- renderPrint({
  #  event_data("plotly_click", source="A")
  #})

  # Link to skill evaluation from introduction text
  observeEvent(input$link_to_selection, {
    js$refocus("ngcm")
  })
  
  ## KMP 2019-11-18: temporarily removed plotly_click
  # When selecting GCMs in plotly scatterplot, update gcms and ngcm
  #observe({
  #  d <- event_data(event="plotly_click", source="A")
  #  if(!is.null(d)) {
  #    #i <- sort(unique(c(as.numeric(gsub(":.*","",input$gcms)),d$pointNumber+1)))
  #    i <- sort(unique(c(which(input$baseensemble %in% input$gcms),d$pointNumber+1)))
  #    updateCheckboxGroupInput(session, inputId = "gcms", 
  #                             choices = input$baseensemble, selected = input$baseensemble[i])
  #    updateNumericInput(session, inputId = "ngcm", value=length(input$gcms), 
  #                       min=1, max=length(input$baseensemble))
  #  }
  #})

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
  
  # When changing RCP, change list of GCMs. NO NEED TO DO THIS!
  # Only simulations available for both RCP4.5 and RCP8.5 are included.
  #observeEvent(input$rcp, {
  #  i <- seq(input$ngcm)
  #  updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, 
  #                           selected = gcmnames[i])
  #})
  
  # When selecting GCMs from the checkboxes, update ngcm
  observeEvent(input$gcms,{
    updateNumericInput(session, inputId = "ngcm", 
                       value=length(input$gcms), min=1, max=length(input$baseensemble))
  })
  
  # When clicking 'best' button, select best performing GCMs
  observeEvent(input$best, {
    i <- best()
    updateCheckboxGroupInput(session, inputId = "gcms", choices = input$baseensemble, 
                             selected = input$baseensemble[i])
  })

  # When clicking 'random' button, select random GCMs
  observeEvent(input$randomize, {
    i <- sample(1:length(gcmnames),input$ngcm,replace=FALSE)
    updateCheckboxGroupInput(session, inputId = "gcms", choices = input$baseensemble, 
                             selected = input$baseensemble[i])
  })
  
  # When clicking 'random' button, select random GCMs
  #observeEvent(input$first, {
  #  i <- 1:input$ngcm
  #  updateCheckboxGroupInput(session, inputId = "gcms", choices = input$baseensemble, 
  #                           selected = input$baseensemble[i])
  #})
  
  # Reset plotly clicks when changing GCM selection (gcms)  
  observeEvent(input$gcms,{
    js$resetClick()
  })
  
  # Update gcmnames when changing the base ensemble
  observeEvent(input$baseensemble,{
    i <- which(input$baseensemble %in% input$gcms)
    updateCheckboxGroupInput(session, inputId = "gcms", choices = input$baseensemble, 
                             selected = input$baseensemble[i])
  })
  
  # Change list of gcmnames when changing the rcp
  #observeEvent(input$rcp,{
  #  
  #})
  
})
