## Load libraries and define functions: 
source("global.R")

## Add after scatterplot: ensemble mean change in temperature and precipitation

## Define a server for the Shiny app
shinyServer(function(input, output, session) {

  stats <- reactive({
    stats.both[[clean(input$rcp)]]
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
  
  im <- reactive({
    as.numeric(gsub(":.*","",input$gcms))
  })

  ## Region selection for scatterplot
  Region <- reactive({
    if(tolower(input$region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
    }
    return(region)
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
  tasRanks <- reactive({ranking.all(stats=stats(),varid="tas",Regions=Regionlist())})
  prRanks <- reactive({ranking.all(stats=stats(),varid="pr",Regions=Regionlist())})

  seas_varweightedranks <- reactive({
    as.numeric(input$wmdt)*tasRanks()+as.numeric(input$wmdp)*prRanks()})
  seasweightvec <- reactive({as.numeric(c(input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson))})
  regweightvec <- reactive({
    rw <- as.numeric(c(input$wmreg1,input$wmreg2))
    rw[which(list(input$regionwm1,input$regionwm2) != "---")] 
  })
  
  weightedranks_all <- reactive({
    W <- array(NA,c(length(gcmnames),4))
    for (i in 1:length(gcmnames)) {
      for (j in 1:4) {
        W[i,j] <- (seasweightvec() %*% seas_varweightedranks()[i,,j,] %*% regweightvec())
      }
    }
    invisible(W)
  })
  
  metweightvec <- reactive({as.numeric(c(input$wmbias,input$wmsd,input$wmsc,input$wmcmpi))})
  weightedrank_all <- reactive({rank(weightedranks_all() %*% metweightvec())})
  
  best <- reactive({order(weightedrank_all())[1:input$ngcm]})

  ## Weighted spread calculations
  dtasSpread <- reactive({spread.all(stats(),varid="tas",Regions=Regionlist(),im=NULL)})
  dtasSelSpread <- reactive({spread.all(stats(),varid="tas",Regions=Regionlist(),im=im())})
  dprSpread <- reactive({spread.all(stats(),varid="pr",Regions=Regionlist(),im=NULL)})
  dprSelSpread <- reactive({spread.all(stats(),varid="pr",Regions=Regionlist(),im=im())})
  
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
  dtas <- reactive({
    sapply(gcmst(), function(gcm) mean(sapply(Season(), function(s)
    stats()$tas[[Period()]][[gcm]][[Region()]][["mean"]][[s]])) - 
      mean(sapply(Season(), function(s) 
        stats()$tas$present[[gcm]][[Region()]][["mean"]][[s]]))) })
  dpr <- reactive({(60*60*24)*sapply(gcmsp(), function(gcm) mean(sapply(Season(), function(s)
    stats()$pr[[Period()]][[gcm]][[Region()]][["mean"]][[s]])) - 
      mean(sapply(Season(), function(s) 
        stats()$pr$present[[gcm]][[Region()]][["mean"]][[s]])))})
  
  # Statistics used in the app
  spreadPr <- reactive({60*60*24*spread(stats(), varid="pr", season=input$season, 
                                   region=input$region, period=Period(), im=NULL)})
  spreadPrSel <- reactive({60*60*24*spread(stats(), varid="pr", season=input$season, 
                                      region=input$region, period=Period(), im=im())})
  spreadTas <- reactive({spread(stats(), varid="tas", season=input$season, 
                           region=input$region, period=Period(), im=NULL)})
  spreadTasSel <- reactive({spread(stats(), varid="tas", season=input$season, region=input$region,
                              period=Period(), im=im())})
  spreadPrRel <- reactive({spreadPrSel()/spreadPr()})
  spreadPrIndx <- reactive({as.integer(mean(spreadPrRel())*10)+1 })
  spreadTasRel <- reactive({spreadTasSel()/spreadTas()})
  spreadTasIndx <- reactive({as.integer(mean(spreadTasRel())*10)+1 })
  
  # Generate table with selected GCMs and their ranking
  gcmtable <- reactive({
    Z <- cbind(as.character(im()),
               gsub(".*:","",gcmnames[im()]),
               as.character(weightedrank()))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name","Rank")
    return(Z)
  })
  
  gcmtableBest <- reactive({
    Z <- cbind(as.character(best()),
               gsub(".*:","",gcmnames[best()]),
               as.character(seq(1,input$ngcm)))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name","Rank")
    return(Z)
  })

  gcmtableAll <- reactive({
    Z <- cbind(gsub(":.*","",gcmnames),
               gsub(".*:","",gcmnames),
               as.character(weightedrank_all()))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name","Rank")
    return(Z)
  })
  
  weightstable <- reactive({
    Z <- cbind(c("primary region","secondary region",
                 "temperature", "precipitation",
                 "all year","dec-feb","mar-apr","jun-jul","sep-nov",
                 "bias","spatial correlation","spatial variability","CMPI"),
               c(input$wmreg1,input$wmreg2,
                 input$wmdt,input$wmdp,
                 input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson,
                 input$wmbias,input$wmsc,input$wmsd,input$wmcmpi))
    colnames(Z) <- c("Parameter","Weights")
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
    paste("This is a tool for selecting and evaluating a subset of climate models from the CMIP5 ensemble.",
          "Here's how it works:<br><br>",
          "Step 1) Go to <i>'Settings for skill evaluation'</i> in the sidebar and select two focus regions ",
          "and weights for various meteorological parameters, seasons, and skill scores.<br>",
          "Step 2) Based on your choices, a weighted <b>model skill evaluation</b> is performed and ",
          "the climate models are ranked according to their representation of the climate of the past.<br>",
          "Step 3) Go to <i>'Settings for scatterplot'</i> in the sidebar and select a focus region, season, time horizon, and emission scenario.<br>",
          "Step 4) Look at the <b>scatterplot of the regional mean climate change</b> which shows the projected regional mean change in temperature and precipitation.",
          "Step 5) Now go to <i>'Model selection'</i> in the sidebar and pick a subset of models ",
          "using the information of the skill evaluation and the scatterplot.<br><br>",
          "Our suggested approach is to exclude climate models that represent the climate of the past very poorly. ",
          "Then, pick a subset out of the well performing models that preserves the statistical characteristics (the spread and mean) of climate change of the full ensemble.",
          "Small changes in the weights and regions of interest can drastically change the outcome, ",
          "so make sure to try some different settings before picking a subset of models.")
  })
  
  # Summary output
  output$value1 <- renderValueBox({
    maxrank <- mean(seq(input$ngcm))
    colorindex <- ceiling((mean_weightedrank()+1-maxrank)/(length(gcmnames)+1-maxrank)*length(colorlist))
    colorindex <- length(colorlist) + 1 - colorindex # reverse scale
    valueBox(
      formatC(mean_weightedrank(), format="d", big.mark=','),
        paste('Mean weighted rank of selected models'),
        icon = NULL,
        color = colorlist[colorindex])  
  })
  
  output$value2 <- renderValueBox({
    colorindex <- ceiling(spreadTasRel()*length(colorlist))
    valueBox(
      formatC(paste(round(spreadTasRel()*100),"%"), format="d", big.mark=','),
        paste('Relative spread of temperature change'),
        icon = NULL,
        color = colorlist[colorindex])  
  })

  output$value3 <- renderValueBox({
    colorindex <- ceiling(spreadPrRel()*length(colorlist))
    valueBox(
      formatC(paste(round(spreadPrRel()*100),"%"), format="d", big.mark=','),
        paste('Relative spread of precipitation change'),
        icon = NULL,
        color = colorlist[colorindex])  
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
  
  ## Output: text about ranking
  output$MetricText  <- renderText({
    paste("The mean weighted model rank of the selected models is  ",
          "<font size = +1, font color=\"",
          legcolsrank[meanRelMetricsIndx()],"\"><b> rank ", round(mean_weightedrank()), 
          "  of  ",length(gcmnames),"</b></font>.<br>",sep="")
  })
  
  output$WeightsTable <- renderTable({
    weightstable()
  })
  
  output$ModelsTable <- DT::renderDataTable({
    bg <- styleEqual(seq(1,length(gcmnames),0.5), 
                     two.colors(n=length(gcmnames)*2-1, start="green",
                                end="red", middle = "orange"))
    
    if(input$tabletype=="Selected models") {
      datatable(gcmtable(), caption=HTML("<font size=+1><b>Selected models</b></font>"), 
              options=list(dom='t',
                           pageLength=input$ngcm,
                           rownames=FALSE#, 
                           #extensions = 'Buttons',
			                     #buttons=list(list(extend='copy'),
			                     #             list(extend='pdf',
			                     #                  filename='CurrentTable',
			                     #                  title="Current Table",
			                     #                  header=FALSE))
			                     )) %>%
                formatStyle('Rank', target = 'row', backgroundColor = bg)
    } else if (input$tabletype=="Best performing models") {
      datatable(gcmtableBest(), caption=HTML("<font size=+1><b>Best performing models</b></font>"),
                rownames=FALSE,#, extensions="Buttons",
                options=list(dom='t',pageLength=input$ngcm#,
                             #buttons=list(list(extend='copy'),
                             #             list(extend='pdf',
                             #                  filename='CurrentTable',
                             #                  title="Current Table",
                             #                  header=FALSE))
                             )) %>% 
        formatStyle('Rank', target = 'row',  backgroundColor = bg)
    } else {
      datatable(
      #DT::renderDataTable # use with buttons
        gcmtableAll(), caption=HTML("<font size=+1><b>All models</b></font>"),
                rownames=FALSE, 
                #extensions="Buttons",
                options=list(dom='t',
                             pageLength=length(gcmnames)#,
                             #buttons=list(list(extend='copy'),
                             #             list(extend='pdf',
                             #                  filename='CurrentTable',
                             #                  title="Current Table",
                             #                  header=FALSE))
                            )) %>% 
        formatStyle('Rank', target = 'row',  backgroundColor = bg)
    }
  })

  output$ModelsTableBest <- DT::renderDataTable({
    datatable(gcmtableBest(), caption=HTML("<font size=+1><b>Best performing models</b></font>"), 
              options=list(dom='t',pageLength=input$ngcm), 
              rownames=FALSE
    )
  })
  
  textOut <- reactive({
      txt <- paste("The spread of the selected models compared to the spread ",
                   "of the whole ensemble is <br><br>",
            "<font size = +1, font color=\"",legcols[spreadTasIndx()],"\"><b>",
            round(spreadTasRel()*100),
            "%</b> </font>  for <b>temperature</b> and <br>",
            "<font size = +1, font color=\"", legcols[spreadPrIndx()],"\"><b>", 
            round(spreadPrRel()*100), 
            "%</b> </font> for <b>precipitation</b> <br><br> ",
            "for the selected region, season, time horizon, and scenario.<br><br>",sep="")
    #}
    return(txt)
  })

  ## Output: text about spread
  output$SpreadText  <- renderText({
    textOut()
  })
  
  ## Output: text about scatterplot
  output$ScatterText  <- renderText({
    paste("<i>Study the mean projected <b>temperature and precipitation changes</b> for the selected",
          " <b>focus region</b>, <b>season</b>, <b>emission scenario</b> and <b>time horizon</b> in the scatterplot below.",
          " Ideally, your chosen subset of models should have a spread similar to the spread of the whole ensemble.",
          " Try to increase the relative spread among the climate models by adding more models at the edges of the scatterplot.</i>",sep="")
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
  
  ## Output: map for scatterplot region selection
  output$map <- renderPlot({
    if(tolower(input$region)=="global") {
      region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
    } else {
      i.srex <- which(srex$name==input$region)
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
  
  ## Output: scatterplot of temperature and precip. change 
  output$dtdpr <- renderPlotly({
    
    colvec <- two.colors(n=length(dtas()), start="green", end="red", middle="orange")
    colrank <- colvec[weightedrank_all()]
    c1 <- rgb(116,196,215,150,maxColorValue=255)
    c2 <- rgb(0,144,168,255,maxColorValue=255)
    clr <- reactive({
      if(input$show.ranking) {
        x <- adjustcolor(colrank, alpha.f=0.4)
        x[im()] <- adjustcolor(colrank[im()], alpha=0.9)
      } else {
        x <- rep(c1,length(gcmnames))
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
      x <- rep(8, length(gcmnames))
      x[im()] <- 11
      return(x)
    })
    
    p <- plot_ly(data.frame(x=dtas(),y=dpr()), x=~x, y=~y, type="scatter", mode="markers",
            marker=list(color=clr(), size=sz(), line=list(color=clr.line(), width=1.2)),
            text=paste(gcmnames,"\nWeighted rank:",weightedrank_all()), source="A",
            name="Selected models") %>%
    add_trace(x=mean(dtas()), y=mean(dpr()), name="mean of all",
              marker=list(symbol="star", color='yellow', size=10, 
                          line=list(color='black', width=1))) %>%
    add_trace(x=mean(dtas()[im()]), y=mean(dpr()[im()]), 
              name="mean of selection",
              marker=list(symbol='star', color='red', size=10,
                          line=list(color='black', width=1))) %>%
    layout(p, title=paste("Present day (1981-2010) to",tolower(input$period)),
           xaxis=list(title="Temperature change (deg C)",range=input$xlim),
           yaxis=list(title="Precipitation change (mm/day)",range=input$ylim),
           showlegend=TRUE, 
           legend=list(orientation="h",  xanchor="left", x = 0.1, y=-0.2, sz=1))
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
      i <- sort(unique(c(as.numeric(gsub(":.*","",input$gcms)),d$pointNumber+1)))
      updateCheckboxGroupInput(session, inputId = "gcms", 
                               choices = gcmnames, selected = gcmnames[i])
      updateNumericInput(session, inputId = "ngcm", value=length(input$gcms), 
                         min=1, max=length(gcmnames))
    }
  })

  ## This doesn't work. Don't know how to access the plot (p) after rendering it
  ## When changing range in plotly scatterplot, update xlim and ylim
  #selectedPlot <- reactive({
  #  browser()
  #  p <- plotlyOutput('dtdpr')
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
                       value=length(input$gcms), min=1, max=length(gcmnames))
  })
  
  # When clicking 'best' button, select best performing GCMs
  observeEvent(input$best, {
    #gcmnames <- metaPrep(input$rcp)
    i <- best()
    updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, 
                             selected = gcmnames[i])
  })

  # When clicking 'random' button, select random GCMs
  observeEvent(input$randomize, {
    #gcmnames <- metaPrep(input$rcp)
    i <- sample(1:length(gcmnames),input$ngcm,replace=FALSE)
    updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, 
                             selected = gcmnames[i])
  })
  
  # When clicking 'random' button, select random GCMs
  observeEvent(input$first, {
    #gcmnames <- metaPrep(input$rcp)
    i <- 1:input$ngcm
    updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, 
                             selected = gcmnames[i])
  })
  
  # Reset plotly clicks when changing GCM selection (gcms)  
  observeEvent(input$gcms,{
    js$resetClick()
  })
  
  
  
})
