dashboardPage(
  skin = HTML("blue"),
  dashboardHeader(
    title = "GCMeval: a tool for climate model ensemble evaluation",
    titleWidth = '600px',
    dropdownMenuOutput("messageMenu")
  ),
  dashboardSidebar(
    sidebarMenu(width="120px",
            menuItem("Focus regions", tabName="rank", icon=NULL,
                selectInput(
                  "regionwm1", 
                  label = "Primary focus region",
                  choices = regionlist,
                  selected = "North Europe [NEU:11]"
                ),
                br(),
                plotOutput("mapm1", width = '100%', height = '130px'),
                br(),
                selectInput(
                  "regionwm2", 
                  label = "Primary focus region",
                  choices = regionlist,
                  selected = "global"
                ),
                br(),
                plotOutput("mapm2", width = '100%', height = '130px'),
                br()
            ),
            menuItem("Weights for skill evaluation", tabName="rank", icon=NULL,
               menuItem("Focus regions", tabName="regionwm", icon=NULL,
                  selectInput(
                    "wmreg1",
                    label = "Primary focus region",#NULL,
                    choices = c(
                      "Not considered (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 2
                  ),
                  selectInput(
                    "wmreg2",
                    label = "Secondary focus region",#NULL,
                    choices = c(
                      "Not considered (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 1
                  )
              ),
              menuItem("Variables", tabName="varwm", icon=NULL,
                 selectInput(
                   "wmdt",
                   label = "Temperature",
                   choices = c(
                     "Not considered (0)" = 0,
                     "Important (1)" = 1,
                     "Very important (2)" = 2
                   ),
                   selected = 1
                 ),
                 selectInput(
                   "wmdp",
                   label = "Precipitation",
                   choices = c(
                     "Not considered (0)" = 0,
                     "Important (1)" = 1,
                     "Very important (2)" = 2
                   ),
                   selected = 1
                 )
              ),
              menuItem("Seasons", tabName="seasonwm", icon=NULL,
                selectInput(
                  "wmann",
                  label = "Annual",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ),
                selectInput(
                  "wmdjf",
                  label = "Winter (DJF)",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ),
                selectInput(
                  "wmmam",
                  label = "Spring (MAM)",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ),
                selectInput(
                  "wmjja",
                  label = "Summer (JJA)",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ),
                selectInput(
                  "wmson",
                  label = "Autumn (SON)",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                )
              ),
              menuItem("Skill scores", tabName="skillwm", icon=NULL,
                selectInput(
                  "wmbias",
                  label = "Bias",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ),
                selectInput(
                  "wmsc",
                  label = "Spatial correlation",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ),
                selectInput(
                  "wmsd",
                  label = "Spatial sd ratio",
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ),
                selectInput(
                  "wmrmse",
                  label = HTML("RMSE of annual cycle"),
                  choices = c(
                    "Not considered (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                )
              )
      ),
      menuItem("Settings for scatterplot", tabName="spread", icon=NULL,
              selectInput(
                "season",
                label = "Season",
                choices = c("Annual mean", "Winter (DJF)", "Spring (MAM)", "Summer (JJA)", "Autumn (SON)"),
                selected = "Annual mean"
              ),
              selectInput(
                "period",
                label = "Time horizon",
                choices = c("Far future (2071-2100)",
                            "Near future (2021-2050)"),
                selected = "Far future (2071-2100)"
              ),
              sliderInput("xlim", 
                          label = "Temperature range",
                          min = -20, 
                          max = 20,
                          step = 0.25,
                          value = c(-3,15)),
              sliderInput("ylim", 
                          label = "Precipitation range",
                          min = -4, 
                          max = 4,
                          step = 0.25,
                          value = c(-0.5,1))
      ),
      menuItem("Ensemble selection", tabName = "selection", icon=NULL, collapsed=FALSE, width='210px',
               checkboxGroupInput("rcp",
                 label = "Emission scenarios in base ensemble",
                 choiceNames = c("RCP4.5 (CMIP5)", "RCP8.5 (CMIP5)", "SSP5 8.5 (CMIP6)"),
                 choiceValues = c("rcp45", "rcp85", "ssp585"),
                 selected = c("rcp85","ssp585")
               ),
               numericInput("ngcm",
                            label = "Ensemble size",
                            value = 10, min = 1,
                            max = length(gcmnames),
                            width = '150px'
               ),
               actionButton("randomize", 
                            label = "Random", 
                            width = '150px'
               ),
               actionButton("best", 
                            label = "Best", 
                            width = '150px'
               ),
               actionButton("deselect",
                            label = "Deselect all",
                            width = '150px'),
               checkboxGroupInput("gcms",
                                  label = "Climate models",
                                  choices = gcmnames.all[["rcp85"]],
                                  selected = gcmnames.all[["rcp85"]][1:10],
                                  inline=TRUE,
                                  width='100%'
               )
      ),                
      menuItem("Advanced settings", tabName="advanced", icon=icon("cog"),
               selectInput(
                 "tasref", 
                 label = "Reference data set, temperature",
                 choices = c("ERA5","ERAinterim"),
                 selected = "ERA5"
               ),
               selectInput(
                 "prref", 
                 label = "Reference data set, precipitation",
                 choices = c("ERA5","ERAinterim","GPCP"),
                 selected = "GPCP"
               ),
               menuItem("Exclude climate models", tabname="ensemble", icon=NULL,
                checkboxGroupInput(
                  "baseensemble",
                  label = "Full ensemble",
                  choices = gcmnames,
                  selected = gcmnames
                )
              )
      )
    )
  ),
  dashboardBody(fluidPage(
    theme = "bootstrap.css",
    useShinyjs(),
    extendShinyjs(text = jsresetclick, functions = c("resetClick")),
    extendShinyjs(text = jsrefocus, functions = "refocus"),
    fluidRow(
      column(
        12,
        box(
          title = HTML("<font size=+1.5 color='black'><b>Getting started</b></font>"),
          width = '100%' ,
          status = 'primary',
          collapsible = TRUE,
          collapsed = FALSE,
       	  htmlOutput("IntroText"),
          br(),
          box(
            title=HTML("<font size=+0>Data and source code</font>"),
            width = '100%' ,
            status = 'info',
            collapsible = TRUE,
            collapsed = TRUE,
          HTML("Global Climate Model (GCM) data:<br>"),
       	  a("Coupled Model Intercomparison Project Phase 5 (CMIP5)",
       	    href = "https://esgf-node.llnl.gov/projects/cmip5/"), br(),
	        a("Coupled Model Intercomparison Project Phase 6 (CMIP6)",
	    href="https://www.wcrp-climate.org/wgcm-cmip/wgcm-cmip6"),
       	  HTML("<br><br>Reference data:<br>"),
       	  a("ERA5", href = "https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5"),
       	   "(temperature and precipitation)", br(),
       	  a("ERAinterim", href = "https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5"),
       	  "(temperature and precipitation)", br(),
       	  a("GPCP v2.3", href = "https://www.esrl.noaa.gov/psd/data/gridded/data.gpcp.html"),
       	  "(precipitation)",
	  br(),
          HTML("The source code for this app is available at GitHub: "),
          a("http://github.com/metno/gcmeval/.", href = "https://github.com/metno/gcmeval/")	  
        )
      )
      ),
      column(
        12,
        box(
          label="skill",
          title = HTML("<font size=+1.5 color='black'><b>Model skill evaluation</b></font>"),
          width = '100%' ,
          status = 'primary',
          collapsible = TRUE,
          collapsed = FALSE,
          column(8,
                 selectInput("tabletype", 
                             label="Show ranking of models",
                             choices=c("Selected models","Best performing models","All models"))
          ),
          column(12,
                 DT::dataTableOutput("ModelsTable"),
                 br()
                 ),
          column(12,
                box(
                  label="weights",
                  title = HTML("<font size=+0>Summary of weights</font>"),
                  width = '100%' ,
                  status = 'primary',
                  collapsible = TRUE,
                  collapsed = TRUE,
                  DT::dataTableOutput("WeightsTable"),
                  br()
                )
          )
        )
      ),
      column(
        12,
        box(
          label="spread",
          title = HTML("<font size=+1.5 color='black'><b>Spread of the regional mean climate change</b></font>"),
          status = 'primary',
          width = '100%',
          collapsible = TRUE,
          collapsed = FALSE,
          checkboxGroupInput(
            "show",
            label="What to show in the scatterplots",
            choiceNames = c("model ranking as color scale",
                            "distributions as box plots"),
            choiceValues = c("ranking", "distribution"),
            selected = c(NULL),
            inline = TRUE, width='100%'
          ),
          box(
              label="spread1",
              title = HTML("<font size=+0>Scatterplot for primary focus region</font>"),
              status = 'primary',
              width = '100%',
              collapsible = TRUE,
              collapsed = FALSE,
              br(),
              htmlOutput("dtdpr1Text"),
              plotlyOutput("dtdpr1", width = '100%', height = 550),
              br()
          ),
          box(
              label="spread2",
              title = HTML("<font size=+0>Scatterplot for secondary focus region</font>"),
              status = 'primary',
              width = '100%',
              collapsible = TRUE,
              collapsed = FALSE,
              br(),
              htmlOutput("dtdpr2Text"),
              plotlyOutput("dtdpr2", width = '100%', height = 550),
              br()
            ),
            box(
              label="summary",
              title = HTML("<font size=+0>Summary statistics</font>"),
              status = 'primary',
              width = '100%',
              collapsible = TRUE,
              collapsed = FALSE,
              htmlOutput("SpreadText")
            )
          )
        )
      )
    )
  )
)

