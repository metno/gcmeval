##Create page
dashboardPage(
  skin = HTML("blue"),
  dashboardHeader(
    title = "Weighted CMIP5 model ranking and climate change spread",
    titleWidth = '600px',
    #set height for header
    #dropdownMenuOutput("messageMenu"),
    tags$li(
      class = "dropdown",
      tags$style(".main-header {height: 60px}"),
      tags$style(".main-header .logo {height: 60px}")
    )
  ),
  dashboardSidebar(
    collapsed=TRUE,
    width='110px'  
  ),
  dashboardBody(fluidPage(
    theme = "bootstrap.css",
    useShinyjs(),
    extendShinyjs(text = jsresetclick, functions = c("resetClick")),
    extendShinyjs(text = jsrefocus, functions = "refocus"),
    fluidRow(
      #column(
      #  12,
      #  box(
      #    collapsible = FALSE,
      #    collapsed = FALSE,
      #    width = '100%',
      #    a(
      #      href = 'https://climate.copernicus.eu/data-evaluation-climate-models',
      #    tags$img(src = "banner_c3s.png", width = "100%")
      #    )
      #  )
      #),
      column(
        12,
        box(label="info",
            #status = 'info',
            #title = "GCMeval: A tool for multi-model ensemble evaluation",
            collapsible = FALSE,
            collapsed = FALSE,
            width="100%",
            htmlOutput("IntroText"),
            htmlOutput("DisclaimerText")
        )
      ),
      column(
        3,
        box(label="select",
          #title = "Ensemble selection",
          width = '100%',
          status = "primary",
          collapsible=FALSE,#TRUE,
          collapsed=FALSE,
            numericInput(
              "ngcm",
              label = h5("Ensemble size"),
              value = 11, min = 1,
              max = length(gcmnames),
              width = '110px'
            ),
            h5("Model selection"),
            actionButton(
              "randomize", 
              label = "Random", 
              width = '110px'
            ),
            br(),
            actionButton(
              "best", label = "Best", width = '110px'
            ),
            br(),
            actionButton(
              "first", label = "First", width = '110px'
            ),
            br(),
            br(),
            #checkboxInput("gcm.repeat", "1 simulation per GCM", 
            #  value=FALSE,
            #  width='110px'
            #),
            checkboxGroupInput(
              "gcms",
              label = "Climate models (GCMs)",
              choices = gcmnames,
              selected = gcmnames[1:11],
              inline=TRUE,
              width='100%'
            )
        )
      ),
      column(
        9,
      column(
        12,
        box(
          label="skill",
          title = HTML("<font size=+1.5 color='black'><b>Model skill evaluation</b></font>"),
          width = '100%' ,
          status = 'primary',
          collapsible = TRUE,
          collapsed = FALSE,
          column(
            12,
            htmlOutput("RankingText"),
            #br(),
            #htmlOutput("MetricText",width='100%'),
            br()),
          column(
            12,
            box(
              title = HTML("<font size=+0.5 color='black'>Settings</font>"),
              width = "100%",
              status = "primary",
              collapsible = TRUE,
              collapsed = FALSE,
              column(
                6,
                selectInput(
                  "regionwm1",
                  label = "Primary focus region",
                  choices = regionlist,
                  selected = "North Europe [NEU:11]"
                ),
                plotOutput("mapm1", width = '100%', height = 153),
                helpText("How important is the model performance in this region?"),
                flowLayout(selectInput(
                  "wmreg1",
                  label = NA,
                  choices = c(
                    "Not important (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 2
                ))
              ),
              column(
                6,
                selectInput(
                  "regionwm2",
                  label = "Secondary focus region",
                  choices = regionlist,
                  selected = "Central Europe [CEU:12]"
                ),
                plotOutput("mapm2", width = '100%', height = 153),
                helpText("How important is the model performance in this region?"),
                flowLayout(selectInput(
                  "wmreg2",
                  label = NA,
                  choices = c(
                    "Not important (0)" = 0,
                    "Important (1)" = 1,
                    "Very important (2)" = 2
                  ),
                  selected = 1
                ))
              ),
              column(
                12,
                helpText("How important is the performance of the single variables?"),
                flowLayout(
                  selectInput(
                    "wmdt",
                    label = "Temperature",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 1
                  ),
                  selectInput(
                    "wmdp",
                    label = "Precipitation",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 1
                  )
                )
              ),
              column(
                12,
                helpText("How important is the annual and seasonal performance?"),
                flowLayout(
                  selectInput(
                    "wmann",
                    label = "Annual",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 1
                  ),
                  selectInput(
                    "wmdjf",
                    label = "Winter",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 2
                  ),
                  selectInput(
                    "wmmam",
                    label = "Spring",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 2
                  ),
                  selectInput(
                    "wmjja",
                    label = "Summer",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 2
                  ),
                  selectInput(
                    "wmson",
                    label = "Autumn",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 2
                  )
                )
              ),
              column(
                12,
                helpText("Which skill scores (w.r.t. the reference data) are important?"),
                flowLayout(
                  selectInput(
                    "wmbias",
                    label = "Bias",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 2
                  ),
                  selectInput(
                    "wmsc",
                    label = "Spatial correlation",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 1
                  ),
                  selectInput(
                    "wmsd",
                    label = "Spatial variability",
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 1
                  ),
                  selectInput(
                    "wmcmpi",
                    label = HTML("CMPI<font size=-1><sup>2</sup></font>"),
                    choices = c(
                      "Not important (0)" = 0,
                      "Important (1)" = 1,
                      "Very important (2)" = 2
                    ),
                    selected = 1
                  )
                ),
                helpText(HTML(
                  paste(
                    "<font size=-1><sup>2</sup></font>The combined model performance index summarizes the ",
                    "root mean square differences for multiple variables (following ",
                    "Gleckler et al. 2008: Performance metrics for climate models, ",
                    "J. Geophys. Res., 113, D06104, doi:10.1029/2007JD008972). ",
                    "Here, it is normalised with respect to the median rmse within the full ensemble."
                  )
                ))
              ),
              br()
            )
          ),
          column(7,
                 DT::dataTableOutput("ModelsTable"),
                 br()),
          column(5,
                 DT::dataTableOutput("ModelsTableBest"),
                 br())
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
        column(12,
               column(12,
                      htmlOutput("ScatterText"),
                      br(),
                      br()
               ),
               column(12,
                      box(title=HTML("<font size=+0.5 color='black'>Settings</font>"), 
                          width = '100%',
                          status = 'primary',
                          collapsible = TRUE,
                          collapsed = FALSE,
                          column(6,
                                 selectInput(
                                   "region",
                                   label = "Focus region",
                                   choices = regionlist,
                                   selected = "North Europe [NEU:11]"
                                 ),
                                 plotOutput("map", width = '100%', height = 153)
                          ),
                          column(6,
                                 selectInput(
                                   "season",
                                   label = "Season",
                                   choices = c("Annual mean", "Winter", "Spring", "Summer", "Autumn"),
                                   selected = "Annual mean"
                                 ),
                                 selectInput(
                                   "period",
                                   label = "Time horizon",
                                   choices = c("Far future (2071-2100)",
                                               "Near future (2021-2050)"),
                                   selected = "Far future (2071-2100)"
                                 ),
                                 selectInput(
                                   "rcp",
                                   label = "Emission scenario",
                                   choices = c("RCP 4.5", "RCP 8.5"),
                                   selected = "RCP 4.5"
                                 ),
                                 #checkboxInput(
                                 #  "weighted",
                                 #  label = HTML("<font size=-1><i> show weighted mean statistics </i></font>"),
                                 #  value = FALSE
                                 #),
                                 br()
                          ))),
               column(
                 12,
                 #useShinyjs(),
                 #extendShinyjs(text = jsresetclick, functions = c("resetClick")),
                 checkboxInput("show.ranking", 
                               label=HTML("<font size=-1<i>Show model ranking as color scale</i></font>"), 
                               value=FALSE),
                 br(),
                 plotlyOutput("dtdpr", width = '100%', height = 550),
                 br(),
                 br()
                 #verbatimTextOutput("clickevent")
               ),
               column(
                 12,
                 htmlOutput("SpreadText"),
                 br()
               )
        )
      )
      )
    ),
    column(
      12,
      box(
        title = HTML('<font size=+1.5 color=black>Info</font>'),
        width = '100%' ,
        status = 'primary',
        collapsible = TRUE,
        collapsed = FALSE,
        h4("Method"),
        "Put some reference to the methods here.",
        h4("Source code"),
          "The source code for this app is available at GitHub ",
        a("(http://github.com/metno/DECM/).",
          href = "https://github.com/metno/DECM/"),
        "This code is partially based on the R-package 'esd' which is also freely available",
        a("(http://github.com/metno/esd/).", href =
            "https://github.com/metno/esd")
      )
  )
  )
)
)
)

  #tags$head(tags$style(HTML("
  #  .skin-blue .main-sidebar {
  #      background-color:  lightblue;
  #      color: black;
  #                          }")))
#)


