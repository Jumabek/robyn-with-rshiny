ui <- navbarPage(
  "55 Marketing Mix Modeling",
  theme = shinytheme("sandstone"),
  #tab training the model----
  tabPanel("Model Training",
           sidebarLayout(
             sidebarPanel(
               #interations for the model
               sliderInput(
                 inputId = 'iterations',
                 label = 'number of iteration',
                 min = 100,
                 max = 2000,
                 value = c(100)
               ),
               
               #trials for the model
               sliderInput(
                 inputId = 'trials',
                 label = 'number of trials',
                 min = 1,
                 max = 10,
                 value = c(1)
               ),
               
               helpText(
                 "Note: while the data view will show only the specified",
                 "number of observations, the summary will still be based",
                 "on the full dataset."
               ),
               actionButton("trainButton", "Train Model"),
               textOutput('trainingStatus')
             ),
             
             mainPanel(h4("Plot Pareto Nevergrad output"),
                       plotOutput('paretoPlot'))
             
           )),
  #tab 2 model selection ----
  tabPanel("Model Selection",
           sidebarLayout(
             sidebarPanel(
               uiOutput('selectModelSid'),
               actionButton("loadDataButton", "Load Data"),
               actionButton("plotButton", "Plot model output"),
               actionButton("useModelButton", "Use this model"),
               
               
             ),
             mainPanel(plotOutput('plotWaterfall'))
           )),
  
  #tab 3 budget optimizer ----
  tabPanel(
    "Allocation Optimizer",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "scenario",
          "Choose a scenario:",
          c(
            "Max historical response" = "max_historical_response",
            "Max response expected spend" = "max_response_expected_spend"
          )
        ),
        # Only show this panel if the plot type is a histogram
        conditionalPanel(
          condition = "input.scenario == 'max_response_expected_spend'",
          numericInput("expected_spend", "Expected Spend", 0),
          numericInput("expected_spend_days", "Expected Spend Days", 0)
        ),
        
        actionButton("optimizeButton", "Optimize Budget"),
        
      ),
      mainPanel(# Output: Tabset w/ plot, summary, and table
        tabsetPanel(
          type = "tabs",
          tabPanel("Budget Allocation", plotOutput("p12")),
          tabPanel("Mean Response", plotOutput("p13")),
          tabPanel("Response Curve", plotOutput("p14"))
        ))
      
    )
  )
)