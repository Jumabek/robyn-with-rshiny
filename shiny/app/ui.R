ui <- navbarPage(
  "55 Marketing Mix Modeling",
  theme = shinytheme("darkly"),
  #tab 1model training ----
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
             
             mainPanel(
               h4("Plot Nevergrad output")
             )
             
           )),
  #tab 2 model selection ----
  tabPanel("Model Selection",
           sidebarLayout(
             sidebarPanel(
               # Input: Select a model once trained ----
               uiOutput('selectModelSid'),
               actionButton("plotButton", "Plot the model output"),

             ),
             mainPanel(
               plotOutput('plotWaterfall')
             )
           )),
  #tab 2 model selection ----
  tabPanel("Model Selection 2",
           sidebarLayout(
             sidebarPanel(
               # Input: Select a model once trained ----
               uiOutput('dthsrt'),
               actionButton("plotrgqerButton", "Plot the model output"),
               textOutput("trainsdrgCheck")
             ),
             mainPanel(
               plotOutput('plotWatsdqzeerfall')
             )
           ))
)