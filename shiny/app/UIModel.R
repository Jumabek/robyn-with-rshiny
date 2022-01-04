# Define UI for dataset viewer app ----
library(shinythemes)


uiR <- fluidPage(
  

  theme = shinytheme("darkly"),
  # App title ----
  titlePanel("55 MMM"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #interations for the model
      sliderInput(
        inputId = 'iterations', 
        label = 'number of iteration',
        min = 100, max =2000, 
        value = c(100)
      ),  
      
      #trials for the model
      sliderInput(
        inputId = 'trials', 
        label = 'number of trials',
        min = 1, max =10, 
        value = c(1)
      ), 
      
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("runButton", "Train Model"),
      h4("Training Status"),
      textOutput("trainingStatus", style="color:blue"),
      
      helpText("Note: Once the model is ready",
               "you will be able to select a model output"),
      
      # Input: Select a model once trained ----
      uiOutput('modelSid'),
      
      actionButton("plotButton", "Plot the model output"),

      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + table of distribution ----
      h4("Observation"),
      dataTableOutput("plotMediaShare"),
      
      h4("Plot"),
      plotOutput("plot2"),
      
    )
    
  )
)
