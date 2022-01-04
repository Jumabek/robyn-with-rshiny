# Define UI for dataset viewer app ----
all_fronts <- unique(OutputCollect$xDecompAgg$robynPareto)
pf = all_fronts[1]
plotMediaShare <- OutputCollect$xDecompAgg[robynPareto == pf & rn %in% InputCollect$paid_media_vars]
uniqueSol <- plotMediaShare[, unique(solID)]

ui <- fluidPage(
  

  
  # App title ----
  titlePanel("More Widgets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput(
        inputId = 'iterations', 
        label = 'number of iteration',
        min = 100, max =2000, 
        value = c(10,90)
      ),  
      
      # Input: Select a dataset ----
      selectInput("sid", "Choose a dataset:",
                  choices = uniqueSol),
      
      # Input: Specify the number of observations to view ----
      numericInput("obs", "Number of observations to view:", 10),
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + table of distribution ----
      h4("Observations"),
      tableOutput("view"),
      
      # Output: Header + summary of distribution ----
      h4("Plot"),
      plotOutput("plot1"),
      plotOutput("plot2")
      

    )
    
  )
)
