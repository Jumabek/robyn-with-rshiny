library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(shinybusy)
library(reticulate)
library(log4r)
library(glue)
library(dashboardthemes)
library(shinycssloaders)
library(plotly)

#0 ENVIRONMENT SETUP ----
prod = FALSE
if(prod == FALSE){
  setwd("C:/Users/Bastien/robyn/shiny/app")
}

##0.1 setup the logfile
logger <- create.logger()
logfile(logger) <- 'serverData.log'
level(logger) <- 'INFO'

#0.2 loading local libraries
source("train.R")
source("allocator.R")
source("plot.R")
source_python("gcpSync.py")

#0.3 remote storage
bucket = 'robyn-test-bucket'
local_model_file = 'Model.RData'
remote_model_file = 'data/Model.RData'
info(logger, glue('Remote storage set to {bucket}/{remote_model_file}'))
info(logger, glue('Prod set to {prod}'))

if(prod == FALSE){
  storage_client = storage_client_with_creds('./credentials/robyn-test-2-672df5a3c62a.json')
  info(logger, glue('Using json file as credentials'))
} else {
  #TODO install the libraries and environment as part as the dockerfile
  virtualenv_create("r-reticulate")
  virtualenv_exists("r-reticulate")
  use_virtualenv("r-reticulate", required = TRUE)
  py_install("nevergrad", pip = TRUE)
  py_install("google-cloud-storage", pip = TRUE)
  py_config()
  storage_client = storage_client_without_creds()
  info(logger, glue('Using default credentials'))
}


#1 LOGIN SCREEN ----
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Log In", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "Sign In", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: admin  Password: 55admin"),
                     br(),
                     tags$code("Username: user  Password: 55user")
                   ))
)

#TODO create remote backend table for login credentials
credentials = data.frame(
  username_id = c("admin", "user"),
  passod   = sapply(c("55admin", "55user"),password_store),
  permission  = c("admin", "user"), 
  stringsAsFactors = F
)

#2 UI ----
header <- dashboardHeader( title = "55 | Marketing Mix Modeling", titleWidth = 300, uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyDashboardThemes(theme = "grey_dark"), shinyjs::useShinyjs(), uiOutput("body"))
loader <- (options(spinner.color="#0dc5c1", spinner.type  = 7))
ui<-dashboardPage(header, sidebar, body, loader)

#3 SERVER ----
server <- function(input, output, session) {
  login = FALSE
  USER <- reactiveValues(login = login)
  
#3.1 loggin logic ----  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
              info(logger, glue('User {Username} logged in'))
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })

#3.2 reactive UI elements ----    
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
#3.2.1 sidebar 
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="admin") {
        #admin pannel ----
        sidebarMenu(
          menuItem("Model Training", tabName = "training", icon = icon("flash")),
          menuItem("Model Selection", tabName = "selection", icon = icon("search")),
          menuItem("Historical Contribution", tabName = "historic", icon = icon("signal")),
          menuItem("Budget Optimization", tabName = "optimizer", icon = icon("dashboard"), selected = T),
          menuItem("Documentation", tabName = "documentation", icon = icon("book")),
          menuItem("Logs", tabName = "logs", icon = icon("list"))
        )
      }
        
      else{
        #user pannel ----
        sidebarMenu(
          menuItem("Historical Contribution", tabName = "historic", icon = icon("signal")),
          menuItem("Budget Optimizer", tabName = "optimizer", icon = icon("dashboard"), selected = T),
          menuItem("Documentation", tabName = "documentation", icon = icon("book"))
        )
        
      }
    }
  })

#3.2.1 body  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="admin") {
        #admin body ----
        tabItems(
          #model training ----
          tabItem(tabName ="training",
            fluidRow(
              column(width = 9,
                box(width = NULL, status = "warning", h3('Model Training')),
                box(title = "Model Pareto", width = NULL, plotlyOutput('modelPareto')),
                box(title = "Model IDs", width = NULL, verbatimTextOutput('modelIds'))
              ),
              column(width = 3,
                box(width = NULL, status = "warning",
                  title = "Controls", 
                  sliderInput("iterations", "Number of Iterations:", 100, 2000, 100),
                  sliderInput("trials", "Number of Trials:", 1, 10, 1),
                  actionButton("trainButton", "Train Model")
                  
              ))
            )
          ),
          #model selection ----
          tabItem(tabName ="selection",
            fluidRow(
              column(width = 9
              ),
              column(width = 3,
                box(width = NULL, status = "warning",
                  title = "Controls",
                  actionButton("importModelButton", "Import Existing Models"),
                  uiOutput('modelSolutions'),
                  uiOutput('modelSelection')
                  
              ),
              box(width = NULL, status = "warning",
                  title = "Info",
                  verbatimTextOutput('saveMessage')
              )
              )
            )
          ),
          #historical driver contribution ----
          tabItem(tabName ="historic",
                  fluidRow(
                    column(width = 9,
                           box(
                             width = NULL, title = "Revenue per week", 
                             withSpinner(plotlyOutput('line_chart_rev_week')),
                           ),
                           box(
                             width = NULL, title = "Media Spend per week", 
                             withSpinner(plotlyOutput('bar_chart_media_spend_week')),
                           ),
                           box(
                             width = 5, title = "ROI per channel", 
                             withSpinner(plotlyOutput('channel_roi')),
                           ),
                           box(
                             width = 5, title = "ROI per channel", 
                             withSpinner(plotOutput('pie_chart_contribution')),
                           )
   
                    ),
                    column(width = 3,
                           uiOutput('dateSelector')
                    )
                  )
          ),
          #budget optimizer ----
          tabItem(tabName ="optimizer", class = "active",
            fluidRow(
              column(width = 9,
                 box(
                   width = NULL, title = "Response curve and mean spend by channel", 
                   withSpinner(plotlyOutput("p14")),
                 ),    

                 tabBox(
                   width = NULL,
                   tabPanel("Otimized mean Response",withSpinner(plotlyOutput("p12"))),
                   tabPanel("Optimized budget Allocation",withSpinner(plotlyOutput("p13"))),
                 ),
              ),

              column(width = 3,
                box(width = NULL, status = "warning",
                  title = "Scenario",
                  uiOutput('SelectedModel'),
                  "Selected marketing allocation scenario",
                  selectInput( "scenario", "Choose a scenario:",
                    c(
                      "Max historical response" = "max_historical_response",
                      "Max response expected spend" = "max_response_expected_spend"
                    )
                  ),
                  conditionalPanel(condition = "input.scenario == 'max_response_expected_spend'",
                    numericInput("expected_spend", "Expected Spend", 0),
                    numericInput("expected_spend_days", "Expected Spend Days", 0)
                  )
                ),
                box(width = NULL, status = "warning", solidHeader = TRUE,
                    title = "Channel Constraints",
                    "Range of budget multiplier based on historical allocation",
                    uiOutput('channelConstr')
                ),
                box(width = NULL, status = "warning", solidHeader = TRUE,
                    title = "Optimization",
                    "Integration of the parameters within the allocation optimization",
                    actionButton("optimizeButton", "Refresh Budget Allocation")
                )
              )
            )
          ),
          #logs ----
          tabItem(tabName ="logs",
            fluidRow(
              column(width = 9,
               box(width = NULL, status = "info", title = "Training Logs",dataTableOutput('logs'))
              ),
              column(width = 3,
                     box(width = NULL, status = "info",
                         title = "Controls",
                         actionButton("RefreshLogs", "Refresh"),
                         actionButton("resetLogs", "Reset Logs")
                     ))
            )
          )
        )
      } 
      else {
        #user body ----
        tabItem(
          tabName ="dashboard", class = "active",
          fluidRow(
            box(width = 12, dataTableOutput('results'))
          ))
        
      }
      
    }
    else {
      loginpage
    }
  })

#3.3 Processing logic ----  
#3.3.1 train model ----
  observeEvent(input$trainButton, {
    show_modal_spinner(
      "circle",
      text = "Please Wait, this might take a few minutes ...",
      color = "#2C2C2C"
    )
    info(logger, glue('Training model: {input$iterations} iterations,  {input$trials} trials'))
    Model <- Train(input$iterations, input$trials)
    #save the model object localy
    save(Model, file = "Model.RData")
    output$modelPareto <- renderPlotly(ggplotly(Model$OutputCollect$UI$pParFront))
    output$modelIds <- renderText(Model$OutputCollect$xDecompAgg$solID)
    #save the model to gcs
    info(logger, upload_blob(storage_client, bucket, local_model_file, remote_model_file))
    remove_modal_spinner()
  })
  
#3.3.2 import model ----
  observeEvent(input$importModelButton, {
    exists <- blob_exists(storage_client, bucket, remote_model_file)
    info(logger, glue('Checking if remote file exists in {bucket}: {exists}'))
    if (exists) {
      #download the model
      info(logger, download_blob(storage_client, bucket, remote_model_file, local_model_file))
      load(local_model_file)
      output$modelSolutions <- renderUI({selectInput("model_id","Select Model", Model$OutputCollect$allSolutions)})
      output$modelSelection <- renderUI({actionButton("selectModelButton", "Choose this models")})
      
      #once the model is selected, save the selected id to the model object
      observeEvent(input$selectModelButton, {
        Model$model_id <- input$model_id
        info(logger, glue("Model selected: {Model$model_id}"))
        save(Model, file = "Model.RData")
        #save the model object to GCS
        info(logger, upload_blob(storage_client, bucket, local_model_file, remote_model_file))
        output$saveMessage <- renderText(glue('Model {Model$model_id} was saved to GCS'))
        
      })
    }else{
      print('Model was not found in remote storage')
    }
  })

  
  
#3.3.3 budget optimizer ----
  print("trying to load existing model from GCS")
  exists <- blob_exists(storage_client, bucket, remote_model_file)
  if (exists) {
    #download the model
    info(logger, download_blob(storage_client, bucket, remote_model_file, local_model_file))
    load(local_model_file)
    #scalable channel constraint selectors
    output$channelConstr <- renderUI({
      lapply(Model$InputCollect$paid_media_vars, 
             function(i){sliderInput(
               inputId = paste0("constr_", i), i, 0.01, 2, value = c(0.7, 1.5), step = 0.1)
             }
      )
    })
    #if a model has already been selected
    if (!is.null(Model$model_id)){
      
      #retrieve the date range selected by the model 
      output$dateSelector <- renderUI({
        box(width = NULL, status = "warning",
            title = "Date range",
            dateInput("startDate", label = "Start Date",
                      min = Model$InputCollect$window_start,
                      max = Model$InputCollect$window_end,
                      value = Model$InputCollect$window_start),
            
            dateInput("endDate", label = "End Date", 
                      min = Model$InputCollect$window_start,
                      max = Model$InputCollect$window_end,
                      value = Model$InputCollect$window_end)
        )
      })
      #output channl ROI
      output$channel_roi <- renderPlotly({PlotHistorical(Model, input$startDate, input$endDate)$channel_roi})
      # outputu line chart
      output$line_chart_rev_week <- renderPlotly({line_chart_rev_week(Model, input$startDate, input$endDate)})
      #output bar chart media spend
      output$bar_chart_media_spend_week <- renderPlotly({bar_chart_media_spend_week(Model, input$startDate, input$endDate)})
      #pie chart media contribution
      output$pie_chart_contribution <- renderPlot({pie_chart_media_contribution(Model, input$startDate, input$endDate)})
      
      #dry run of the allocator
      AllocatorCollect <- Allocate(InputCollect = Model$InputCollect, 
                                   OutputCollect= Model$OutputCollect, 
                                   select_model = Model$model_id, 
                                   scenario ="max_historical_response",
                                   channel_constr_low = rep(0.7, each = length(Model$InputCollect$paid_media_vars)), 
                                   channel_constr_up = rep(1.5, each = length(Model$InputCollect$paid_media_vars))
      )
      output$p12 <- renderPlotly(PlotAllocator(AllocatorCollect)$p1)
      output$p13 <- renderPlotly(PlotAllocator(AllocatorCollect)$p2)
      output$p14 <- renderPlotly(PlotAllocator(AllocatorCollect)$p3)

      #new run of the allocator
      observeEvent(input$optimizeButton,{
        channel_consrt_low_val <- lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][1]})
        channel_consrt_up_val <- lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][2]})
        scenario <- input$scenario
        expected_spend <- input$expected_spend
        expected_spend_days <- input$expected_spend_days
        #run the allocator (reactive so that the loading effect takes place as soon as it is refreshed)
        AllocatorCollect <- reactive({Allocate(Model$InputCollect, 
                                               Model$OutputCollect, 
                                               Model$model_id, 
                                               scenario = scenario,
                                               channel_constr_low = unlist(channel_consrt_low_val), 
                                               channel_constr_up = unlist(channel_consrt_up_val),
                                               expected_spend = expected_spend,
                                               expected_spend_days = expected_spend_days
        )})
        
        output$p12 <- renderPlotly(PlotAllocator(AllocatorCollect())$p1)
        output$p13 <- renderPlotly(PlotAllocator(AllocatorCollect())$p2)
        output$p14 <- renderPlotly(PlotAllocator(AllocatorCollect())$p3)

      })

    }else{
      print('Model does not contain a selected model_id')
    }
  }else{
    print('Model was not found in remote storage')
  }
  
#log file ----
  #3.2.2 reactive logs
  observeEvent(input$RefreshLogs, {
    logdata <- read.delim('serverData.log',header=FALSE)
    output$logs <- renderDataTable({datatable(logdata)})
  })
  
  observeEvent(input$resetLogs, {
    file.remove('serverData.log')
    logger <- create.logger()
    logfile(logger) <- 'serverData.log'
    level(logger) <- 'INFO'
    info(logger, 'Reset log files')
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE, host='0.0.0.0', port=3838)


