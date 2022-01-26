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
  setwd("C:/Users/Youssef/Documents/robyn/shiny/app")
}

##0.1 setup the logfile
logger <- create.logger()
logfile(logger) <- 'serverData.log'
level(logger) <- 'INFO'

#0.2 loading local libraries
source("train.R")
source("allocator.R")
source("plot.R")
virtualenv_create("r-robynshiny")
virtualenv_exists("r-robynshiny")
use_virtualenv("r-robynshiny", required = TRUE)
# py_install("nevergrad")
# py_install("google-cloud-storage")
# py_install("google.cloud")
source_python("gcpSync.py")

#0.3 remote storage
bucket = 'robyn-test-bucket'
local_model_file = 'Model.RData'
remote_model_file = 'data/Model.RData'
info(logger, glue('Remote storage set to {bucket}/{remote_model_file}'))
info(logger, glue('Prod set to {prod}'))

if(prod == FALSE){
  storage_client = storage_client_with_creds('./credentials/robyn-test-2-58e587891fc8.json')
  info(logger, glue('Using json file as credentials'))
} else {
  #TODO install the libraries and environment as part as the dockerfile
  
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
header <- dashboardHeader( title = "Marketing Budget Optimizer", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyDashboardThemes(theme = "blue_gradient"), shinyjs::useShinyjs(), uiOutput("body"))
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
          menuItem("Historical", icon = icon("signal"),
                   menuSubItem("Historical Overview", tabName = "overview"),
                   menuSubItem("Historical Contribution", tabName = "contribution")
          ),
          menuItem("Optimization", icon = icon("dashboard"),
                   menuSubItem("Budget Optimization", tabName = "optimizer", selected = T),
                   menuSubItem("Scenario Comparison", tabName = "comparison")
          ),
          menuItem("Documentation", tabName = "documentation", icon = icon("book")),
          menuItem("Logs", tabName = "logs", icon = icon("list")),
          uiOutput("dateRangeSelector"),
          verbatimTextOutput("test")
          
        )
      }
      
      else{
        #user pannel ----
        sidebarMenu(
          menuItem("Historical Overview", tabName = "overview", icon = icon("signal")),
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
                           box(width = NULL, status = "primary", h3('Model Training')),
                           box(title = "Model Pareto", width = NULL, plotlyOutput('modelPareto')),
                           box(title = "Model IDs", width = NULL, verbatimTextOutput('modelIds'))
                    ),
                    column(width = 3,
                           box(width = NULL, status = "primary",
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
                           box(width = NULL, status = "primary",
                               title = "Controls",
                               actionButton("importModelButton", "Import Existing Models"),
                               uiOutput('modelSolutions'),
                               uiOutput('modelSelection')
                               
                           ),
                           box(width = NULL, status = "primary",title = "Info",verbatimTextOutput('saveMessage'))
                    )
                  )
          ),
          #historical spend and revenue ----
          tabItem(tabName ="overview",
                  fluidRow(
                    column(width = 9,
                           box(width = NULL, title = "Revenue and Spend: Historical overview per channel",  withSpinner(plotlyOutput('WeeklyRevSpend'))),
                    ),
                    column(width = 3,
                           uiOutput('DateSelectorOverview')
                    )
                  )
          ),
          #historical driver contribution ----
          tabItem(tabName ="contribution",
                  fluidRow(
                    column(width = 9,
                           box(width = NULL, title = "Revenue Contribution", withSpinner(plotlyOutput('HistoricalRevenue'))),
                           box(width = NULL, title = "Revenue Contribution paid media", withSpinner(plotlyOutput('HistoricalRevenuepaidmedia'))),
                           box(width = NULL, title = "ROI Contribution", withSpinner(plotlyOutput('WaterfallContrib')),)
                    ),
                    column(width = 3,
                           uiOutput('DateSelectorContrib')
                    )
                  )
          ),
          #budget optimizer ----
          tabItem(tabName ="optimizer", class = "active",
                  fluidRow(
                    column(width = 9,
                           box(width = NULL, title = "Response curve and mean spend by channel", withSpinner(plotlyOutput("AllocatorCurve"))),
                           box(width = 6, title = "Otimized mean Response",withSpinner(plotlyOutput("ResponseComparison"))),
                           box(width = 6, title = "Optimized spend per channel", withSpinner(dataTableOutput("AllocatorTable"))),    
                           
                    ),
                    column(width = 3,
                           box(width = NULL, status = "primary",title = "Scenario",
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
                           box(width = NULL, status = "primary", solidHeader = TRUE,
                               title = "Channel Constraints",
                               "Range of budget multiplier based on historical allocation",
                               uiOutput('channelConstr')
                           ),
                           box(width = NULL, status = "success", solidHeader = TRUE,
                               title = "Optimization",
                               "Integration of the parameters within the allocation optimization",
                               actionButton("optimizeButton", "Refresh Budget Allocation")
                           )
                    )
                  )
          ),
          
          tabItem(tabName ="comparison", class = "active",
                  fluidRow(
                    column(width = 5,
                           h4("Scenario 1"),
                           
                           actionButton("optimizeButton1", "Refresh Budget Allocation"),
                           
                           selectInput( "scenario1", "Choose a scenario:",
                                        c(
                                          "Max historical response" = "max_historical_response",
                                          "Max response expected spend" = "max_response_expected_spend"
                                        )
                           ),
                           conditionalPanel(condition = "input.scenario1 == 'max_response_expected_spend'",
                                            numericInput("expected_spend1", "Expected Spend", 0),
                                            numericInput("expected_spend_days1", "Expected Spend Days", 0)
                           )
                           ,
                           
                           uiOutput('channelConstr1')
                           ,
                           
                           
                           withSpinner(dataTableOutput("AllocatorTable1"))
                           
                    ),
                           
                    
                           column(width = 5,
                                  h4("Scenario 2"),
                                  
                                  actionButton("optimizeButton2", "Refresh Budget Allocation"),
                                  
                                  selectInput( "scenario2", "Choose a scenario:",
                                               c(
                                                 "Max historical response" = "max_historical_response",
                                                 "Max response expected spend" = "max_response_expected_spend"
                                               )
                                  ),
                                  conditionalPanel(condition = "input.scenario2 == 'max_response_expected_spend'",
                                                   numericInput("expected_spend2", "Expected Spend", 0),
                                                   numericInput("expected_spend_days2", "Expected Spend Days", 0)
                                  )
                                  ,
                                  
                                  uiOutput('channelConstr2')
                                  ,
                                  withSpinner(dataTableOutput("AllocatorTable2"))
                    ),
                    fluidRow(
                    box(width =12,withSpinner(plotlyOutput("ResponseComparisonscenario")))
          ))
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
  
  
  
  #3.3.3 main processing ----
  print("trying to load existing model from GCS")
  #check the model exists in GCS
  exists <- blob_exists(storage_client, bucket, remote_model_file)
  exists <- TRUE
  #create date range selecor
  output$dateRangeSelector <- renderUI({
    dateRangeInput('dateRange',
                   label = 'Date range:',
                   start = Model$InputCollect$window_start, 
                   end = Model$InputCollect$window_end,
                   min = Model$InputCollect$window_start,
                   max = Model$InputCollect$window_end
    )
  })
  
  
  if (exists) {
    #download the model
    info(logger, download_blob(storage_client, bucket, remote_model_file, local_model_file))
    load(local_model_file)
    #date range selector
    
    #scalable channel constraint selectors
    output$channelConstr <- renderUI({
      lapply(Model$InputCollect$paid_media_vars, 
             function(i){sliderInput(
               inputId = paste0("constr_", i), i, 0.01, 2, value = c(0.7, 1.5), step = 0.1)
             }
      )
    })
    
    output$channelConstr1 <- renderUI({
      lapply(Model$InputCollect$paid_media_vars, 
             function(i){sliderInput(
               inputId = paste0("constr_", i), i, 0.01, 2, value = c(0.7, 1.5), step = 0.1)
             }
      )
      
      
    })
    
    output$channelConstr2 <- renderUI({
      lapply(Model$InputCollect$paid_media_vars, 
             function(i){sliderInput(
               inputId = paste0("constr_", i), i, 0.01, 2, value = c(0.7, 1.5), step = 0.1)
             }
      )
      
      
    })
    #if a model has already been selected
    if (!is.null(Model$model_id)){
      #revenue and spend over time
      output$WeeklyRevSpend <- renderPlotly({PlotWeeklyRevSpend(Model, input$dateRange[1], input$dateRange[2])})
      #Global channel revenu
      output$HistoricalRevenue <- renderPlotly({PlotHistoricalRev(Model, input$dateRange[1], input$dateRange[2])})
      #Global channel Revenue paid media
      output$HistoricalRevenuepaidmedia <- renderPlotly({PlotHistoricalRevpaidmedia(Model, input$dateRange[1], input$dateRange[2])})
      #contribution waterfall plot
      output$WaterfallContrib <- renderPlotly({PlotWaterfallContrib(Model)})
      
      
      #dry run of the allocator
      Model$OutputCollect$plot_folder <- paste0(getwd(),"/output/out/")
      AllocatorCollect <- Allocate(InputCollect = Model$InputCollect, 
                                   OutputCollect= Model$OutputCollect, 
                                   select_model = Model$model_id, 
                                   scenario ="max_historical_response",
                                   channel_constr_low = rep(0.7, each = length(Model$InputCollect$paid_media_vars)), 
                                   channel_constr_up = rep(1.5, each = length(Model$InputCollect$paid_media_vars))
      )
      AllocatorGraphs <- PlotAllocatorGraphs(Model, AllocatorCollect)
      output$AllocatorCurve <- renderPlotly(AllocatorGraphs$fig_curve)
      output$AllocatorTable <- renderDataTable(AllocatorGraphs$table_curve)
      output$ResponseComparison <- renderPlotly(AllocatorGraphs$fig_response_comparison)
      
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
        AllocatorGraphs <- reactive({PlotAllocatorGraphs(Model, AllocatorCollect())})
        output$AllocatorCurve <- renderPlotly(AllocatorGraphs()$fig_curve)
        output$AllocatorTable <- renderDataTable(AllocatorGraphs()$table_curve)
        output$ResponseComparison <- renderPlotly(AllocatorGraphs()$fig_response_comparison)
        
      })
      

      AllocatorCollect1 <- Allocate(InputCollect = Model$InputCollect, 
                                   OutputCollect= Model$OutputCollect, 
                                   select_model = Model$model_id, 
                                   scenario ="max_historical_response",
                                   channel_constr_low = rep(0.7, each = length(Model$InputCollect$paid_media_vars)), 
                                   channel_constr_up = rep(1.5, each = length(Model$InputCollect$paid_media_vars))
      )
      AllocatorGraphs1 <- PlotAllocatorGraphs(Model, AllocatorCollect1)
      output$AllocatorCurve1 <- renderPlotly(AllocatorGraphs1$fig_curve)
      output$AllocatorTable1 <- renderDataTable(AllocatorGraphs1$table_curve)
      output$ResponseComparisonscenario <- renderPlotly(responsecomparisonbar(Model,AllocatorCollect1,AllocatorCollect2))
      
      #new run of the allocator
      observeEvent(input$optimizeButton1,{
        channel_consrt_low_val <- lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][1]})
        channel_consrt_up_val <- lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][2]})
        scenario <- input$scenario1
        expected_spend <- input$expected_spend1
        expected_spend_days <- input$expected_spend_days1
        #run the allocator (reactive so that the loading effect takes place as soon as it is refreshed)
        AllocatorCollect1 <- reactive({Allocate(Model$InputCollect, 
                                               Model$OutputCollect, 
                                               Model$model_id, 
                                               scenario = scenario,
                                               channel_constr_low = unlist(channel_consrt_low_val), 
                                               channel_constr_up = unlist(channel_consrt_up_val),
                                               expected_spend = expected_spend,
                                               expected_spend_days = expected_spend_days
        )})
        AllocatorGraphs1 <- reactive({PlotAllocatorGraphs(Model, AllocatorCollect1())})
        output$AllocatorCurve1 <- renderPlotly(AllocatorGraphs1()$fig_curve)
        output$AllocatorTable1 <- renderDataTable(AllocatorGraphs1()$table_curve)
        output$ResponseComparisonscenario <- renderPlotly(responsecomparisonbar(Model,AllocatorCollect1,AllocatorCollect2))
        
      })
      
      AllocatorCollect2 <- Allocate(InputCollect = Model$InputCollect, 
                                    OutputCollect= Model$OutputCollect, 
                                    select_model = Model$model_id, 
                                    scenario ="max_historical_response",
                                    channel_constr_low = rep(0.7, each = length(Model$InputCollect$paid_media_vars)), 
                                    channel_constr_up = rep(1.5, each = length(Model$InputCollect$paid_media_vars))
      )
      AllocatorGraphs2 <- PlotAllocatorGraphs(Model, AllocatorCollect2)
      output$AllocatorCurve2 <- renderPlotly(AllocatorGraphs1$fig_curve)
      output$AllocatorTable2 <- renderDataTable(AllocatorGraphs1$table_curve)
      output$ResponseComparisonscenario <-  renderPlotly(responsecomparisonbar(Model,AllocatorCollect1,AllocatorCollect2))
      
      #new run of the allocator
      observeEvent(input$optimizeButton2,{
        channel_consrt_low_val <- lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][1]})
        channel_consrt_up_val <- lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][2]})
        scenario <- input$scenario2
        expected_spend <- input$expected_spend2
        expected_spend_days <- input$expected_spend_days2
        #run the allocator (reactive so that the loading effect takes place as soon as it is refreshed)
        AllocatorCollect2 <- reactive({Allocate(Model$InputCollect, 
                                                Model$OutputCollect, 
                                                Model$model_id, 
                                                scenario = scenario,
                                                channel_constr_low = unlist(channel_consrt_low_val), 
                                                channel_constr_up = unlist(channel_consrt_up_val),
                                                expected_spend = expected_spend,
                                                expected_spend_days = expected_spend_days
        )})
        AllocatorGraphs2 <- reactive({PlotAllocatorGraphs(Model, AllocatorCollect2())})
        output$AllocatorCurve2 <- renderPlotly(AllocatorGraphs2()$fig_curve)
        output$AllocatorTable2 <- renderDataTable(AllocatorGraphs2()$table_curve)
        output$ResponseComparisonscenario <- renderPlotly(responsecomparisonbar(Model,AllocatorCollect1,AllocatorCollect2))
        
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
