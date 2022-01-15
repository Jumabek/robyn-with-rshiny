library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(shinybusy)
library(reticulate)
library(log4r)

# set up environment ----
prod = FALSE
if(prod == FALSE){
  setwd("C:/Users/Bastien/robyn/shiny/app")
}

source("train.R")
source("allocator.R")
source_python("gcpSync.py")

bucket = 'robyn-test-data'
local_model_file = 'Model.RData'
remote_model_file = 'data/Model.RData'

if(prod == FALSE){
  storage_client = storage_client_with_creds('./credentials/robyn-test-337911-f105d6e3daf1.json')
} else {
  virtualenv_create("r-reticulate")
  virtualenv_exists("r-reticulate")
  use_virtualenv("r-reticulate", required = TRUE)
  py_install("nevergrad", pip = TRUE)
  py_install("google-cloud-storage", pip = TRUE)
  py_config()
  storage_client = storage_client_without_creds()
}


#setup the logfile ----
logger <- create.logger()
logfile(logger) <- 'serverData.log'
level(logger) <- 'INFO'

# Main login screen ----
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
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

credentials = data.frame(
  username_id = c("admin", "user"),
  passod   = sapply(c("55admin", "55user"),password_store),
  permission  = c("admin", "user"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "55 | Marketing Mix Modeling", titleWidth = 300, uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "black")

#server ----
server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
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
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="admin") {
        #admin pannel ----
        sidebarMenu(
          menuItem("Model Training", tabName = "training", icon = icon("flash")),
          menuItem("Model Selection", tabName = "selection", icon = icon("search")),
          menuItem("Budget Optimizer", tabName = "optimizer", icon = icon("dashboard")),
          menuItem("Documentation", tabName = "documentation", icon = icon("book")),
          menuItem("Logs", tabName = "logs", icon = icon("list"))
        )
      }
        
      else{
        #user pannel ----
        sidebarMenu(
          menuItem("Budget Optimizer", tabName = "optimizer", icon = icon("dashboard")),
          menuItem("Documentation", tabName = "documentation", icon = icon("book"))
        )
        
      }
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="admin") {
        #admin body ----
        tabItems(
          #model training ----
          tabItem(tabName ="training", class = "active",
            fluidRow(
              column(width = 9,
                box(width = NULL, plotOutput('modelPareto')),
                box(width = NULL, dataTableOutput('modelTable'))
              ),
              column(width = 3,
                box(width = NULL, status = "info",
                  title = "Controls", 
                  sliderInput("iterations", "Number of Iterations:", 100, 2000, 100),
                  sliderInput("trials", "Number of Trials:", 1, 10, 1),
                  actionButton("trainButton", "Train Model")
                  
              ))
            )
          ),
          #model selection ----
          tabItem(tabName ="selection", class = "active",
            fluidRow(
              column(width = 9
              ),
              column(width = 3,
                box(width = NULL, status = "info",
                  title = "Controls",
                  actionButton("importModelButton", "Import Existing Models"),
                  uiOutput('modelSolutions')
              ))
            )
          ),
          #budget optimizer ----
          tabItem(tabName ="optimizer", class = "active",
            fluidRow(
              column(width = 9,
                 plotOutput("p12"),
                 plotOutput("p13"),
                 plotOutput("p14"),
              ),
              column(width = 3,
              box(width = NULL, status = "info",
                title = "Controls",
                actionButton("importModelButtonUser", "Import Existing Models"),
                uiOutput('SelectedModel'),
                selectInput( "scenario", "Choose a scenario:",
                  c(
                    "Max historical response" = "max_historical_response",
                    "Max response expected spend" = "max_response_expected_spend"
                  )
                ),
                conditionalPanel(condition = "input.scenario == 'max_response_expected_spend'",
                  numericInput("expected_spend", "Expected Spend", 0),
                  numericInput("expected_spend_days", "Expected Spend Days", 0)
                ),
                uiOutput('channelConstr'),
                actionButton("optimizeButton", "Optimize Budget")
                         
              ))
            )
          ),
          #logs ----
          tabItem(tabName ="logs", class = "active",
            fluidRow(
              column(width = 9,
               box(width = NULL, status = "info", title = "Training Logs",tableOutput('logs'))
              ),
              column(width = 3,
                     box(width = NULL, status = "info",
                         title = "Controls",
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
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
#train model ----
  observeEvent(input$trainButton, {

    show_modal_spinner(
      "circle",
      text = "Please Wait, this might take a few minutes ...",
      color = "#2C2C2C"
    )
    Model <- train(input$iterations, input$trials)
    output$modelPareto <- renderPlot(Model$OutputCollect$UI$pParFront)
    output$modelTable <- renderDataTable(Model$OutputCollect$xDecompAgg$solID)
    #save the model to gcs
    info(logger, upload_blob(storage_client, bucket, local_model_file, remote_model_file))
    remove_modal_spinner()
    output$logs <- renderTable({read.delim('serverData.log',header=FALSE)})

  })
  
#import model ----
  observeEvent(input$importModelButton, {
    exists <- blob_exists(storage_client, bucket, remote_model_file)
    if (exists) {
      info(logger, 'Model file was retrieved successfully from GCS bucket')
      #download the model
      info(logger, download_blob(storage_client, bucket, remote_model_file, local_model_file))
      load(local_model_file)
      output$modelSolutions <- renderUI({selectInput("modelSolution","Select Model", Model$OutputCollect$allSolutions)})
    }else{
      info(logger, 'Model file was not found GCS bucket')
    }
    #output the log file
    output$logs <- renderTable({read.delim('serverData.log',header=FALSE)})
  })
  
#budget optimizer ----
  observeEvent(input$importModelButtonUser, {
    exists <- blob_exists(storage_client, bucket, remote_model_file)
    if (exists) {
      info(logger, 'Model file was retrieved successfully from GCS bucket')
      #download the model
      info(logger, download_blob(storage_client, bucket, remote_model_file, local_model_file))
      load(local_model_file)
      output$SelectedModel <- renderUI({selectInput("selectedModelUser","Select Model", Model$OutputCollect$allSolutions)})
      #scalable channel constraint selectors
      output$channelConstr <- renderUI({
        lapply(Model$InputCollect$paid_media_vars, 
               function(i){sliderInput(
                 inputId = paste0("constr_", i), i, 0.01, 2, value = c(0.7, 1.5), step = 0.1)
               }
        )
      })
    }else{
      info(logger, 'Model file was not found GCS bucket')
    }
    #output the log file
    output$logs <- renderTable({read.delim('serverData.log',header=FALSE)})
  })

  observeEvent(input$optimizeButton, {
    load(local_model_file)
    channel_consrt_low_val <- unlist(lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][1]}))
    channel_consrt_up_val <- unlist(lapply(Model$InputCollect$paid_media_vars, function(i){input[[paste0("constr_", i)]][2]}))
    AllocatorCollect <- allocate(Model$robyn_object, 
                                 Model$InputCollect, 
                                 Model$OutputCollect, 
                                 input$selectedModelUser, 
                                 input$scenario,
                                 channel_constr_low = channel_consrt_low_val,
                                 channel_constr_up = channel_consrt_up_val,
                                 expected_spend=input$expected_spend,
                                 expected_spend_days=input$expected_spend_days
                               )
    output$p12 <- renderPlot({AllocatorCollect$ui$p12})
    output$p13 <- renderPlot({AllocatorCollect$ui$p13})
    output$p14 <- renderPlot({AllocatorCollect$ui$p14})
  })
#reset log file ----
  observeEvent(input$resetLogs, {
    file.remove('serverData.log')
    logger <- create.logger()
    logfile(logger) <- 'serverData.log'
    level(logger) <- 'INFO'
    info(logger, 'File log resset')
    output$logs <- renderTable({read.delim('serverData.log',header=FALSE)})
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE, host='0.0.0.0', port=3838)


