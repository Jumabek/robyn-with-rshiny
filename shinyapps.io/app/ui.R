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
prod = TRUE
if(prod == FALSE){
  setwd("C:/Users/jumabek/robyn/shiny/app")
}

##0.1 setup the logfile
logger <- create.logger()
logfile(logger) <- 'serverData.log'
level(logger) <- 'INFO'

#0.2 loading local libraries
source("train.R")
source("allocator.R")
source("plot.R")


#0.3 remote storage
bucket = 'robyn-test-bucket'


local_model_file = 'JumaModel3.RData'
print("Local file")
print(local_model_file)
print(getwd())
print(list.files())
print(list.dirs())

info(logger, glue('Prod set to {prod}'))

if(prod == FALSE){
  #storage_client = storage_client_with_creds('./credentials/robyn-test-2-672df5a3c62a.json')
  info(logger, glue('Using json file as credentials'))
} else {
  #TODO install the libraries and environment as part as the dockerfile
  virtualenv_create("r-reticulate")
  virtualenv_exists("r-reticulate")
  use_virtualenv("r-reticulate", required = TRUE)
  
  #use_condaenv("r-reticulate")
  py_install("nevergrad", pip = TRUE)
  py_install("google-cloud-storage", pip = TRUE)
  py_config()
  
  #source_python("gcpSync.py")# uses google library
  #storage_client = storage_client_with_creds('./credentials/robyn-test-2-672df5a3c62a.json') #storage_client_without_creds()
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
header <- dashboardHeader( title = "FixedPoint | Marketing Mix Modeling", titleWidth = 300, uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyDashboardThemes(theme = "grey_dark"), shinyjs::useShinyjs(), uiOutput("body"))
loader <- (options(spinner.color="#0dc5c1", spinner.type  = 7))

dashboardPage(header, sidebar, body, loader)
