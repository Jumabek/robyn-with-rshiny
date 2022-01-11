library(Robyn)
library(reticulate)
virtualenv_create("r-reticulate")
virtualenv_exists("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
py_install("nevergrad", pip = TRUE)
py_config()

#Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/r-reticulate/bin/python3.8")
#ng <-import("nevergrad", delay_load = FALSE)

#set.seed(123)

robyn_object <- "/out/robyn.RDS"
getwd()

#---------------------------
#    main app
#---------------------------
#if working locally
#setwd("C:/Users/Bastien/robyn/shiny/app")


library(shiny)
library(shinythemes)
library(shinybusy)

source('ui.R')
source('server.R')



app <- shinyApp(ui=ui,server=server)
runApp(host='0.0.0.0', port=3838)
