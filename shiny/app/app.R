Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/r-reticulate/bin/python3.8")

library(reticulate)
virtualenv_exists("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
py_install("nevergrad", pip = TRUE)

Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/r-reticulate/bin/python3.8")

ng <-import("nevergrad", delay_load = FALSE)

library(Robyn) 

#set.seed(123)

## force multicore when using RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE="true")
options(future.fork.enable = TRUE)

robyn_object <- "/out/robyn.RDS"



library(shiny)
setwd("C:/Users/Bastien/robyn/shiny/app")
source('uiModel.R')
source('serverModel.R')


shinyApp(
  ui = ui,
  server = server
)
