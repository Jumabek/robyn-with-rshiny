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

## Check simulated dataset or load your own dataset
data("dt_simulated_weekly")

## Check holidays from Prophet
# 59 countries included. If your country is not included, please manually add it.
# Tipp: any events can be added into this table, school break, events etc.
data("dt_prophet_holidays")

robyn_object <- "/out/robyn.RDS"


library(shiny)
library(glue)


getwd()


source(glue('{getwd()}/shinyapp/ModelUI.R'))
source(glue('{getwd()}/shinyapp/ModelServer.R'))


shinyApp(
  ui = uiR,
  server = serverR
)
