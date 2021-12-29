library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(Robyn)
source('robyn/shinyapp/RobynUI.R')
source('robyn/shinyapp/RobynServer.R')


shinyApp(
  ui = ui,
  server = server
)
