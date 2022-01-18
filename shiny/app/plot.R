library(dplyr)
library(tidyr)
library(ggplot2)

PlotGraph <- function(Model, startDate, endDate){
  
  #initialize the list of plots
  plot <- list()
  
  weekly_channel_ROI <- 
    Model$OutputCollect$xDecompVecCollect %>%
    filter(solID == Model$model_id) %>%
    select(c("ds", Model$InputCollect$paid_media_vars))%>%
    filter(ds >= startDate & ds <= endDate) %>%
    pivot_longer(Model$InputCollect$paid_media_vars, names_to = "paid_media_vars", values_to = "return")
  
  # Stacked
  plot$return_per_channel <- 
    ggplot(weekly_channel_ROI, aes(fill=paid_media_vars, y=return, x=ds)) + 
    geom_bar(position="stack", stat="identity") 
  
  #TODO add the scatter plot for the total Revenue contribution
  #return the plots
  return(plot)
  
}