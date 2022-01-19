library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(plotly)


#1 plot the historical outputs ----
PlotHistorical <- function(Model, startDate, endDate){

#1.1 variables computation ----  
  #initialize the list of plots
  Hplot <- list()
  weekly_channel_rev <- 
    Model$OutputCollect$xDecompVecCollect %>%
    filter(solID == Model$model_id) %>%
    select(c("ds", Model$InputCollect$paid_media_vars))%>%
    filter(ds >= startDate & ds <= endDate) %>%
    pivot_longer(Model$InputCollect$paid_media_vars, names_to = "paid_media_vars", values_to = "revenue")
  
  total_channel_rev <- 
    Model$OutputCollect$xDecompVecCollect %>%
    filter(solID == Model$model_id) %>%
    filter(ds >= startDate & ds <= endDate) %>%
    select(c(Model$InputCollect$paid_media_vars))%>%
    summarise(across(everything(), ~ sum(., is.na(.), 0))) %>%
    pivot_longer(Model$InputCollect$paid_media_vars, names_to = "paid_media_vars", values_to = "revenue")
  
  total_channel_spend <-
    Model$InputCollect$dt_input %>%
    filter(DATE >= startDate & DATE <= endDate)%>%
    select(c(Model$InputCollect$paid_media_vars))%>%
    summarise(across(everything(), ~ sum(., is.na(.), 0))) %>%
    pivot_longer(Model$InputCollect$paid_media_vars, names_to = "paid_media_vars", values_to = "spend")
  
  total_channel_roi <-
    bind_cols(c(total_channel_rev,total_channel_spend['spend'])) %>%
    mutate(roi = revenue/spend)
  
  
  
#1.2 graph refinement ----
  # Stacked historical return 
  #TODO fix this plot
  Hplot$return_per_channel <- 
    ggplot(weekly_channel_rev, aes(fill=paid_media_vars, y=return, x=ds)) + 
    geom_bar(position="stack", stat="identity")
  
  # total historical return per channel
  Hplot$channel_roi <-
  ggplotly(
    ggplot(total_channel_roi, aes(x=paid_media_vars, y=roi, fill=paid_media_vars)) + 
      geom_bar(stat='identity') +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, option = 'plasma') +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(colour="white"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        rect = element_rect(fill = "transparent")
      )
  ) %>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = F)
  
  
  
  #return the plots
  return(Hplot)
  
}


#2 plot the allocator outputs ----
PlotAllocator <- function(AllocatorCollect){
  Aplot <- list()
  
  #initial vs optimised mean response
  Aplot$p1 <- ggplotly(
    AllocatorCollect$ui$p12 +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, option = 'plasma') +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(colour="white"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        rect = element_rect(fill = "transparent")
      )
    )%>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = F)
  
  Aplot$p2 <- ggplotly(
    AllocatorCollect$ui$p13 +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, option = 'plasma') +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(colour="white"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        rect = element_rect(fill = "transparent")
      )
    )%>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = F)
  
  Aplot$p3 <- ggplotly(
    AllocatorCollect$ui$p14 +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, option = 'plasma') +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(colour="white"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        rect = element_rect(fill = "transparent")
      )
    )%>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = F)
  
  return(Aplot)
  
}