library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(plotly)
library(reshape2)


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
        plot.title = element_blank(),
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
        plot.title = element_blank(),
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
        plot.title = element_blank(),
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

#3. historique des revenus ----
line_chart_rev_week  <- function(Model, start_date, end_date) {
  dt_input <- Model$InputCollect$dt_input %>%
    filter(DATE >= start_date & DATE <= end_date) %>%
    mutate(revenue = round(revenue,0))
  
  p1 <- ggplotly(
    ggplotly(ggplot(data = dt_input, aes(x = DATE, y = revenue, group = 1)) +  
               geom_point(color='dodgerblue4')+ geom_line(color="dodgerblue4") +  
               #theme_minimal() +
               scale_fill_viridis(discrete = TRUE, option = 'plasma') +
               theme(
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text.x=element_text(colour="white"),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 #TODO passer les Y en blanc + changer le format numérique
                 axis.text.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 rect = element_rect(fill = "transparent")
               )
    )%>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', 
             paper_bgcolor = 'rgba(0,0,0,0)',
             showlegend = F) 
  )
  
  
  return(p1)
  
}

#3.1 historique agg à l'année ----
line_chart_rev_year  <- function(Model) {
  dt_input <- Model$InputCollect$dt_input
  dt_input$year <- format(dt_input$DATE,"%Y")
  year_rev <- dt_input %>%
    group_by(year) %>%
    summarise(revenue = sum(revenue, na.rm = TRUE) ) 
  year_rev$revenue <-round(year_rev$revenue,0)
  
  
  
  
  monthly_view <- ggplot(data = year_rev, aes(x = year, y = revenue, group = 1)) 
  
  p <-monthly_view %>%
    + geom_line(color='dodgerblue4')  + geom_point(color='dodgerblue4') +theme(axis.text.x=element_text(angle = -90, hjust = 0))
  
  ggplotly(p+
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
             ))
  
  
  
} 

#4 historique des dépenses ----
#TODO passer le graphique en aires au lieu de barplot
bar_chart_media_spend_week<- function(Model, start_date, end_date) {
  dt_input <- Model$InputCollect$dt_input
  
  media_spend<-dt_input %>%
    select(DATE, Model$InputCollect$paid_media_spends) %>%
    filter(DATE >= start_date & DATE <= end_date) 
  
  media_spend <- melt(media_spend, id = c("DATE"))
  
  
  colnames(media_spend)[colnames(media_spend) %in% c("variable", "value")]  <- c("media_channel", "revenue")
  media_spend$revenue <-round(media_spend$revenue,0)
  
  p<-ggplotly(
    ggplot(media_spend, aes(fill=media_channel, y=revenue, x=DATE),lwd=20)+ 
    geom_bar(position = "stack", stat="identity")+
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

  
  
  
  
} 

#4 pie chart media contribution ----
pie_chart_media_contribution<- function(Model,start_date,end_date) {
  start_date<- "2018-01-01"
  end_date<-"2018-01-31"
  df <- Model[["OutputCollect"]][["xDecompVecCollect"]]
  df <- Model[["OutputCollect"]][["xDecompVecCollect"]]
  df<- df %>% filter(solID == Model$model_id,ds >= start_date & ds <= end_date)
  
  df$trend <- df$trend + df$season + df$holiday
  media_effect<-df %>%
    select(-solID,-dep_var,-depVarHat,-intercept,-season,-holiday)
  
  media_effect <- melt(media_effect,id = c("ds"))
  media_effect <- media_effect  %>%
    group_by(variable) %>%
    summarise(revenue = sum(value, na.rm = TRUE))
  
  media_effect$revenue <-round(media_effect$revenue,0)
  
  sum_rev <- sum(media_effect$revenue)    
  media_effect$perc <- media_effect$revenue/sum_rev
  media_effect$labels <-  scales::percent(media_effect$perc,accuracy = 1)
  
  
  ggplot(media_effect, aes(x="", y=perc, fill=variable)) +
    geom_col() +
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    coord_polar("y", start=0) +
  # printing the percentage

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
} 


#5 ROI sur période aggrégée ----
bar_chart_media_ROI <- function(Model,start_date,end_date,ID) {
  df <- Model[["OutputCollect"]][["xDecompVecCollect"]]
  df<- df %>% filter(solID == ID,ds >= start_date & ds <= end_date)
  
  
  df<-df %>%
    
    select(ds, Model$InputCollect$paid_media_vars)
  
  df
  
  media_effect <- melt(df,id = c("ds"))
  
  media_effect <- media_effect  %>%
    group_by(variable) %>%
    summarise(revenue = sum(value, na.rm = TRUE))
  
  
  dt_input <- Model$InputCollect$dt_input
  media_spend<-dt_input %>%
    select(DATE, Model$InputCollect$paid_media_vars)
  media_spend <- media_spend %>% filter(DATE >= start_date & DATE <= end_date)
  media_spend <- melt(media_spend,id = c("DATE"))
  media_spend <- media_spend  %>%
    group_by(variable) %>%
    summarise(spend = sum(value, na.rm = TRUE))
  
  ROI_media <- merge(media_spend,media_effect, by ="variable")
  ROI_media$ROI <- ROI_media$revenue/ROI_media$spend -1
  ROI_media
  
  ggplotly(
    ggplot(ROI_media, aes(fill=variable, y=ROI, x=variable),lwd=20)+ 
      geom_bar( stat="identity")+
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
  ) 
  
  
}
