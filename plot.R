library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(plotly)
library(grid)
library(scales)
library(Robyn)

#1 plot the historical ROI ----
PlotHistoricalRev <- function(Model, start_date, end_date){
  print("Generating historical contribution plot")
  #initialize the list of plots
  channel_rev <- 
    Model$OutputCollect$xDecompVecCollect %>%
    filter(solID == Model$model_id) %>%
    filter(ds >= start_date & ds <= end_date) %>%
    select(-c(depVarHat,intercept,solID)) %>%
    pivot_longer(Model$InputCollect$all_ind_vars, names_to = "channel", values_to = "revenue") %>%
    mutate(revenue = round(revenue,0)/1000)
  

  
  
  # total historical contribution per channel
  fig <-ggplotly(
    ggplot(channel_rev, aes(x=ds, y=revenue, fill=channel)) + 
      geom_area() +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, option = 'viridis') +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank()
      )
  ) %>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = T,
           legend =list(orientation = 'h',bgcolor = 'rgba(0,0,0,0)', title=""),
           yaxis = list(title = 'Revenue (K$)', titlefont = list(size = 12))
    )
  

  
  #return the plots
  return(fig)
  
}

PlotHistoricalRevpaidmedia <- function(Model, start_date, end_date){
  print("Generating historical contribution plot")
  #initialize the list of plots
  channel_rev <- 
    Model$OutputCollect$xDecompVecCollect %>%
    filter(solID == Model$model_id) %>%
    filter(ds >= start_date & ds <= end_date) %>%
    select(c("ds",Model$InputCollect$paid_media_vars)) %>%
    pivot_longer(Model$InputCollect$paid_media_vars, names_to = "channel", values_to = "revenue") %>%
    mutate(revenue = round(revenue,0)/1000)
  
  
  
  # total historical contribution per channel
  fig <-ggplotly(
    ggplot(channel_rev, aes(x=ds, y=revenue, fill=channel)) + 
      geom_area() +
      theme_minimal() +
      scale_fill_viridis(discrete = TRUE, option = 'viridis') +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank()
      )
  ) %>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = T,
           legend =list(orientation = 'h',bgcolor = 'rgba(0,0,0,0)', title=""),
           yaxis = list(title = 'Revenue (K$)', titlefont = list(size = 12))
    )
  
  
  
  #return the plots
  return(fig)
  
}
#2 plot the allocator outputs ----
PlotAllocatorGraphs <- function(Model, AllocatorCollect){
  print("Generating allocator plots")
  Plot <- list()
  
  
  #response comparison bar plot ----
  plotDT_resp <- AllocatorCollect$dt_optimOut %>%
    select(channels, initResponseUnit, optmResponseUnit) %>%
    rename("Initial response" = initResponseUnit, "Optimised response" = optmResponseUnit)%>%
    pivot_longer(c("Initial response","Optimised response"), names_to = "response_type", values_to = "response" )%>%
    mutate(response = response/1000)
  

  Plot$fig_response_comparison <-ggplotly(
          ggplot(plotDT_resp, aes(x = channels, y = response, fill = response_type)) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_brewer(palette = "Paired") +
          geom_text(aes(
            label = paste0(round(response, 2), ' k$')
            ),
            fontface = "bold", show.legend = FALSE,
            size = 3, position = position_dodge(0.9), vjust = -0.3
          ) +
          theme(
            legend.title = element_blank(),
            axis.title.x=element_blank(),
            axis.ticks.x = element_blank()
          ) +
          labs(y = "Response (K$)", x = "Channel")
      )%>%
        layout(plot_bgcolor = 'rgba(0,0,0,0)', 
               paper_bgcolor = 'rgba(0,0,0,0)',
               showlegend = T,
               legend =list(orientation = 'h',bgcolor = 'rgba(0,0,0,0)',title="", y = -0.4),
               xaxis = list(tickangle = -65)
               )
  
  #create the media spend curve ----
  plotDT_saturation <- Model$OutputCollect$mediaVecCollect %>%
    filter(type == "saturatedSpendReversed" & solID == Model$model_id) %>%
    select(c("ds",Model$InputCollect$paid_media_vars)) %>%
    pivot_longer(Model$InputCollect$paid_media_vars, names_to = "channel", values_to = "spend" )%>%
    arrange(channel, ds)
  
  plotDT_decomp <- Model$OutputCollect$mediaVecCollect %>%
    filter(type == "decompMedia" & solID == Model$model_id) %>%
    select(c("ds",Model$InputCollect$paid_media_vars)) %>%
    pivot_longer(Model$InputCollect$paid_media_vars, names_to = "channel", values_to = "response" )%>%
    arrange(channel, ds)
  
  plotDT_scurve <- bind_cols(plotDT_saturation,plotDT_decomp["response"])%>%
    filter(spend >= 0)%>%# remove outlier introduced by MM nls fitting
    mutate(spend = spend/1000, response = response/1000)
  
  plotDT_scurveMeanResponse <- Model$OutputCollect$xDecompAgg %>%
    filter(rn %in% Model$InputCollect$paid_media_vars & solID == Model$model_id)
  
  dt_optimOutScurve <- bind_rows(
    AllocatorCollect$dt_optimOut %>%
    select(c("channels", "initSpendUnit", "initResponseUnit")) %>%
    mutate(type = "initial")%>%
    rename(spend = initSpendUnit, response = initResponseUnit),

    AllocatorCollect$dt_optimOut %>%
    select(c("channels", "optmSpendUnit", "optmResponseUnit")) %>%
    mutate(type = "optimised")%>%
    rename(spend = optmSpendUnit, response = optmResponseUnit)
  )%>%
    mutate(spend = spend/1000, response = response/1000)
  
  Plot$fig_curve <-ggplotly(
    ggplot(data = plotDT_scurve, aes(x = spend, y = response, color = channel)) +
    geom_line(size=0.7) +
    geom_point(data = dt_optimOutScurve, aes(
      x = spend, y = response, color = channels, shape = type
    ), size = 2) +
    scale_shape_manual(values=c(20, 3)) +
    geom_text(
      data = dt_optimOutScurve, aes(
        x = spend + 40, y = response - 15, color = channels, label = round(spend, 1), size=10
      ),
      show.legend = FALSE, hjust = -0.2
    ) +
    scale_colour_viridis_d( option = 'viridis') +
    theme(
      legend.title = element_blank(),
      plot.title = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.title=element_text(size=10),
      axis.text = element_text(size=10)
      )+
    labs(x = "Spend (K$)", y = "Response (K$)")
  )%>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = T,
           legend =list(orientation = 'h',bgcolor = 'rgba(0,0,0,0)',title="", y = -0.2)
    )%>%
    style(showlegend = FALSE, traces = 6:20)
  

  
  Plot$table_curve <- dt_optimOutScurve %>%
    select("channels", "spend", "type")%>%
    mutate(spend = round(spend, 4))%>%
    pivot_wider(names_from = type, values_from = spend)
  
  
  
  return(Plot)
}

#3 plot weekly revenues and spend per channel ----
PlotWeeklyRevSpend  <- function(Model, start_date, end_date) {
  print("Generating weekly revenue and spend")
  
  #media revenues
  dt_input <- Model$InputCollect$dt_input %>%
    filter(DATE >= start_date & DATE <= end_date) %>%
    mutate(revenue = round(revenue,0)/1000)
  
  #media spend
  media_spend<-dt_input %>%
    select(DATE, Model$InputCollect$paid_media_spends) %>%
    filter(DATE >= start_date & DATE <= end_date) %>%
    pivot_longer(Model$InputCollect$paid_media_spends, names_to = "channel", values_to = "spend")%>%
    mutate(spend = round(spend,0)/1000)
  
  fig1 <- ggplotly(
            ggplot(data = dt_input, aes(x = DATE, y = revenue, group = 1)) +
              geom_line(color = "#0c4b74") + 
               scale_y_continuous(labels = comma) +
               theme(
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x=element_blank()
               )
        ) %>%
    layout(yaxis = list(title = 'Revenue (K$)', titlefont = list(size = 12)))
  
  fig2<-ggplotly(
    ggplot(media_spend, aes(fill=channel, y=spend, x=DATE),lwd=20)+ 
      geom_area()+
      scale_fill_viridis(discrete = TRUE, option = 'viridis') +
      scale_y_continuous(labels = comma) +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank()
      )
    )%>%
    layout( yaxis = list(title = 'Spend (K$)', titlefont = list(size = 12)))

  
  fig <- subplot(fig1, fig2, nrows = 2, titleY = TRUE)%>%
          layout(plot_bgcolor = 'rgba(0,0,0,0)', 
                 paper_bgcolor = 'rgba(0,0,0,0)',
                 showlegend = T,
                 legend =list(orientation = 'h',bgcolor = 'rgba(0,0,0,0)',title="")
           )
  
  return(fig)
  
}


PlotWeeklyRevSpend(Model,start_date,end_date)#4 plot the waterfall contribution ----
PlotWaterfallContrib <- function(Model){
  plotWaterfallLoop <- Model$OutputCollect$xDecompAgg %>%
    filter(solID == Model$model_id)%>%
    filter(rn %in% Model$InputCollect$paid_media_vars)%>%
    select(rn, xDecompPerc)%>%
    arrange(desc(xDecompPerc))%>%
    mutate(end = cumsum(xDecompPerc))%>%
    mutate(start = lag(end, default = 0), id=1:n(), sign=ifelse(xDecompPerc >= 0, "pos", "neg"))
  
  
  
  fig <- ggplotly(
          ggplot(plotWaterfallLoop, aes(x = id, fill = sign)) +
          geom_rect(aes(x = rn, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start), stat = "identity") +
          scale_x_discrete('', labels = plotWaterfallLoop$rn) +
          theme(
            axis.text.x = element_text(angle = 65, vjust = 0.6),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          ) +
          geom_text(mapping = aes(
            label = paste0(round(xDecompPerc * 100, 1), "%"),
            colour = factor(sign),
            y = end + 0.05
          ),
          size = 3
          )
        )%>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)',
           showlegend = F,
           legend =list(bgcolor = 'rgba(0,0,0,0)', title=""),
           yaxis = list(title = 'ROI', titlefont = list(size = 12))
    )%>%
    style(hoverinfo = 'none')
  
  return(fig)

} 
#5 pie chart media contribution ----
pie_chart_media_contribution<- function(Model,start_date,end_date) {
  print("Generating contribution pie chart")
  
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

    scale_fill_viridis(discrete = TRUE, option = 'viridis') +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      rect = element_rect(fill = "transparent")
    )
} 


#5 ROI sur p?riode aggr?g?e ----
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
      scale_fill_viridis(discrete = TRUE, option = 'viridis') +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        rect = element_rect(fill = "transparent")
      )
  ) 
}
  responsecomparisonbar <- function(Model, AllocatorCollect1,AllocatorCollect2){
    print("Generating allocator plots")
    Plot <- list()
    
    
    #response comparison bar plot ----
    
    
    plotDT_resp1 <- AllocatorCollect1$dt_optimOut %>%
      select(channels, initResponseUnit, optmResponseUnit) %>%
      rename("Initial response" = initResponseUnit, "Optimised response scenario 1" = optmResponseUnit)%>%
      pivot_longer(c("Initial response","Optimised response scenario 1"), names_to = "response_type", values_to = "response" )%>%
      mutate(response = response/1000)
    
    
    # <- colnames(plotDT_resp1)[colnames(plotDT_resp1) == 'channels'] <- 'channels_scenario1'
    
    
    plotDT_resp2 <- AllocatorCollect1$dt_optimOut %>%
      select(channels, initResponseUnit, optmResponseUnit) %>%
      rename("Initial response" = initResponseUnit, "Optimised response scenario 2" = optmResponseUnit)%>%
      pivot_longer(c("Initial response","Optimised response scenario 2"), names_to = "response_type", values_to = "response" )%>%
      mutate(response = response/1000)%>%
      filter(response_type == "Optimised response scenario 2")
    
    plotDT_resp_scenario <- rbind(plotDT_resp1, plotDT_resp2) 
    plotDT_resp_scenario %>%  arrange(channels)
    
    fig_response_comparison <-ggplotly(
      ggplot(plotDT_resp_scenario, aes(x = channels, y = response, fill = response_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_brewer(palette = "Paired") +
        geom_text(aes(
          label = paste0(round(response, 2), ' k$')
        ),
        fontface = "bold", show.legend = FALSE,
        size = 3, position = position_dodge(0.9), vjust = -0.3
        ) +
        theme(
          legend.title = element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x = element_blank()
        ) +
        labs(y = "Response (K$)", x = "Channel")
    )%>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)', 
             paper_bgcolor = 'rgba(0,0,0,0)',
             showlegend = T,
             legend =list(orientation = 'h',bgcolor = 'rgba(0,0,0,0)',title="", y = -0.4),
             xaxis = list(tickangle = -65)
      )
    
    
    
    
    
    return(fig_response_comparison)
  }
  

