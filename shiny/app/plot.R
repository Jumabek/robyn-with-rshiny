library(ggplot2)
library(data.table)
library(Robyn)
library(patchwork)
library(grid)
library(scales)
library(dplyr)

plot <- function(Model){
  #initialize the list of plots
  Plot <- list()
  
  #initialize the required variables
  xDecompAgg <- Model$OutputCollect$xDecompAgg 
  InputCollect <- Model$InputCollect
  model_id <- Model$model_id
  
  
  #TODO check the robynPareto variable, why is it fixed to 1
  plotMediaShare <- xDecompAgg[robynPareto == 1 & rn %in% InputCollect$paid_media_vars]
  plotWaterfall <- xDecompAgg[robynPareto == 1]
  uniqueSol <- plotMediaShare[, unique(solID)]
  
  parallelResult <- ## plot spend x effect share comparison
    plotMediaShareLoop <- plotMediaShare[solID == model_id]
    #TODO save data
    rsq_train_plot <- plotMediaShareLoop[, round(unique(rsq_train), 4)]
    nrmse_plot <- plotMediaShareLoop[, round(unique(nrmse), 4)]
    decomp_rssd_plot <- plotMediaShareLoop[, round(unique(decomp.rssd), 4)]
    mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input), plotMediaShareLoop[, round(unique(mape), 4)], NA)
    
    suppressWarnings(plotMediaShareLoop <- data.table::melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train"), measure.vars = c("spend_share", "effect_share", "roi_total", "cpa_total")))
    plotMediaShareLoop[, rn := factor(rn, levels = sort(InputCollect$paid_media_vars))]
    plotMediaShareLoopBar <- plotMediaShareLoop[variable %in% c("spend_share", "effect_share")]
    # plotMediaShareLoopBar[, variable:= ifelse(variable=="spend_share", "total spend share", "total effect share")]
    plotMediaShareLoopLine <- plotMediaShareLoop[variable == ifelse(InputCollect$dep_var_type == "conversion", "cpa_total", "roi_total")]
    # plotMediaShareLoopLine[, variable:= "roi_total"]
    line_rm_inf <- !is.infinite(plotMediaShareLoopLine$value)
    ySecScale <- max(plotMediaShareLoopLine$value[line_rm_inf]) / max(plotMediaShareLoopBar$value) * 1.1
  
  
  
  p1 <- ggplot(plotMediaShareLoopBar, aes(x = rn, y = value, fill = variable)) +
    geom_bar(stat = "identity", width = 0.5, position = "dodge") +
    geom_text(aes(label = paste0(round(value * 100, 2), "%")), color = "darkblue", position = position_dodge(width = 0.5), fontface = "bold") +
    geom_line(data = plotMediaShareLoopLine, aes(x = rn, y = value / ySecScale, group = 1, color = variable), inherit.aes = FALSE) +
    geom_point(data = plotMediaShareLoopLine, aes(x = rn, y = value / ySecScale, group = 1, color = variable), inherit.aes = FALSE, size = 4) +
    geom_text(
      data = plotMediaShareLoopLine, aes(label = round(value, 2), x = rn, y = value / ySecScale, group = 1, color = variable),
      fontface = "bold", inherit.aes = FALSE, hjust = -1, size = 6
    ) +
    scale_y_continuous(sec.axis = sec_axis(~ . * ySecScale)) +
    coord_flip() +
    theme(legend.title = element_blank(), legend.position = c(0.9, 0.2), axis.text.x = element_blank()) +
    scale_fill_brewer(palette = "Paired") +
    labs(
      title = paste0("Share of Spend VS Share of Effect with total ", ifelse(InputCollect$dep_var_type == "conversion", "CPA", "ROI")),
      subtitle = paste0(
        "rsq_train: ", rsq_train_plot,
        ", nrmse = ", nrmse_plot,
        ", decomp.rssd = ", decomp_rssd_plot,
        ", mape.lift = ", mape_lift_plot
      ),
      y = "", x = ""
    )
  
  Plot$p1 <- p1
  
  #2 piechart ----
  
  # Create Data
  data <- data.frame(
    group=plotMediaShareLoopLine$rn,
    value=round(plotMediaShareLoopLine$value,2)
  )
  
  
  # Compute percentages
  data$fraction <- data$value / sum(data$value)
  
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax <- cumsum(data$fraction)
  
  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n=-1))
  
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute a good label
  data$label <- paste0(data$group, "\n", data$value)
  
  # Make the plot
  pie_plot <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
    geom_rect() +
    geom_text( x=3.5, aes(y=labelPosition, label=label), size=6) +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent') #transparent legend panel
  
    )+
    labs(
      title = paste0("Channel ", ifelse(InputCollect$dep_var_type == "conversion", "CPA", "ROI"))
    )
  Plot$pie_plot <- pie_plot
  
  return(Plot)
  
  #plot revenue per month
  
}