# Define server logic to summarize and view selected dataset ----
library(Robyn)
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  
  plotMediaShareLoop <- eventReactive(input$update, {
    plotMediaShare[solID == input$sid]
  }, ignoreNULL = FALSE)
  
  ##plot1
  output$plot1<- renderPlot({
    
    plotMediaShareLoop <- plotMediaShare[solID == input$sid]
    rsq_train_plot <- plotMediaShareLoop[, round(unique(rsq_train), 4)]
    nrmse_plot <- plotMediaShareLoop[, round(unique(nrmse), 4)]
    decomp_rssd_plot <- plotMediaShareLoop[, round(unique(decomp.rssd), 4)]
    mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input), plotMediaShareLoop[, round(unique(mape), 4)], NA)
    suppressWarnings(plotMediaShareLoop <- melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train"), measure.vars = c("spend_share", "effect_share", "roi_total", "cpa_total")))
    plotMediaShareLoop[, rn := factor(rn, levels = sort(InputCollect$paid_media_vars))]
    plotMediaShareLoopBar <- plotMediaShareLoop[variable %in% c("spend_share", "effect_share")]
    plotMediaShareLoopLine <- plotMediaShareLoop[variable == ifelse(InputCollect$dep_var_type == "conversion", "cpa_total", "roi_total")]
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
    p1
    
  })
  
  ##plot2
  output$plot2<-renderPlot({
    ## plot waterfall
    #TODO adapt robynpareto, is not supposed to be equal to 1
    plotMediaShareLoop <- plotMediaShare[solID == input$sid]
    rsq_train_plot <- plotMediaShareLoop[, round(unique(rsq_train), 4)]
    nrmse_plot <- plotMediaShareLoop[, round(unique(nrmse), 4)]
    decomp_rssd_plot <- plotMediaShareLoop[, round(unique(decomp.rssd), 4)]
    mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input), plotMediaShareLoop[, round(unique(mape), 4)], NA)
    suppressWarnings(plotMediaShareLoop <- melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train"), measure.vars = c("spend_share", "effect_share", "roi_total", "cpa_total")))
    plotWaterfall <- OutputCollect$xDecompAgg[robynPareto == 1]
    plotWaterfallLoop <- plotWaterfall[solID == input$sid][order(xDecompPerc)]
    plotWaterfallLoop[, end := cumsum(xDecompPerc)]
    plotWaterfallLoop[, end := 1 - end]
    plotWaterfallLoop[, ":="(start = shift(end, fill = 1, type = "lag"),
                             id = 1:nrow(plotWaterfallLoop),
                             rn = as.factor(rn),
                             sign = as.factor(ifelse(xDecompPerc >= 0, "pos", "neg")))]
    
    p2 <-ggplot(plotWaterfallLoop, aes(x = id, fill = sign)) +
        geom_rect(aes(x = rn, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start), stat = "identity") +
        scale_x_discrete("", breaks = levels(plotWaterfallLoop$rn), labels = plotWaterfallLoop$rn) +
        theme(axis.text.x = element_text(angle = 65, vjust = 0.6), legend.position = c(0.1, 0.1)) +
        geom_text(mapping = aes(
          label = paste0(xDecompAgg, "\n", round(xDecompPerc * 100, 2), "%"),
          y = rowSums(cbind(end, xDecompPerc / 2))
        ), fontface = "bold") +
        coord_flip() +
        labs(
          title = "Response decomposition waterfall by predictor",
          subtitle = paste0(
            "rsq_train: ", rsq_train_plot,
            ", nrmse = ", nrmse_plot,
            ", decomp.rssd = ", decomp_rssd_plot,
            ", mape.lift = ", mape_lift_plot
          ),
          x = "",
          y = ""
        )
    p2
    
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- plotMediaShareLoop()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(plotMediaShareLoop(), n = isolate(input$obs))
  })
  
}
