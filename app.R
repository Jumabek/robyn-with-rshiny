library(shiny)
library(ggplot2)
library(data.table)
library(Robyn)


all_fronts <- unique(OutputCollect$xDecompAgg$robynPareto)
pf = all_fronts[1]
plotMediaShare <- OutputCollect$xDecompAgg[robynPareto == pf & rn %in% InputCollect$paid_media_vars]
uniqueSol <- plotMediaShare[, unique(solID)]

# See above for the definitions of ui and server
ui <- pageWithSidebar(
  headerPanel('Plot Title'),
  sidebarPanel(
    selectInput("sid", "Select sid", uniqueSol)
  ),
  mainPanel(
    tableOutput('table1')
  )
)
  

server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame

  #select the input ID

  ## plot spend x effect share comparison
  sid<-reactive({
    input$sid
  })
  
  plotMediaShareLoop <-reactive({
    plotMediaShare[solID == sid]
  }) 
  
  rsq_train_plot <- reactive({
    plotMediaShareLoop[, round(unique(rsq_train), 4)]
  })
  
  nrmse_plot <- reactive({
    plotMediaShareLoop[, round(unique(nrmse), 4)]
  })
  
  decomp_rssd_plot <- reactive({
    plotMediaShareLoop[, round(unique(decomp.rssd), 4)]
  })
  mape_lift_plot <- reactive({
    ifelse(!is.null(InputCollect$calibration_input), plotMediaShareLoop[, round(unique(mape), 4)], NA)
  })
  
  suppressWarnings(plotMediaShareLoop <- reactive({
    melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train"), measure.vars = c("spend_share", "effect_share", "roi_total", "cpa_total"))
    }))
  
  plotMediaShareLoopBar <- reactive({
    plotMediaShareLoop[, rn := factor(rn, levels = sort(InputCollect$paid_media_vars))]
    plotMediaShareLoop[variable %in% c("spend_share", "effect_share")]
  })
  
  line_rm_inf <- reactive({
    !is.infinite(plotMediaShareLoopLine$value)
  })
  ySecScale <- reactive({
    max(plotMediaShareLoopLine$value[line_rm_inf]) / max(plotMediaShareLoopBar$value) * 1.1
  })

  
  output$table1 <- renderTable({plotMediaShareLoop})
  

  
}

shinyApp(ui = ui, server = server)
