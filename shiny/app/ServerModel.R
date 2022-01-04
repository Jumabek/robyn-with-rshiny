
library(Robyn)
library(data.table)
library(ggplot2)
library(shinybusy)

#-------------------
#     server
#-------------------
server <- function(input, output) {
  #-------------------
  #     run model
  #-------------------
  
  toReturn <- reactiveValues(
    InputCollect = NULL,
    OutputCollect = NULL,
    hyperparameters = NULL
  )
  
  output$trainingStatus <- renderText({"Model not trained"})
  
  observeEvent(input$runButton, {
    show_modal_spinner(
      "circle", 
      text = "Please Wait, this might take a few minutes ...", 
      color = "#ffffff"
    ) # show the modal window
    ## force multicore when using RStudio
    Sys.setenv(R_FUTURE_FORK_ENABLE="true")
    options(future.fork.enable = TRUE)
    
    output$trainingStatus <- renderText({"Training model"})
    
    ## Must install the python library Nevergrad once
    ## ATTENTION: The latest Python 3.10 version will cause Nevergrad installation error
    ## See here for more info about installing Python packages via reticulate
    ## https://rstudio.github.io/reticulate/articles/python_packages.html
    
    #create the output directory
    output_dir <- paste(getwd(), "output", sep="/")
    #if the output directory doesn't exist, create it
    if (file.exists(output_dir)){
      glue("{output_dir} already exists")
    } else {
      dir.create(file.path(output_dir), showWarnings = TRUE)
    }
    
    ################################################################
    #### Step 1: load data
    
    ## Check simulated dataset or load your own dataset
    data("dt_simulated_weekly")
    head(dt_simulated_weekly)
    
    ## Check holidays from Prophet
    # 59 countries included. If your country is not included, please manually add it.
    # Tipp: any events can be added into this table, school break, events etc.
    data("dt_prophet_holidays")
    head(dt_prophet_holidays)
    
    ## Set robyn_object. It must have extension .RDS. The object name can be different than Robyn:
    robyn_object <- paste(output_dir,"MyRobyn.RDS", sep="/")
    
    ################################################################
    #### Step 2a: For first time user: Model specification in 4 steps
    
    #### 2a-1: First, specify input data & model parameters
    
    # Run ?robyn_inputs to check parameter definition
    InputCollect <- robyn_inputs(
      dt_input = dt_simulated_weekly
      ,dt_holidays = dt_prophet_holidays
      
      ### set variables
      
      ,date_var = "DATE" # date format must be "2020-01-01"
      ,dep_var = "revenue" # there should be only one dependent variable
      ,dep_var_type = "revenue" # "revenue" or "conversion"
      
      ,prophet_vars = c("trend", "season", "holiday") # "trend","season", "weekday", "holiday"
      # are provided and case-sensitive. Recommended to at least keep Trend & Holidays
      ,prophet_signs = c("default","default", "default") # c("default", "positive", and "negative").
      # Recommend as default.Must be same length as prophet_vars
      ,prophet_country = "DE"# only one country allowed once. Including national holidays
      # for 59 countries, whose list can be found on our github guide
      
      ,context_vars = c("competitor_sales_B", "events") # typically competitors, price &
      # promotion, temperature, unemployment rate etc
      ,context_signs = c("default", "default") # c("default", " positive", and "negative"),
      # control the signs of coefficients for baseline variables
      
      ,paid_media_vars = c("tv_S", "ooh_S"	,	"print_S"	,"facebook_I" ,"search_clicks_P")
      # c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I", "facebook_S","search_clicks_P"	,"search_S")
      # we recommend to use media exposure metrics like impressions, GRP etc for the model.
      # If not applicable, use spend instead
      ,paid_media_signs = c("positive", "positive","positive", "positive", "positive")
      # c("default", "positive", and "negative"). must have same length as paid_media_vars.
      # Controls the signs of coefficients for media variables
      ,paid_media_spends = c("tv_S","ooh_S",	"print_S"	,"facebook_S", "search_S")
      # spends must have same order and same length as paid_media_vars
      
      ,organic_vars = c("newsletter")
      ,organic_signs = c("positive") # must have same length as organic_vars
      
      ,factor_vars = c("events") # specify which variables in context_vars and
      # organic_vars are factorial
      
      ### set model parameters
      
      ## set cores for parallel computing
      ,cores = 6 # I am using 6 cores from 8 on my local machine. Use future::availableCores() to find out cores
      
      ## set rolling window start
      ,window_start = "2016-11-23"
      ,window_end = "2018-08-22"
      
      ## set model core features
      ,adstock = "geometric" # geometric, weibull_cdf or weibull_pdf. Both weibull adstocks are more flexible
      # due to the changing decay rate over time, as opposed to the fixed decay rate for geometric. weibull_pdf
      # allows also lagging effect. Yet weibull adstocks are two-parametric and thus take longer to run.
      ,iterations = input$iterations  # number of allowed iterations per trial. For the simulated dataset with 11 independent
      # variables, 2000 is recommended for Geometric adsttock, 4000 for weibull_cdf and 6000 for weibull_pdf.
      # The larger the dataset, the more iterations required to reach convergence.
      
      ,nevergrad_algo = "TwoPointsDE" # recommended algorithm for Nevergrad, the gradient-free
      # optimisation library https://facebookresearch.github.io/nevergrad/index.html
      ,trials = input$trials # number of allowed trials. 5 is recommended without calibration,
      # 10 with calibration.
      
      # Time estimation: with geometric adstock, 2000 iterations * 5 trials
      # and 6 cores, it takes less than 1 hour. Both Weibull adstocks take up to twice as much time.
    )


    
    # Run ?hyper_names to check parameter definition
    hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
    
    # Example hyperparameters for Geometric adstock
    hyperparameters <- list(
      facebook_I_alphas = c(0.5, 3)
      ,facebook_I_gammas = c(0.3, 1)
      ,facebook_I_thetas = c(0, 0.3)
      
      ,print_S_alphas = c(0.5, 3)
      ,print_S_gammas = c(0.3, 1)
      ,print_S_thetas = c(0.1, 0.4)
      
      ,tv_S_alphas = c(0.5, 3)
      ,tv_S_gammas = c(0.3, 1)
      ,tv_S_thetas = c(0.3, 0.8)
      
      ,search_clicks_P_alphas = c(0.5, 3)
      ,search_clicks_P_gammas = c(0.3, 1)
      ,search_clicks_P_thetas = c(0, 0.3)
      
      ,ooh_S_alphas = c(0.5, 3)
      ,ooh_S_gammas = c(0.3, 1)
      ,ooh_S_thetas = c(0.1, 0.4)
      
      ,newsletter_alphas = c(0.5, 3)
      ,newsletter_gammas = c(0.3, 1)
      ,newsletter_thetas = c(0.1, 0.4)
    )
    
    # Example hyperparameters for Weibull CDF adstock
    # facebook_I_alphas = c(0.5, 3)
    # facebook_I_gammas = c(0.3, 1)
    # facebook_I_shapes = c(0.0001, 2)
    # facebook_I_scales = c(0, 0.1)
    
    # Example hyperparameters for Weibull PDF adstock
    # facebook_I_alphas = c(0.5, 3
    # facebook_I_gammas = c(0.3, 1)
    # facebook_I_shapes = c(0.0001, 10)
    # facebook_I_scales = c(0, 0.1)
    
    #### 2a-3: Third, add hyperparameters into robyn_inputs()
    
    InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
    
    #### 2a-4: Fourth (optional), model calibration / add experimental input
    
    ## Guide for calibration source
    
    # 1. We strongly recommend to use experimental and causal results that are considered
    # ground truth to calibrate MMM. Usual experiment types are people-based (e.g. Facebook
    # conversion lift) and geo-based (e.g. Facebook GeoLift).
    # 2. Currently, Robyn only accepts point-estimate as calibration input. For example, if
    # 10k$ spend is tested against a hold-out for channel A, then input the incremental
    # return as point-estimate as the example below.
    # 3. The point-estimate has to always match the spend in the variable. For example, if
    # channel A usually has 100k$ weekly spend and the experimental HO is 70%, input the
    # point-estimate for the 30k$, not the 70k$.
    
    # dt_calibration <- data.frame(
    #   channel = c("facebook_I",  "tv_S", "facebook_I")
    #   # channel name must in paid_media_vars
    #   , liftStartDate = as.Date(c("2018-05-01", "2017-11-27", "2018-07-01"))
    #   # liftStartDate must be within input data range
    #   , liftEndDate = as.Date(c("2018-06-10", "2017-12-03", "2018-07-20"))
    #   # liftEndDate must be within input data range
    #   , liftAbs = c(400000, 300000, 200000) # Provided value must be
    #   # tested on same campaign level in model and same metric as dep_var_type
    # )
    #
    # InputCollect <- robyn_inputs(InputCollect = InputCollect
    #                              , calibration_input = dt_calibration)
    
    
    ################################################################
    #### Step 2b: For known model specification, setup in one single step
    
    ## Specify hyperparameters as in 2a-2 and optionally calibration as in 2a-4 and provide them directly in robyn_inputs()
    
    # InputCollect <- robyn_inputs(
    #   dt_input = dt_simulated_weekly
    #   ,dt_holidays = dt_prophet_holidays
    #   ,date_var = "DATE"
    #   ,dep_var = "revenue"
    #   ,dep_var_type = "revenue"
    #   ,prophet_vars = c("trend", "season", "holiday")
    #   ,prophet_signs = c("default","default", "default")
    #   ,prophet_country = "DE"
    #   ,context_vars = c("competitor_sales_B", "events")
    #   ,context_signs = c("default", "default")
    #   ,paid_media_vars = c("tv_S", "ooh_S", 	"print_S", "facebook_I", "search_clicks_P")
    #   ,paid_media_signs = c("positive", "positive", "positive", "positive", "positive")
    #   ,paid_media_spends = c("tv_S", "ooh_S",	"print_S", "facebook_S", "search_S")
    #   ,organic_vars = c("newsletter")
    #   ,organic_signs = c("positive")
    #   ,factor_vars = c("events")
    #   ,cores = 6
    #   ,window_start = "2016-11-23"
    #   ,window_end = "2018-08-22"
    #   ,adstock = "geometric"
    #   ,iterations = 2000
    #   ,trials = 5
    #   ,hyperparameters = hyperparameters # as in 2a-2 above
    #   #,calibration_input = dt_calibration # as in 2a-4 above
    # )
    
    ################################################################
    #### Step 3: Build initial model
    
    
    # Run ?robyn_run to check parameter definition
    
    OutputCollect <- robyn_run(
      
      InputCollect = InputCollect # feed in all model specification
      , plot_folder = output_dir # plots will be saved in the same folder as robyn_object
      , pareto_fronts = 1
      , plot_pareto = FALSE
      # , calibration_constraint = 0.1 # run ?robyn_run to see description
      # , lambda_control = 1 # run ?robyn_run to see description
    )
    remove_modal_spinner() # remove it when done
    
    output$trainingStatus <- renderText({"Model trained"})
    output$modelSid <- renderUI({
      selectInput("modelSid2", "Select Model Sid", OutputCollect$xDecompAgg[, unique(solID)])
    })
  
  toReturn$OutputCollect <- OutputCollect
  toReturn$InputCollect <- InputCollect
  toReturn$hyperparameters <- hyperparameters
  
  return(toReturn)
  
  })
  #-------------------
  #     plot model
  #-------------------
  observeEvent(input$plotButton, {
    ## plot waterfall
    plotMediaShare <- toReturn$OutputCollect$xDecompAgg[robynPareto == 1 & rn %in% toReturn$InputCollect$paid_media_vars]
    output$plotMediaShare <- renderDataTable({plotMediaShare}) 
    #TODO adapt robynpareto, is not supposed to be equal to 1
    plotMediaShareLoop <- plotMediaShare[solID == input$modelSid2]
    rsq_train_plot <- plotMediaShareLoop[, round(unique(rsq_train), 4)]
    nrmse_plot <- plotMediaShareLoop[, round(unique(nrmse), 4)]
    decomp_rssd_plot <- plotMediaShareLoop[, round(unique(decomp.rssd), 4)]
    mape_lift_plot <- ifelse(!is.null(toReturn$InputCollect$calibration_input), plotMediaShareLoop[, round(unique(mape), 4)], NA)
    suppressWarnings(plotMediaShareLoop <- melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train"), measure.vars = c("spend_share", "effect_share", "roi_total", "cpa_total")))
    plotWaterfall <- toReturn$OutputCollect$xDecompAgg[robynPareto == 1]
    plotWaterfallLoop <- plotWaterfall[solID == input$modelSid2][order(xDecompPerc)]
    plotWaterfallLoop[, end := cumsum(xDecompPerc)]
    plotWaterfallLoop[, end := 1 - end]
    plotWaterfallLoop[, ":="(start = shift(end, fill = 1, type = "lag"),
                             id = 1:nrow(plotWaterfallLoop),
                             rn = as.factor(rn),
                             sign = as.factor(ifelse(xDecompPerc >= 0, "pos", "neg")))]
    
    output$plot2<-renderPlot({
      
        ggplot(plotWaterfallLoop, aes(x = id, fill = sign)) +
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
      
    })
    
  })
}
