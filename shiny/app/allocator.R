library(Robyn)

allocate <- function(InputCollect, OutputCollect, select_model, scenario){

  ################################################################
  #### Step 5: Get budget allocation based on the selected model above
  
  ## Budget allocator result requires further validation. Please use this result with caution.
  ## Don't interpret budget allocation result if selected result doesn't meet business expectation
  
  # Check media summary for selected model
  #OutputCollect$xDecompAgg[solID == select_model & !is.na(mean_spend)
  #                          , .(rn, coef,mean_spend, mean_response, roi_mean
  #                             , total_spend, total_response=xDecompAgg, roi_total, solID)]
  
  # Run ?robyn_allocator to check parameter definition
  # Run the "max_historical_response" scenario: "What's the revenue lift potential with the
  # same historical spend level and what is the spend mix?"

  AllocatorCollect <- robyn_allocator(
    InputCollect = InputCollect
    , OutputCollect = OutputCollect
    , select_model = select_model
    , scenario = scenario
    , channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7)
    , channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
    ,ui = TRUE
  )

  return(AllocatorCollect)

  # View allocator result. Last column "optmResponseUnitTotalLift" is the total response lift.
  #AllocatorCollect$dt_optimOut
}