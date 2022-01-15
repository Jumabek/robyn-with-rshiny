library(Robyn)

allocate <- function(robyn_object,
                     InputCollect, 
                     OutputCollect, 
                     select_model, 
                     scenario, 
                     channel_constr_low,
                     channel_constr_up,
                     expected_spend, 
                     expected_spend_days){

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
  robyn_save(robyn_object = robyn_object # model object location and name
             , select_model = select_model # selected model ID
             , InputCollect = InputCollect # all model input
             , OutputCollect = OutputCollect # all model output
  )
  
  AllocatorCollect <- robyn_allocator(
      robyn_object = robyn_object
    , scenario = scenario
    , expected_spend = expected_spend
    , expected_spend_days = expected_spend_days
    , channel_constr_low = channel_constr_low
    , channel_constr_up = channel_constr_up
    ,ui = TRUE
  )

  return(AllocatorCollect)

  # View allocator result. Last column "optmResponseUnitTotalLift" is the total response lift.
  #AllocatorCollect$dt_optimOut
}