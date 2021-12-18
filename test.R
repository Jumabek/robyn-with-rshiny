library(data.table)
source("robynR/allocator.R")
source("robynR/auxiliary.R")
source("robynR/checks.R")
source("robynR/data.R")
source("robynR/imports.R")
source("robynR/inputs.R")
source("robynR/model.R")
source("robynR/refresh.R")
source("robynR/transformation.R")
library(Robyn)


OutputCollect_test <- robyn_run(
  
  InputCollect = InputCollect # feed in all model specification
  , plot_folder = output_dir # plots will be saved in the same folder as robyn_object
  , pareto_fronts = 3
  , plot_pareto = TRUE
  # , calibration_constraint = 0.1 # run ?robyn_run to see description
  # , lambda_control = 1 # run ?robyn_run to see description
)