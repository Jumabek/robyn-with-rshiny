install.packages("shiny")
install.packages('DT')
install.packages('tidyverse')
install.packages('bigrquery')
install.packages("remotes") 
install.packages("shinythemes")
install.packages("shinybusy")
remotes::install_github("facebookexperimental/Robyn/R")


## Must install the python library Nevergrad once
## ATTENTION: The latest Python 3.10 version will cause Nevergrad installation error
## See here for more info about installing Python packages via reticulate
## https://rstudio.github.io/reticulate/articles/python_packages.html

## Load 
library(reticulate)
## Option 1: nevergrad installation via PIP
virtualenv_create("~/.virtualenvs/r-test")
use_virtualenv("C:/Users/Bastien/.virtualenvs/r-reticulate/Scripts", required = TRUE)
py_install("nevergrad", pip = TRUE)
# py_config() # Check your python version and configurations
## In case nevergrad still can't be installed,
# Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/r-reticulate/bin/python")
# Reset your R session and re-install Nevergrad with option 1

## Option 2: nevergrad installation via conda
# conda_create("r-reticulate", "Python 3.9") # Only works with <= Python 3.9 sofar
# use_condaenv("r-reticulate")
# conda_install("r-reticulate", "nevergrad", pip=TRUE)
# py_config() # Check your python version and configurations
## In case nevergrad still can't be installed,
## please locate your python file and run this line with your path:
# use_python("~/Library/r-miniconda/envs/r-reticulate/bin/python3.9")
# Alternatively, force Python path for reticulate with this:
# Sys.setenv(RETICULATE_PYTHON = "~/Library/r-miniconda/envs/r-reticulate/bin/python3.9")
# Finally, reset your R session and re-install Nevergrad with option 2

# Check this issue for more ideas to debug your reticulate/nevergrad issues:
# https://github.com/facebookexperimental/Robyn/issues/189

## Must install the python library Nevergrad once
## ATTENTION: The latest Python 3.10 version will cause Nevergrad installation error
## See here for more info about installing Python packages via reticulate
## https://rstudio.github.io/reticulate/articles/python_packages.html

## Load 
library(reticulate)
## Option 1: nevergrad installation via PIP
virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
Sys.setenv(RETICULATE_PYTHON = "C:/Users/Bastien/.virtualenvs/r-reticulate/Scripts")
py_install("nevergrad", pip = TRUE)
py_config() # Check your python version and configurations
## In case nevergrad still can't be installed,
# Reset your R session and re-install Nevergrad with option 1

if (!reticulate::py_module_available("nevergrad")) {
  cat("This is yout current python config:\n")
  print(reticulate::py_config())
  stop("Nevergrad is not available. Python config has been printed above.")
} else {
  cat("You have nevergrad and it is available to R!\n")
}
