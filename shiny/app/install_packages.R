Sys.setenv(DOWNLOAD_STATIC_LIBV8=1)

install.packages("remotes")
install.packages("reticulate")
install.packages("shiny")
install.packages("glue")
install.packages("DT")
install.packages("bigrquery")
install.packages("shinybusy")
install.packages("shinythemes")

remotes::install_github("facebookexperimental/Robyn/R")
