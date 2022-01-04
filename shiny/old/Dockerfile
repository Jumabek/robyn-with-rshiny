FROM rocker/shiny-verse:latest
# install R package dependencies
RUN apt-get update && apt-get -qq -y install curl \
    libssl-dev \
    libcurl4-openssl-dev \
    ## clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
   
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssh2-1-dev
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('bigrquery', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('facebookexperimental/Robyn/R')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinybusy', repos='http://cran.rstudio.com/')"

   
## Install packages from CRAN
RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    googleAuthR \
    ## install Github packages
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
## assume shiny app is in build folder /shiny
COPY ./Shiny/ /srv/shiny-server/shiny/
# select port
EXPOSE 3838
# run app
CMD ["/usr/bin/shiny-server.sh"]