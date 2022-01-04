FROM ubuntu:focal

ENV TZ=UTC
ENV TEST_ENV=""

RUN mkdir /app
COPY install_packages.R /app/install_packages.R

RUN apt update
RUN apt -y install dirmngr gnupg apt-transport-https ca-certificates software-properties-common
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
RUN apt update
RUN apt -y install libgl1-mesa-glx libegl1-mesa libxrandr2 libxrandr2 libxss1 libxcursor1 libxcomposite1 libasound2 libxi6 libxtst6 wget python3 python3-dev python3-pip python3-venv libcurl4-openssl-dev libv8-dev r-base
RUN Rscript /app/install_packages.R
RUN Rscript /


RUN mkdir -p /out

COPY app.R /app/app.R

CMD Rscript /app/app.R