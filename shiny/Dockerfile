FROM rocker/shiny:4.2.2

# Install OS libraries needed by sf & units and by RPostgres
RUN apt-get update && apt-get install -y \
      libudunits2-0 \
      libudunits2-dev \
      libgdal-dev \
      libgeos-dev \
      libproj-dev \
      libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
# 2. Install R packages, including the tidyverse umbrella
RUN install2.r --error \
    shiny \
    leaflet \
    sf \
    dplyr \
    tidyr \
    stringr \
    DBI \
    RPostgres \
    ggplot2 \
    htmlwidgets \
    tidyverse

# Copy app and shapefile
COPY app.R /srv/shiny-server/
COPY LGA_shapefile_cleaned/ /srv/shiny-server/LGA_shapefile_cleaned/

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server","--server-daemonize=0"]
