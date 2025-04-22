# shiny/Dockerfile

FROM rocker/shiny:4.2.2

# 1. Install OS libraries needed by sf and RPostgres
RUN apt-get update && apt-get install -y \
      libudunits2-dev \
      libgdal-dev \
      libgeos-dev \
      libproj-dev \
      libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R packages
RUN install2.r --error \
    shiny \
    leaflet \
    sf \
    dplyr \
    DBI \
    RPostgres \
    ggplot2 \
    htmlwidgets

    FROM rocker/shiny:4.2.2

# (system deps and install2.r lines here…)

# Copy your custom config **before** start
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy your app & shapefiles
COPY app.R /srv/shiny-server/
COPY LGA_shapefile_cleaned/ /srv/shiny-server/LGA_shapefile_cleaned/

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server","--server-daemonize=0"]

