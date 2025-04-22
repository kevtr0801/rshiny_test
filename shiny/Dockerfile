# shiny/Dockerfile

# 1. Start from RStudio’s Shiny Server image
FROM rocker/shiny:4.2.2

# 2. Install the R packages your app needs
RUN install2.r --error \
    shiny \
    leaflet \
    sf \
    dplyr \
    DBI \
    RPostgres \
    ggplot2 \
    htmlwidgets

# 3. Copy your app code into the container
COPY app.R /srv/shiny-server/

# 4. Ensure permissions are correct
RUN chown -R shiny:shiny /srv/shiny-server

# 5. Expose Shiny’s default port
EXPOSE 3838

# 6. Launch Shiny Server
CMD ["/usr/bin/shiny-server", "--server-daemonize=0"]
