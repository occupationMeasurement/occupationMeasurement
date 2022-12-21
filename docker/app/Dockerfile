FROM rocker/shiny:4.2.1

LABEL org.opencontainers.image.source=https://github.com/occupationMeasurement/occupationMeasurement
LABEL org.opencontainers.image.description="Container image of the interactive occupationMeasurement shiny application."
LABEL org.opencontainers.image.licenses=MIT

# Install libxml2 (a dependency of the xml2 R package)
RUN apt-get update && apt-get install libxml2

# Install the package and its dependencies
COPY occupationMeasurement.tar.gz /occupationMeasurement.tar.gz
COPY container-setup.R /container-setup.R
RUN R -e "source('/container-setup.R')"
RUN rm /occupationMeasurement.tar.gz

# Clear the shiny-server directory
RUN rm -r /srv/shiny-server
RUN mkdir /srv/shiny-server

# Make sure shiny is allowed to write to the output directory
RUN mkdir /srv/shiny-server/output
RUN chown -R shiny.shiny /srv/shiny-server/output

# Add our app.R
COPY app/app.R /srv/shiny-server/app.R
