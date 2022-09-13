# Definition: https://github.com/rstudio/plumber/blob/main/Dockerfile
FROM rstudio/plumber:v1.2.0

# Install the package and its dependencies
COPY occupationMeasurement.tar.gz /occupationMeasurement.tar.gz
COPY container-setup.R /container-setup.R
RUN R -e "source('/container-setup.R')"
RUN rm /occupationMeasurement.tar.gz

# Remove the plumber example to avoid possible confusion
RUN rm ~/plumber.R

# Remove the entrypoint from the original image
ENTRYPOINT []

COPY api/api.R /api.R
CMD R -e "source('/api.R')"
