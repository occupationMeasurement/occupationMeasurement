# Note: This script is expected to be called from the root occupationMeasurement directory

# Build the occupationMeasurement package (to pass it into the docker container and install it)
R -e "devtools::build(pkg = '.', path = 'docker/occupationMeasurement.tar.gz')"

# Build the API image
docker compose -f docker/docker-compose.yml build api

# Build the interactive shiny app image
docker compose -f docker/docker-compose.yml build app
