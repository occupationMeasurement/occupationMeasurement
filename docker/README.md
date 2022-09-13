# occupation_measurement Docker Images 🐋

## Building Images

Docker images can be built by running `build.sh` from the project root directory.

> It's also possible to use `docker compose` or `docker` directly, but please note, that the Dockerfiles expect the built R package under `docker/occupationMeasurement.tar.gz` to install it within the containers.


## Starting images

After the images have been built using the command above, they can be started via the following commands.

```bash
# Start the interactive shiny app
# it can be reached at http://localhost:3838
docker run --rm -p 3838:3838 occupation_measurement/app

# Start the api
# it can be reached at http://localhost:8000
# Swagger documentation can be viewed at http://localhost:8000/__docs__/
docker run --rm -p 8000:8000 occupation_measurement/api
```

> The provided URLs assume that you are executing commands on your own machine. When running the containers in the web, IP addresses will be different.

## Saving Images for Sharing (as a file)

To share a docker image without using docker hub you need to save it as a file. This can be done via the following command to e.g. save the image for `occupation_measurement/api`.

```bash
docker save 'occupation_measurement/api' | gzip > occupation_measurement_api.tar.gz
```

To load the image from a file (on e.g. another machine) you need to run the following command.

```bash
docker load -i occupation_measurement_api.tar.gz
```
