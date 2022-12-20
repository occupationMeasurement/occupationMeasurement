# `occupationmeasurement` Docker Images 🐋

# Using the Docker Images

[Pre-built Versions of the Docker Images](https://github.com/orgs/occupationMeasurement/packages?ecosystem=container) for the occupationMeasurement package are available on the [Github container registry](https://ghcr.io). You can immediately use both the app and the api, by running the commands below. 

```bash
# Start the interactive shiny app
# it can be reached at http://localhost:3838
docker run --rm -p 3838:3838 -v $(pwd)/output:/srv/shiny-server/output ghcr.io/occupationmeasurement/app:latest

# Start the api
# it can be reached at http://localhost:8000
# Swagger documentation can be viewed at http://localhost:8000/__docs__/
docker run --rm -p 8000:8000 -v $(pwd)/output:/output -e ALLOW_ORIGIN="*" -e REQUIRE_IDENTIFIER=TRUE ghcr.io/occupationmeasurement/api:latest
```

# Building Images

Docker images can be built by running `build.sh` from the project root directory.

> It's also possible to use `docker compose` or `docker` directly, but please note, that the Dockerfiles expect the built R package under `docker/occupationMeasurement.tar.gz` to install it within the containers.

## Starting Built Images

After the images have been built using the command above, they can be started via the following commands.

```bash
# Start the interactive shiny app
# it can be reached at http://localhost:3838
docker run --rm -p 3838:3838 -v $(pwd)/output:/srv/shiny-server/output occupationmeasurement/app

# Start the api
# it can be reached at http://localhost:8000
# Swagger documentation can be viewed at http://localhost:8000/__docs__/
docker run --rm -p 8000:8000 -v $(pwd)/output:/output -e ALLOW_ORIGIN="*" -e REQUIRE_IDENTIFIER=TRUE occupationmeasurement/api
```

> The provided URLs assume that you are executing commands on your own machine. When running the containers in the web, IP addresses will be different.

# Sharing Images

## Publishing Images on a Container Registry

Docker images can be published to the [Github container registry](https://ghcr.io) by running the `publish.sh` script from the project root directory. Please note, that you always need to first *build* the images (see section above on how to do that), before *publishing* them.

## Saving Images for Sharing (as a file)

To share a docker image without using docker hub you need to save it as a file. This can be done via the following command to e.g. save the image for `occupationmeasurement/api`.

```bash
docker save 'occupationmeasurement/api' | gzip > occupationmeasurement_api.tar.gz
```

To load the image from a file (on e.g. another machine) you need to run the following command.

```bash
docker load -i occupationmeasurement_api.tar.gz
```
