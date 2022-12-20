# Publish the docker containers to the github container registry
# More Info:
# https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry

docker tag occupationmeasurement/api ghcr.io/occupationmeasurement/api:latest
docker push ghcr.io/occupationmeasurement/api:latest

# TODO: Also publish app docker image
# docker tag occupationmeasurement/app ghcr.io/occupationmeasurement/app:latest
# docker push ghcr.io/occupationMeasurement/app:latest
