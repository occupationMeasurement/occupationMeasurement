# Load our custom api
pr <- occupationMeasurement::api(start = FALSE)

# Adapted from the original docker image
# https://github.com/rstudio/plumber/blob/main/Dockerfile
args <- list(host = "0.0.0.0", port = 8000)
if (packageVersion("plumber") >= "1.0.0") {
  pr$setDocs(TRUE)
} else {
  args$swagger <- TRUE
}
do.call(pr$run, args)
