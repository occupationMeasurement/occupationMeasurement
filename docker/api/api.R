default_api_settings <- list(
  # Don't auto-start the api as we do it manually below
  start = FALSE,
  log_to_file = TRUE
)
user_api_settings <- occupationMeasurement:::parse_env_settings(
  possible_params = c(
    "LOG_TO_CONSOLE",
    "LOG_TO_FILE",
    "LOG_FILEPATH",
    "REQUIRE_IDENTIFIER",
    "ALLOW_ORIGIN"
  ),
  verbose = TRUE
)

# Combine user and default settings
api_settings <- utils::modifyList(default_api_settings, user_api_settings)

# Load our custom api
pr <- do.call(occupationMeasurement::api, api_settings)

# Adapted from the original docker image
# https://github.com/rstudio/plumber/blob/main/Dockerfile
args <- list(host = "0.0.0.0", port = 8000)
if (packageVersion("plumber") >= "1.0.0") {
  pr$setDocs(TRUE)
} else {
  args$swagger <- TRUE
}
do.call(pr$run, args)
