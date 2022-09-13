library(occupationMeasurement)

# Generate the app_settings based on env variables
app_settings <- occupationMeasurement:::create_app_settings_from_env(
  verbose = TRUE
)

# Load the app
app(
  app_settings = app_settings
)
