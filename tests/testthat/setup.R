# It seems shinytest2 does not correctly load the global.R, so we manually evaluate it for now.
# Note that for some reason this code has to be called before the load_app_env

library(data.table)
library(shiny)

# Use a locally installed occupationMeasurement package for all tests
local_package(.local_envir = teardown_env())

# Load application support files into testing environment
suppressWarnings(shinytest2::load_app_env())
