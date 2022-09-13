# Note: We're using the Global environment here (which is suboptimal)
# but due to complicated usage of different environments here for
# the shiny app and api, this is the only changes persisted into child
# environments so far
local_package <- function(pkg = file.path("..", ".."), .local_envir = .GlobalEnv) {
  # Use a temporary library location for the duration of the environment
  withr::local_temp_libpaths(.local_envir = .local_envir)

  # Install the package
  devtools::install(pkg = pkg, quick = TRUE, quiet = TRUE, upgrade = "never")
}
