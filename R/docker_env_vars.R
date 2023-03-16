# Generate app_settings based on environment variables
# Mostly meant to work with Docker, but the functions here may also be useful in other settings.
create_app_settings_from_env <- function(verbose = FALSE) {
  if (verbose) message("Loading settings for occupationMeasurement app")

  # Get a list of possible parameters
  # (They should all be in upper case)
  possible_params <- create_app_settings |>
    args() |>
    as.list() |>
    names() |>
    toupper()

  possible_params <- possible_params[
    !(
      # Remove parameters starting with "."
      startsWith(possible_params, ".") |
        # Remove empty parameters
        possible_params == ""
    )
  ]

  # Parameters that should be evaluated as R code
  # This should not really be an attack vector, as if you have access to the
  # env vars you probably also have acccess to the container itself?
  eval_params <- c(
    "handle_data",
    "get_job_suggestion_params"
  ) |>
    toupper()

  app_settings_params_from_env <- parse_env_settings(
    possible_params = possible_params,
    eval_params = eval_params,
    verbose = verbose,
    reference_function = "occupationMeasurement::create_app_settings"
  )

  # Provide default params for create_app_settings
  default_app_settings_params <- list(
    save_to_file = TRUE
  )
  app_settings_params <- utils::modifyList(
    default_app_settings_params,
    app_settings_params_from_env
  )

  # Actually generate the app_settings taking into account defaults etc.
  app_settings <- do.call(create_app_settings, app_settings_params)

  return(app_settings)
}

parse_env_settings <- function(possible_params, eval_params = c(), verbose = FALSE, reference_function = NULL) {
  # Iterate over all possible parameters and extract them if present
  settings_params_from_env <- list()
  for (param in possible_params) {
    value <- Sys.getenv(param)

    # Try and convert to a number
    value_num <- suppressWarnings(as.numeric(value))

    # Set value if it exists
    if (value != "") {
      # Do a crude conversion from strings to different types
      if (param %in% eval_params) {
        # Evaluate parameter as R-Code
        value <- eval(value)
      } else if (value == "TRUE") {
        value <- TRUE
      } else if (value == "FALSE") {
        value <- FALSE
      } else if (!is.na(value_num)) {
        value <- value_num
      }

      settings_params_from_env[[param]] <- value
    }
  }

  # Convert parameters back to lowercase
  names(settings_params_from_env) <- names(settings_params_from_env) |>
    tolower()

  if (verbose) {
    paste(
      "Detected variables:",
      settings_params_from_env |>
        print() |>
        utils::capture.output() |>
        paste(collapse = "\n"),
      "List of supported environment variables:",
      possible_params |>
        print() |>
        utils::capture.output() |>
        paste(collapse = "\n"),
      if (!is.null(reference_function)) paste0("Check ?", reference_function, " for explanations.") else "",
      sep = "\n"
    ) |>
      message()
  }

  return(settings_params_from_env)
}
