#' Create app_settings.
#'
#' This is the primary and most convenient way of configuring the app.
#'
#' @param require_id Are user_ids required?
#'   Defaults to FALSE
#' @param warn_before_leaving Should users be warned that their progress will
#'   be lost upon leaving the site? Defaults to FALSE.
#' @param skip_followup_types A vector of strings corresponding to the
#'   question_type of followup_question that should be skipped.
#' @param save_to_file Should responses be saved as files in
#'   response_output_dir? Defaults to use the SAVE_TO_FILE environment variable
#'   or TRUE if it is not set.
#' @param response_output_dir Path to the directory in which to store data
#'   from the app. Defaults to `./output/responses/`.
#' @param handle_data Callback function to handle data from the app.
#'   This setting takes a function that get's passed 3 parameters:
#'   table_name (A reference name indicating which data to save),
#'   data (A dataframe of data to save), session (the user's current session).
#' @param get_job_suggestion_params List of parameters to pass to
#'   get_job_suggestion. Refer to [get_job_suggestions()] for a list of
#'   supported parameters.
#' @param verbose Should additional output be printed when running?
#'   Defaults to TRUE.
#' @param .validate Whether the created app_settings should be validated.
#'   Defaults to TRUE.
#'
#' @inheritParams get_job_suggestions
#'
#' @return A list of app_settings.
#' @export
#'
#' @examples
#' app_settings <- create_app_settings(require_id = TRUE)
create_app_settings <- function(suggestion_type = "auxco",
                                require_id = FALSE,
                                warn_before_leaving = FALSE,
                                skip_followup_types = c(),
                                save_to_file = TRUE,
                                response_output_dir = file.path("output", "responses"),
                                handle_data = NULL,
                                get_job_suggestion_params = NULL,
                                verbose = TRUE,
                                .validate = TRUE) {
  final_app_settings <- list(
    suggestion_type = suggestion_type,
    require_id = require_id,
    warn_before_leaving = warn_before_leaving,
    skip_followup_types = skip_followup_types,
    save_to_file = save_to_file,
    response_output_dir = response_output_dir,
    handle_data = handle_data,
    get_job_suggestion_params = get_job_suggestion_params,
    verbose = verbose
  )

  if (.validate) {
    validate_app_settings(final_app_settings)
  }

  return(final_app_settings)
}

validate_app_settings <- function(app_settings) {
  # Validate sugestion_type
  possible_suggestion_types <- c(
    "auxco", "kldb"
  )
  stopifnot(app_settings$suggestion_type %in% possible_suggestion_types)

  # Validate followup types
  possible_followup_types <- c(
    "anforderungsniveau",
    "aufsicht",
    "spezialisierung",
    "sonstige"
  )
  stopifnot(
    sum(app_settings$skip_followup_types %in% possible_followup_types) ==
      length(app_settings$skip_followup_types)
  )

  if (!is.null(app_settings$get_job_suggestion_params)) {
    if (!is.null(app_settings$get_job_suggestion_params$suggestion_type)) {
      warning("suggestion_type should be set on the app_settings level, not in app_settings$get_job_suggestion_params")
    }
  }

  # Add warnings for odd / dangerous settings
  if (!app_settings$require_id) {
    warning("User Ids are currently not required, this could lead to unmatchable data.")
  }

  if (!app_settings$save_to_file) {
    warning("No data will be saved, as saving to files has been disabled.")
  }
}

# Generate app_settings based on environment variables
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

  # Iterate over all possible parameters and extract them if present
  app_settings_params_from_env <- list()
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

      app_settings_params_from_env[[param]] <- value
    }
  }

  # Convert parameters back to lowercase:
  names(app_settings_params_from_env) <- names(app_settings_params_from_env) |>
    tolower()

  if (verbose) {
    paste(
      "Detected variables:",
      app_settings_params_from_env |>
        print() |>
        utils::capture.output() |>
        paste(collapse = "\n"),
      "List of supported environment variables:",
      possible_params |>
        print() |>
        utils::capture.output() |>
        paste(collapse = "\n"),
      "Check ?occupationMeasurement::create_app_settings for explanations.",
      sep = "\n"
    ) |>
      message()
  }

  # Actually generate the app_settings taking into account defaults etc.
  app_settings <- do.call(create_app_settings, app_settings_params_from_env)

  return(app_settings)
}
