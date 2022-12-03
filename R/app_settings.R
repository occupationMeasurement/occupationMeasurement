#' Create app_settings.
#'
#' This is the primary and most convenient way of configuring the app.
#'
#' @param default_num_suggestions The number of suggestions to generate and
#'   display to users. Accepts all positive integers.
#'   Defaults to 5.
#' @param require_respondent_id Are respondent_ids required?
#'   Defaults to FALSE
#' @param warn_before_leaving Should users be warned that their progress will
#'   be lost upon leaving the site? Defaults to FALSE.
#' @param skip_followup_types A vector of strings corresponding to the
#'   question_type of followup_question that should be skipped. Allowed `
#'   values: c("anforderungsniveau", "aufsicht", "spezialisierung", "sonstige")
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
#'   supported parameters. Note that the parameter aggregate_score_threshold
#'   needs to be set on [page_first_freetext()] or [page_second_freetext()].
#' @param display_page_ids Whether `page_ids` should be displayed within the
#'   questionnaires.
#' @param default_tense We may not always want to ask for the current
#'   occupation, but maybe also for the previous occupation in case of
#'   pensioners etc. with a value of "past".
#'   Possible values are "present" (default), "past".
#'   This setting can be overwritten on a session-by-session basis with the
#'   URL-Query parameter "tense".
#' @param default_extra_instructions Display additional instructions for e.g.
#'   an interviewer conducting an interview.
#'   Possible values are "on" (default), "off".
#'   This setting can be overwritten on a session-by-session basis with the
#'   URL-Query parameter "extra_instructions".
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
#' app_settings <- create_app_settings(require_respondent_id = TRUE)
create_app_settings <- function(suggestion_type = "auxco-1.2.x",
                                default_num_suggestions = 5,
                                require_respondent_id = FALSE,
                                warn_before_leaving = FALSE,
                                skip_followup_types = c(),
                                save_to_file = TRUE,
                                response_output_dir = file.path("output", "responses"),
                                handle_data = NULL,
                                get_job_suggestion_params = NULL,
                                display_page_ids = FALSE,
                                default_tense = "present",
                                default_extra_instructions = "on",
                                verbose = TRUE,
                                .validate = TRUE) {
  final_app_settings <- list(
    suggestion_type = suggestion_type,
    default_num_suggestions = default_num_suggestions,
    require_respondent_id = require_respondent_id,
    warn_before_leaving = warn_before_leaving,
    skip_followup_types = skip_followup_types,
    save_to_file = save_to_file,
    response_output_dir = response_output_dir,
    handle_data = handle_data,
    get_job_suggestion_params = get_job_suggestion_params,
    display_page_ids = display_page_ids,
    default_tense = default_tense,
    default_extra_instructions = default_extra_instructions,
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
    "auxco-1.2.x", "kldb-2010"
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
  if (!app_settings$require_respondent_id) {
    warning("User Ids are currently not required, this could lead to unmatchable data.")
  }

  if (!app_settings$save_to_file) {
    warning("No data will be saved, as saving to files has been disabled.")
  }
}
