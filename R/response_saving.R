save_data <- function(table_name, data, session) {
  app_settings <- session$userData$app_settings

  data <- prepare_data_for_saving(table_name, data)

  # Automatically save data on disk
  if (app_settings$save_to_file) {
    session_id <- session$userData$user_info$session_id

    save_data_on_disk(table_name, data, session_id, app_settings)
  }

  # Support a user specified function for data saving
  if (is.function(app_settings$handle_data)) {
    app_settings$handle_data(table_name, data, session)
  }
}

append_tables <- list(
  "answers" = c(
    "respondent_id",
    "session_id",
    "page_id",
    "start",
    "end",
    "item_id",
    "question_text",
    "response_id",
    "response_text"
  ),
  "toggle_submitted" = c(
    "respondent_id",
    "session_id",
    "toggle_message",
    "time"
  ),
  "session_info" = c(
    "session_id",
    "url_search",
    "respondent_id",
    "history",
    "time_session_ended"
  ),
  "occupations_suggested" = c(
    "id",
    "auxco_id",
    "input_text",
    "score",
    "title",
    "task",
    "task_description",
    "kldb_title_short",
    "has_followup_questions",
    "session_id",
    "start"
  )
)
prepare_data_for_saving <- function(table_name, data) {
  # Column names used in data.table (for R CMD CHECK)
  question_text <- NULL

  # Ensure we're using data.table here.
  # Note: When using data.frames instead, once has to ensure that there are
  # no NULLs in data before conversion as converting from a list to a data.frame
  # will throw errors elsewise.
  data <- as.data.table(data)

  # Ensure the same columns when appending to a table
  is_append_table <- table_name %in% names(append_tables)
  if (is_append_table) {
    standard_columns <- append_tables[[table_name]]
    data <- ensure_columns(data, columns = standard_columns, warn_on_extra_columns = TRUE)
  }

  # Since the column question_text typically contains HTML, we can simply all
  # whitespace characters as they wouldn't be visible to participants either.
  # This way the CSV is both more readable and more concise.
  if (table_name == "answers") {
    data[, question_text := stringr::str_squish(question_text)]
  }

  return(data)
}

# Will take an existing data table and ensures that the provided columns exist
# in the output data frame as well as that they are in the provided order
ensure_columns <- function(dt, columns, warn_on_extra_columns = TRUE) {
  # Check for extra columns in the dt that will be discarded
  extra_columns <- colnames(dt)[!(colnames(dt) %in% columns)]
  if (warn_on_extra_columns && length(extra_columns) > 0) {
    warning(paste("Found additional columns, which will not be saved:", paste(extra_columns, collapse = ",")))
  }

  # Add the missing columns and fill them with NAs
  missing_columns <- columns[!(columns %in% colnames(dt))]
  for (missing_column in missing_columns) {
    dt[, missing_column] <- NA
  }

  # Select only the columns we actually want
  dt <- dt[, columns, with = FALSE]

  dt
}

# Save the provided data in a file on disk
save_data_on_disk <- function(table_name, data, session_id, app_settings) {
  is_append_table <- table_name %in% names(append_tables)

  # Make sure the output directory exists
  dir.create(app_settings$response_output_dir, recursive = TRUE, showWarnings = FALSE)

  # Create a unique file name
  file_name <- sprintf("%s_%s.csv", session_id, table_name)
  file_path <- file.path(app_settings$response_output_dir, file_name)

  # Write the file to the local system
  data.table::fwrite(
    x = data,
    file = file_path,
    quote = TRUE,
    # Only append for certain tables and only if the csv already exists
    append = is_append_table && file.exists(file_path)
  )
}

# Extract a clean question dataframe from the provided page's data
extract_questions_df <- function(page_data) {
  # If page_data is old / stale, return an empty data.table()
  if (page_data$status == "old") {
    return(data.table())
  }

  # Create a df / data.table from the question data
  if (length(page_data$questions) > 0) {
    question_dfs <- lapply(page_data$questions, as.data.table)
    df <- do.call(function(...) rbind(..., fill = TRUE), question_dfs)
    df$item_id <- names(page_data$questions)
  } else {
    df <- data.table()
  }

  # Add page-level data to the dataframe
  df$page_id <- page_data$page_id
  df$respondent_id <- page_data$respondent_id
  df$session_id <- page_data$session_id
  df$start <- page_data[["start"]]
  # The "end" value might not exist yet when a question is not completed
  if (!is.null(page_data[["end"]])) {
    df$end <- page_data[["end"]]
  }

  # Make sure we're always outputting the same columns
  df <- ensure_columns(df, columns = append_tables[["answers"]], warn_on_extra_columns = TRUE)

  return(df)
}

# Extract all question data in wide format for a whole questionnaire
extract_questions_wide <- function(questionnaire_data) {
  # Column names used in data.table (for R CMD CHECK)
  questionnaire_order <- NULL

  # Return empty data.table if no / empty data has been passed
  if (
    is.null(questionnaire_data) ||
      length(questionnaire_data) == 0
  ) {
    return(data.table())
  }

  # Call extract_questions_df for every page in the questionnaire and
  # combine the results with rbind
  all_questions <- do.call(rbind, lapply(questionnaire_data, extract_questions_df))
  all_questions$questionnaire_order <- seq.int(nrow(all_questions))

  # Convert all measure_vars to character (for consistency and to get rid of a warning from before)
  measure_vars <- c("response_id", "response_text")
  all_questions[, (measure_vars) := lapply(.SD, as.character), .SDcols = measure_vars]

  # Reshape the data into an ever longer format where each row corresponds to
  # exactly one response-type per question per page
  all_responses_long <- melt(
    all_questions,
    measure.vars = measure_vars,
    variable.name = "response_type",
    value.name = "value"
  )
  # Always convert value to character (for consistency)
  all_responses_long$value <- as.character(all_responses_long$value)
  # Remove "response_" prefix
  all_responses_long$response_type <- stringr::str_remove(all_responses_long$response_type, "response_")
  # Construct the new column names
  all_responses_long$column_name <- paste(
    "P", all_responses_long$page_id,
    "Q", all_responses_long$item_id,
    "R", all_responses_long$response_type,
    sep = "_"
  )

  # Order column names based on when they appear in the questionnaire and
  # save them for later reordering of columns after reshaping
  setorder(all_responses_long, questionnaire_order)
  ordered_column_names <- unique(all_responses_long$column_name)

  # Transform the question responses into a wide format
  question_answers_wide <- dcast(
    all_responses_long,
    respondent_id + session_id ~ column_name,
    value.var = "value",
    fun.aggregate = last,
    fill = NA
  )

  # Since dcast will sort columns alphabetically, we have to reintroduce the
  # correct order manually
  setcolorder(question_answers_wide, c(
    # Key columns correspond to the left hand site when casting
    key(question_answers_wide),
    ordered_column_names
  ))

  return(question_answers_wide)
}

# Extract the final results_overview data, combining user and questionnaire data
# in wide format
extract_results_overview <- function(session) {
  # Session was never initialized (e.g. due to missing id)
  if (is.null(session$userData$user_info$session_id)) {
    return(NULL)
  }

  user_data <- data.table(
    session_id = session$userData$user_info$session_id,
    url_search = session$userData$user_info$url_search
  )

  question_answers_wide <- extract_questions_wide(session$userData$questionnaire_data)

  # TODO: Maybe make this code more flexible to support multiple numbers of follow_up questions in the future
  if (!is.null(question_answers_wide$P_select_suggestion_Q_default_R_id)) {
    # Generate named list of followup_answers
    question_ids <- get_followup_questions(
      suggestion_id = question_answers_wide$P_select_suggestion_Q_default_R_id
    ) |>
      sapply(function(x) x$question_id)
    followup_answers <- list(
      question_answers_wide$P_followup_1_Q_default_R_id,
      question_answers_wide$P_followup_2_Q_default_R_id
    )
    length(question_ids) <- length(followup_answers)
    names(followup_answers) <- question_ids

    # Retrieve the final codes
    final_codes <- get_final_codes(
      suggestion_id = question_answers_wide$P_select_suggestion_Q_default_R_id,
      followup_answers = followup_answers,
      code_type = c("isco_08", "kldb_10")
    )
    user_data$isco_08 <- final_codes$isco_08
    user_data$kldb_10 <- final_codes$kldb_10
  }

  # Merge user and answer data (if necessary)
  if (nrow(question_answers_wide) == 0) {
    final_data <- user_data
  } else {
    final_data <- merge(
      user_data,
      question_answers_wide,
      by = "session_id"
    )
  }

  return(final_data)
}

# Save the final results_overview
save_results_overview <- function(session) {
  final_data <- extract_results_overview(session = session)

  # Skip if there is no data to save (i.e. no session has been started)
  if (is.null(final_data)) {
    return()
  }

  hash <- digest::digest(final_data)

  # Check whether response_overview has already been saved
  if (
    is.null(session$userData$results_overview_saved_hash) ||
      hash != session$userData$results_overview_saved_hash
  ) {
    save_data("results_overview", final_data, session)

    # Remember hash
    session$userData$results_overview_saved_hash <- hash
  }
}

#' Convenience function to aggregate all saved results_overview files.
#'
#' Expects data to be saved as files.
#' @param app_settings The app_settings configuration, should be the same as
#'   used in [app()].
#' @return A combined data.table of user data (based on results_overview) or
#'   NULL if there are no files.
#' @export
#' @examples
#' \dontshow{data.table::setDTthreads(1)}
#'
#' \dontrun{
#' app_settings <- create_app_settings(save_to_file = TRUE)
#' if (interactive()) {
#'   get_responses(app_settings = app_settings)
#' }
#' }
get_responses <- function(app_settings = create_app_settings(save_to_file = TRUE)) {
  # Note: This has to match with the pattern of filenames specified in save_data_on_disk
  files_to_read <- list.files(app_settings$response_output_dir, pattern = "_results_overview.*\\.csv$", full.names = T)

  if (length(files_to_read) == 0) {
    warning("There are no results_overview files to be read, returning NULL.")
    return(NULL)
  }

  # Load the different result overviews
  list_of_result_overviews <- lapply(files_to_read, fread, colClasses = "character")

  # And stitch them together into a single one
  combined_result_overviews <- do.call(function(...) rbind(..., fill = TRUE), list_of_result_overviews)

  if (any(duplicated(combined_result_overviews$respondent_id))) {
    warning("There were technical problems during data collection: Multiple records were saved from a single respondent_id! Further data cleaning is required.")
  }

  # TODO: provide functionality to clean duplicate respondent_ids automatically

  combined_result_overviews
}
