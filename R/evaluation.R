#' Evaluate the Potential Predictive Performance of the Interactive App
#'
#' This function takes in a dataframe that contains free text responses and
#' their accompanying occupational classification codes. It will then generate
#' suggestions for each of these free text responses and will check whether
#' the provided occupational classification code overlaps with the codes that
#' could be reached by the suggestions.
#'
#' Because our interactive coding app uses followup questions to explicitly ask
#' for e.g. the level of supervision a respondent may have, one suggested answer
#' may correspond to multiple occupational classification codes. This function
#' will therefore *slightly overestimate* performance, as we assume that
#' respondents will answer the followup questions in the correct way to arrive
#' the final code. This problem can unfortunately not be solved, as ignoring the
#' potential different codings based on followup answers, would lead to a
#' *severe underestimation* of performance.
#'
#' This function sohuld still be able to provide you with a rough estimate of
#' what levels of performance to expect when employing the tool in the field.
#'
#' @param test_data A data.frame or data.table with at least two columns,
#'   containing free text responses and the corresponding occupational
#'   classification codes.
#' @param freetext_colname The name of the column containing freetext responses.
#' @param code_colname The name of the column containing occupational
#'   classification codes.
#' @param code_format The coding format of the occupational classification codes
#'   possible values are "kldb-2010" or "isco-08".
#'   Defaults to "kldb-2010".
#' @param followup_questions Should occupational classification codes based on
#'   answers to followup questions be included? See the function description for
#'   a detailed explanation. Setting this to `TRUE` most likely leads to a
#'   *slight overestimation* of performance, setting this to `FALSE` most likely
#'   leads to a *severe underestimation* of performance.
#' @param app_settings The app_settings you plan to use with [app()]. Check the
#'   documentation for create_app_settings to learn about the options.
#'   Especially options related to the generation of suggestions are relevant.
#' @param suggestion_type_options A list with options for generating
#'   suggestions. Only interesting if you plan to use a custom version of e.g.
#'   the AuxCo or other coding systems.
#'
#' @return An invisible list with aggregated scores (`$score`) and a detailed
#'   performance report in form of a data.table (`$data`).
#' @export
#'
#' @examples
#' evaluate_performance(
#'   test_data = data.frame(
#'     freetext_answer = c(
#'       "Koch",
#'       "Bäcker",
#'       "Donaudampschifffahrtskapitän",
#'       "Arbeiter"
#'     ),
#'     kldb_code = c(
#'       "29302",
#'       "29222",
#'       "52423",
#'       "72213"
#'     )
#'   ),
#'   freetext_colname = "freetext_answer",
#'   code_colname = "kldb_code",
#'   code_format = "kldb-2010"
#' )
evaluate_performance <- function(
  test_data,
  freetext_colname,
  code_colname,
  code_format = "kldb-2010",
  followup_questions = TRUE,
  app_settings = create_app_settings(.validate = FALSE),
  suggestion_type_options = NULL
) {
  # Column names used in data.table (for R CMD CHECK)
  auxco_id <- ..answer_code_colname <- ..default_code_colname <- ..id_colname  <- NULL

  stopifnot(!is.null(test_data) && !is.null(freetext_colname) && !is.null(code_colname))

  # Merge
  suggestion_parameters <- list(
    suggestion_type = app_settings$suggestion_type,
    num_suggestions = app_settings$default_num_suggestions
  )
  if (!is.null(app_settings$get_job_suggestion_params)) {
    suggestion_parameters <- utils::modifyList(
      suggestion_parameters,
      app_settings$get_job_suggestion_params
    )
  }

  if (code_format == "kldb-2010") {
    final_code_name <- "kldb_id"
  } else if(code_format == "isco-08") {
    final_code_name <- "isco_id"
  }

  if (suggestion_parameters$suggestion_type == "auxco-1.2.x") {
    auxco <- get_data("auxco-1.2.x", user_provided_data = suggestion_type_options$datasets)
    id_colname <- "auxco_id"
  }

  evaluate_single_row <- function (row) {
    # Step 1. Generate suggestions
    final_suggestion_parameters <- utils::modifyList(
      suggestion_parameters,
      list(text = row[freetext_colname])
    )
    suggestions <- do.call(get_job_suggestions, final_suggestion_parameters)

    # Step 2. Determine final codings
    if (suggestion_parameters$suggestion_type == "auxco-1.2.x") {
      # Get default codes associated with suggestions
      default_code_colname <- paste0("default_", final_code_name)
      suggestion_codes <- auxco$categories[
        auxco_id %in% suggestions$auxco_id,
        ..default_code_colname
      ][[1]] # convert to vector

      # Get codes associated via followup questions
      if (followup_questions) {
        answer_code_colname <- paste0("answer_", final_code_name)
        followup_question_codes <- auxco$followup_questions[
          auxco_id %in% suggestions$auxco_id,
          ..answer_code_colname
        ][[1]] # convert to vector
        # Drop NAs e.g. when it's not a question answer, but just the question itself
        followup_question_codes <- followup_question_codes[
          !is.na(followup_question_codes) &
          followup_question_codes != ""
        ]

        # Add followup_question codes to suggestion_codes
        suggestion_codes <- c(suggestion_codes, followup_question_codes)
      }

      # Drop duplicate codes
      suggestion_codes <- unique(suggestion_codes)

    } else {
      stop("Unsupported suggestion_type")
    }

    # Generate the final row
    data.table(
      # Whether or not the final code is included in the suggestion
      suggestions_include_final_code = row[code_colname] %in% suggestion_codes,
      # A list of all the suggestions' codes, ordered by probability
      suggestions_ordered = suggestions[, ..id_colname][[1]] |>
        paste(collapse = ";"),
      # How many suggestions were generated
      n_suggestions = nrow(suggestions),
      # A list of all the associated final codes matching the code_format
      # This list is *not* ordered by probability / score
      final_codes_unordered = suggestion_codes |>
        paste(collapse = ";"),
      # The code format of the suggestions
      suggestions_format = id_colname,
      # The code format of the final codes (this should correspond to the
      # code_format, although they have slightly different names atm.)
      final_codes_format = final_code_name
    )
  }

  # Generate suggestions and summarise their information into one row
  evaluation_list <- apply(
    X = test_data |>
      as.data.table(),
    MARGIN = 1,
    FUN = evaluate_single_row
  )
  evaluation_df <- do.call(rbind, c(evaluation_list, list(fill = TRUE)))

  # Final performance scores
  scores <- list(
    frac_include_final_code = sum(
      evaluation_df$suggestions_include_final_code
    ) / nrow(evaluation_df),
    average_n_suggestions = mean(evaluation_df$n_suggestions)
  )

  if (suggestion_parameters$suggestion_type == "auxco-1.2.x") {
    if (followup_questions) {
      followup_questions_warning <- paste(
        "As final_codes of followup_questions are included,",
        "this may *slightly overestimate* performance."
      )
    } else {
      followup_questions_warning <- paste(
        "As final_codes of followup_questions are excluded,",
        "this may *severly underestimate* performance."
      )
    }
  } else {
    followup_questions_warning <- ""
  }

  # Generate a summary message
  cat(paste0(
    "=== Evaluation Report ===\n",
    "Using test_data passed with N = ", nrow(test_data), " rows.\n",
    "Occupational coding format in test_data: ", code_format, ".\n",
    "Generated on average ", round(scores$average_n_suggestions, digits = 2),
    " suggestions per entry. The maximum number of suggestions to ",
    "generate is ", suggestion_parameters$num_suggestions,
    " (set in app_settings).\n",
    "A total ",  round(scores$frac_include_final_code * 100),
    "% of codes were included as possible final_codes in the generated ",
    "suggestions. ",
    followup_questions_warning, "\n",
    "If you save the output from this function in a variable, ",
    "you can explore the results in detail.",
    "\n"
  ))

  # Combine the passed test_data with our newly generated evaluation data
  evaluation_combined <- cbind(
    test_data,
    evaluation_df
  )

  invisible(list(
    scores = scores,
    data = evaluation_combined
  ))
}
