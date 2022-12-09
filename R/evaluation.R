evaluate_performance <- function(
  test_data,
  freetext_colname,
  code_colname,
  code_format = "kldb-2010",

  app_settings = create_app_settings(.validate = FALSE),
  suggestion_type_options = NULL,
  followup_questions = TRUE
) {
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
      suggestions_include_final_code = row[code_colname] %in% suggestion_codes,
      suggestions_ordered = suggestions[, ..id_colname][[1]] |>
        paste(collapse = ";"),
      final_codes_unordered = suggestion_codes |>
        paste(collapse = ";"),
      suggestions_format = id_colname,
      final_codes_format = final_code_name
    )
  }

  evaluation_list <- apply(
    X = test_data |>
      as.data.table(),
    MARGIN = 1,
    FUN = evaluate_single_row
  )
  evaluation_df <- do.call(rbind, c(evaluation_list, list(fill = TRUE)))

  scores <- list(
    frac_include_final_code = sum(
      evaluation_df$suggestions_include_final_code
    ) / nrow(evaluation_df)
  )

  cat(paste0(
    "=== Evaluation Report === \n",
    round(scores$frac_include_final_code * 100), "% of codes were included as ",
    "possible final_codes in the generated suggestions.",
    "If you save the output from this function, you can explore the results in detail.",
    "\n"
  ))

  evaluation_combined <- cbind(
    test_data,
    evaluation_df
  )

  invisible(list(
    scores = scores,
    data = evaluation_combined
  ))
}
