#* @apiTitle occupationMeasurement API
#* @apiDescription An API to allow the interactive coding of occupations based on free text job descriptions. The flow of coding is to first get a list of [suggestions](#/occupationMeasurement/get_suggestions) based on a free form text answer. Once one of these suggestions has been picked, its id can be used to retrieve potential [followup questions](#/occupationMeasurement/get_followup_questions). Using the suggestion's id and the followup_questions's answer id, the [final codes](#/occupationMeasurement/get_final_codes) can be queried. Please also refer to the official package documentation for a more detailed description of the API coding flow.
#* @apiVersion 0.0.4-beta
#* @apiTag "occupationMeasurement" Endpoints related to the coding of occupations.
#* @apiTag "experimental" API endpoints where major changes are likely.
#* @apiTag "other" Other endpoints of the API.

# Note: Plumber annotations are always on one line per type
# (irrespective of number of characters), as it seems that plumber has some
# issues with parsing multi-line annotations.

#* Generate occupation coding suggestions based on a users free text input.
#*
#* We recommend displaying the "task" and optionally the "task_description" to
#* respondents.
#*
#* @param text:character The raw text input from the user.
#* @param suggestion_type:character Which type of suggestion to use / provide. Possible options are "auxco-1.2.x" and "kldb-2010".
#* @param num_suggestions:integer The maximum number of suggestions to show. This is an upper bound and less suggestions may be returned.
#* @tag "occupationMeasurement"
#* @get /v1/suggestions
function(text, suggestion_type = "auxco-1.2.x", num_suggestions = 5) {
  suggestions <- occupationMeasurement::get_job_suggestions(
    text = text,
    suggestion_type = suggestion_type,
    num_suggestions = num_suggestions
  )

  return(suggestions)
}

#* Get the next followup question to show after a suggestion has been selected
#*
#* Or a previous followup question has just been answered.
#*
#* @param suggestion_id:character The id of the selected suggestion.
#* @param followup_question_id:character The `question_id` of the followup question that has just been answered.
#* @param followup_answer_id:integer The answer_id of the chosen answer to the followup question that has just been answered.
#* @tag "occupationMeasurement"
#* @get /v1/next_followup_question
#* @serializer unboxedJSON
function(res, suggestion_id, followup_question_id = "", followup_answer_id = "") {
  # Sanitize input
  if (suggestion_id == "" || is.null(suggestion_id)) {
    suggestion_id <- NA
  }
  if (followup_question_id == "" || is.null(followup_question_id)) {
    followup_question_id <- NA
  }
  if (is.null(followup_answer_id)) {
    followup_answer_id <- NA
  } else {
    followup_answer_id <- as.integer(followup_answer_id)
  }

  # Validate input
  if (is.na(suggestion_id)) {
    res$status <- 400
    return(list(error = "It is mandatory to provide a suggestion_id"))
  }
  if (xor(is.na(followup_question_id), is.na(followup_answer_id))) {
    res$status <- 400
    return(list(error = "Specifying only one of followup_answer_id or followup_question_id is not supported"))
  }

  all_followup_questions <- occupationMeasurement::get_followup_questions(
    suggestion_id = suggestion_id
  )
  next_followup_question <- NULL

  response_finished <- list(
    coding_is_finished = TRUE
  )

  if (is.na(followup_question_id)) {
    # No follow_question_id provided -> return the first followup question
    next_followup_question <- all_followup_questions[[1]]
  } else {
    # A followup_question_id has been provided, find the question that has been answered
    all_question_ids <- sapply(all_followup_questions, function(x) x$question_id)
    answered_question_index <- seq_along(all_question_ids)[all_question_ids == followup_question_id]
    if (length(answered_question_index) == 0) {
      res$status <- 400
      return(list(error = paste("Can't find question_id", followup_question_id," in followup questions for suggestion", suggestion_id)))
    }
    answered_followup_question <- all_followup_questions[[answered_question_index]]

    # Determine whether the answer already finished coding
    if (answered_followup_question$answers$coding_is_finished[[as.numeric(followup_answer_id)]]) {
      return(response_finished)
    } else {
      # Else return the next followup question
      next_followup_question <- all_followup_questions[[answered_question_index + 1]]
    }
  }

  # When there are no followup_questions coding is finished
  if (length(all_followup_questions) == 0) {
    return(response_finished)
  }

  # Return the found followup question
  if (!is.null(next_followup_question)) {
    return(next_followup_question)
  } else {
    # Return an error upon unexpected behaviour
    res$status <- 500
    return(list(error = "Unexpected error when trying to find the next followup question."))
  }
}

#* Get the final occupation codes
#*
#* This is only needed when using auxco based suggestions, to get the final responses based on answers to followup questions.
#*
#* @param suggestion_id:character The id of the selected suggestion
#* @param followup_answers:[numeric] An Array / a list of integers corresponding to answers to followup questions in exact order.
#* @param isco_skill_level:character Standardized level of skill the respondent indicated (corresponding to ISCO-08 manual).
#* @param isco_supervisor_manager:character Standardized level of management / supervision the respondent indicated (corresponding to ISCO-08 manual).
#* @param:[character] code_type Which type of codes should be returned. Multiple codes can be returned at the same time. Supported types of codes are "isco_08" and "kldb_10". Defaults to "isco_08" and "kldb_10".
#* @tag "occupationMeasurement"
#* @get /v1/final_codes
#* @serializer unboxedJSON
function(suggestion_id,
         followup_answers = numeric(),
         isco_skill_level = NA_character_,
         isco_supervisor_manager = NA_character_,
         code_type = c("isco_08", "kldb_10")) {
  # Generate named list of followup answers
  question_ids <- occupationMeasurement::get_followup_questions(
    suggestion_id = suggestion_id
  ) |>
    sapply(function(x) x$question_id)
  followup_answers <- as.list(followup_answers)
  if (!is.null(followup_answers) && length(followup_answers) > 0) {
    length(question_ids) <- length(followup_answers)
    names(followup_answers) <- question_ids
  }

  # Generate list of standardized_answer_levels
  standardized_answer_levels <- list()
  if (isco_skill_level != "" && !is.na(isco_skill_level)) {
    standardized_answer_levels$isco_skill_level <- isco_skill_level
  }
  if (isco_supervisor_manager != "" && !is.na(isco_supervisor_manager)) {
    standardized_answer_levels$isco_supervisor_manager <- isco_supervisor_manager
  }

  final_codes <- occupationMeasurement::get_final_codes(
    suggestion_id = suggestion_id,
    # Converting to numeric here, to match the format used in the auxco
    followup_answers = followup_answers,
    standardized_answer_levels = standardized_answer_levels,
    code_type = code_type
  )

  return(final_codes)
}

#* Default API root response showing the API status and a link to its documentation.
#* @tag "other"
#* @get /
#* @serializer html
function() {
  "<html>
    <body>
      <h1>occupationMeasurement Api</h1>
      <p>Status: \U0001f7e2 Running</p>
      <p><a href=\"/__docs__/\">Documentation</a></p>
    </body>
  </html>"
}
