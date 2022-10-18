#* @apiTitle occupationMeasurement API
#* @apiDescription An API to allow the interactive coding of occupations based
#*   on free text job descriptions. The flow of coding is to first get a list
#*   of [suggestions](#/occupationMeasurement/get_suggestions) based on a free
#*   form text answer. Once one of these suggestions has been picked, its id
#*   can be used to retrieve potential
#*   [followup questions](#/occupationMeasurement/get_followup_questions).
#*   Using the suggestion's id and the followup_questions's answer id, the
#*   [final codes](#/occupationMeasurement/get_final_codes) can be queried.
#* @apiVersion 0.0.3-beta
#* @apiTag "occupationMeasurement" Endpoints related to the coding of occupations.
#* @apiTag "experimental" API endpoints where major changes are likely.
#* @apiTag "other" Other endpoints of the API.

#* Generate occupation coding suggestions based on a users free text input.
#* We recommend displaying the "task" and optionally the "task_description" to
#* respondents.
#* @param text:character The raw text input from the user.
#* @tag "occupationMeasurement"
#* @get /v1/suggestions
function(text) {
  suggestions <- occupationMeasurement::get_job_suggestions(
    text = text
  )

  return(suggestions)
}

#* Get the next followup question to show after a suggestion has been selected
#* or a previous followup question has just been answered.
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
#* Note: This function is likely to change drastically.
#* @param suggestion_id:character The id of the selected suggestion
#* @param followup_answers:[numeric] An Array / a list of integers
#*   corresponding to answers to followup questions in exact order.
#* @param:[character] code_type Which type of codes should be returned.
#*   Multiple codes can be returned at the same time.
#*   Supported types of codes are "isco_08" and "kldb_10".
#*   Defaults to "isco_08" and "kldb_10".
#* @param suggestion_type:character Which suggestion type is being used.
#*   Only auxco-based suggestion_types are supported.
#* @tag "experimental"
#* @get /v1/final_codes
function(suggestion_id,
         followup_answers = numeric(),
         code_type = c("isco_08", "kldb_10"),
         suggestion_type = "auxco") {
  question_ids <- occupationMeasurement::get_followup_questions(
    suggestion_id = suggestion_id
  ) |>
    sapply(function(x) x$question_id)
  if (!is.null(followup_answers) && length(followup_answers) > 0) {
    length(question_ids) <- length(followup_answers)
    names(followup_answers) <- question_ids
  }

  final_codes <- occupationMeasurement::get_final_codes(
    suggestion_id = suggestion_id,
    # Converting to numeric here, to match the format used in the auxco
    followup_answers = followup_answers,
    code_type = code_type,
    suggestion_type = suggestion_type
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
