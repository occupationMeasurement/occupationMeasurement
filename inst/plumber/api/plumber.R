#* @apiTitle occupationMeasurement API
#* @apiDescription An API to allow the interactive coding of occupations based
#*   on free text job descriptions. The flow of coding is to first get a list
#*   of [suggestions](#/occupationMeasurement/get_suggestions) based on a free
#*   form text answer. Once one of these suggestions has been picked, its id
#*   can be used to retrieve potential
#*   [followup questions](#/occupationMeasurement/get_followup_questions).
#*   Using the suggestion's id and the followup_questions's answer id, the
#*   [final codes](#/occupationMeasurement/get_final_codes) can be queried.
#* @apiVersion 0.0.2-beta
#* @apiTag "occupationMeasurement" Endpoints related to the coding of occupations.
#* @apiTag "experimental" API endpoints where major changes are likely.
#* @apiTag "other" Other endpoints of the API.

#* Generate occupation coding suggestions based on a users free text input.
#* @param text:character The raw text input from the user.
#* @tag "occupationMeasurement"
#* @get /v1/suggestions
function(text) {
  suggestions <- occupationMeasurement::get_job_suggestions(
    text = text
  )

  return(suggestions)
}

#* Get a list of followup questions to ask, based on a selected suggestion id
#* @param suggestion_id:character The id of the selected suggestion.
#* @tag "occupationMeasurement"
#* @get /v1/followup_questions
function(suggestion_id) {
  followup_questions <- occupationMeasurement::get_followup_questions(
    suggestion_id = suggestion_id
  )

  return(followup_questions)
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
