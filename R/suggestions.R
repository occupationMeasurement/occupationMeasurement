create_document_term_matrix <- function(input) {
  # create vocabulary and document term matrix from coding_index$title
  # TODO: Warnungen abfangen wenn input der leere String "" ist
  # prep_fun = toupper
  tok_fun <- text2vec::word_tokenizer
  it_train <- text2vec::itoken(input,
    #                  preprocessor = prep_fun,
    tokenizer = tok_fun,
    #                 ids = data$id,
    progressbar = FALSE
  )
  vocab <- text2vec::create_vocabulary(it_train, stopwords = toupper(tm::stopwords("de")))
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  input_dtm <- text2vec::create_dtm(it_train, vectorizer, type = "dgTMatrix")
  return(input_dtm)
}

#' Make suggestions using similarity based reasoning.
#'
#' The Algorithm used here corresponds to Algorithm #10 in (Schierholz, 2019).
#' Note: This function should not be used directly, but rather as a step /
#' algorithm in get_job_suggestions.
#'
#' @references Schierholz, M. (2019). New Methods for Job and Occupation Classification (Ph.D. Thesis). University of Mannheim.
#' @seealso [get_job_suggestions]
#'
#' @param text_processed The processed user input.
#'   Will be provided by get_job_suggestions.
#' @param sim_name Which similarity measure to use.
#'   Possible values are "wordwise" or "substring".
#' @param probabilities Trained probabilities to be used,
#'   defaults to the one bundled with the package. See [pretrained_models]. This pretrained model always predicts a 5-digit code from the 2010 German Classification of Occupations, with some exceptions: -0004 stands for 'Not precise enough/uncodable', -0006 stands for 'Multiple Jobs', -0012 stands for 'Blue-collar workers', -0019 stands for 'Volunteer/Social Service', and -0030 stands for 'Student assistant'.
#' @return A data.table with suggestions or NULL if no suggestions were found.
#' @export
#' @examples
#' # Use with default settings
#' get_job_suggestions(
#'   "Arzt",
#'   steps = list(
#'     simbased_default = list(
#'       algorithm = algo_similarity_based_reasoning
#'     )
#'   )
#' )
#'
#' # Use with substring similarity
#' get_job_suggestions(
#'   "Arzt",
#'   steps = list(
#'     simbased_substring = list(
#'       algorithm = algo_similarity_based_reasoning,
#'       parameters = list(
#'         sim_name = "substring"
#'       )
#'     )
#'   )
#' )
#'
#' ## Compare algo_similarity_based_reasoning() with get_job_suggestions()
#'
#' # Example of using algo_similarity_based_reasoning() directly. Not recommended.
#' \dontrun{
#' algo_similarity_based_reasoning(preprocess_string("Arzt"), sim_name = "wordwise")[order(score, decreasing = TRUE)]
#' }
#' 
#' # Same output as before, but the function is more adaptable.
#' \dontrun{
#' get_job_suggestions(
#'  "Arzt",
#'  suggestion_type = "kldb",
#'  num_suggestions = 1500,
#'  steps = list(
#'        simbased_default = list(
#'          algorithm = algo_similarity_based_reasoning,
#'          parameters = list(
#'            sim_name = "wordwise"
#'          )
#'        )
#'      ))[, list(kldb_id, score, sim_name, kldb_id_title = title)]
#'}
algo_similarity_based_reasoning <- function(text_processed,
                                            sim_name = "wordwise",
                                            probabilities = occupationMeasurement::pretrained_models$similarity_based_reasoning, ...) {
  # Column names used in data.table (for R CMD CHECK)
  string <- dictString.dist <- str.dist <- dictString.string <- dictString.string.prob <- dictString.unobserved.mean.theta <- model.prob <- string.prob <- dist <- mean.theta <- unobserved.mean.theta <- NULL

  if (sim_name == "wordwise") {
    # Create a term-document matrix from the input
    document_term_matrix <- create_document_term_matrix(text_processed)

    # Calculate distances between any word in text and job titles from the coding index
    wordwise_str_distances <- stringdist::stringdistmatrix(
      probabilities$wordwise$modelProb[, string],
      document_term_matrix@Dimnames[[2]],
      method = "osa",
      weight = c(d = 1, i = 1, s = 1, t = 1)
    )
    # indices of words at most one character apart
    wordwise_str_distances_ind <- which(wordwise_str_distances <= 1, arr.ind = TRUE)

    # from this matrix we will only need those words (= word.id) and job titles (= dictString) that have a distance <= 1 (= dist)
    wordwise_str_distances_DT <- data.table(
      word.id = wordwise_str_distances_ind[, 2],
      dictString = probabilities$wordwise$modelProb[wordwise_str_distances_ind[, 1]],
      str.dist = wordwise_str_distances[wordwise_str_distances_ind]
    )

    # only keep entries from  probabilities$wordwise$modelProb with minimal dist & < 1
    model_prob_query <- unique(wordwise_str_distances_DT[
      !is.na(dictString.dist) & # not needed because this comes from the alphabetic dictionary
        str.dist == suppressWarnings(min(str.dist)), # only use entries that have minimal string distance
      list(string = dictString.string, string.prob = dictString.string.prob, unobserved.mean.theta = dictString.unobserved.mean.theta, dist = dictString.dist)
    ]) # only use entries that were originally in probabilities$wordwise$modelProb, throw away all duplicate rows that may be in here because they had different codes in the dictionary
  } else if (sim_name == "substring") {
    model_prob_query <- probabilities$"substring"$modelProb[which(sapply(probabilities$"substring"$modelProb[, string], grepl, text_processed, fixed = TRUE))]
  } else {
    stop("Unsupported sim_name.")
  }

  category_prob <- probabilities[[sim_name]][["categoryProb"]]

  # Generate Suggestions

  # Handle edge case of no similar entries
  if (model_prob_query[, .N] == 0) {
    return(NULL)
  }

  # calculate p(C_f = c | y)
  model_prob_query[, model.prob := string.prob / sum(string.prob)]

  # create the result data table
  # simple idea merge(category_prob, model_prob_query, by = c("dist", "string"), allow.cartesian=TRUE), BUT:
  # If one would calculate list(score = sum(mean.theta * model.prob)), by = list(id, code) at this point, we would make an error: when a particular rule does not suggest a category we still need to take the prior probabilities unobserved.mean.theta into account
  # -> add those prior probabilities to the suggestions table (though the difference may well be neglectible)
  # first pick the subset of rows from category_prob that are covered by cells similar to the query
  category_prob.subset <- merge(model_prob_query, category_prob, by = c("dist", "string"), all.x = TRUE)[, list(dist, string, code, mean.theta)]
  # DT needs a row for every combination of (string) and (subset of codes that was assigned to any of the strings)
  # we also include a special extra code here: -9999 represents any code that has no training cases in the cell -> unobserved.mean.theta will be inserted here
  suggestions <- unique(CJ(string = model_prob_query[, string], code = c("-9999", category_prob.subset[, unique(code)]))) # CJ returns a cross product, unique(CJ(...)) is used to avoid that we have rows for each dist, string, combination
  # get unobserved.mean.theta and model.prob from model_prob_query
  suggestions <- merge(suggestions, model_prob_query, by = c("string"), all.x = TRUE, allow.cartesian = TRUE) # allow cartesian means that we might get duplicate rows if a string is in dist = official and in dist = self-created
  # get mean.theta from category_prob
  suggestions <- merge(suggestions, category_prob.subset, by = c("dist", "string", "code"), all.x = TRUE)

  # insert mean theta for categories that were not found via a specific rule
  suggestions[is.na(mean.theta), mean.theta := unobserved.mean.theta]

  # now we have everything in place to make the suggestions
  suggestions <- suggestions[, list(score = sum(mean.theta * model.prob)), by = list(pred.code = code)]
  suggestions[, sim_name := sim_name]

  return(suggestions)
}

#' Make coding suggestions based on a users free text input.
#'
#' @param text The raw text input from the user.
#' @param suggestion_type Which type of suggestion to use / provide.
#'   Possible options are "auxco" and "kldb".
#' @param num_suggestions The maximum number of suggestions to show.
#'   This is an upper bound and less suggestions may be returned.
#'   Defaults to 5.
#' @param suggestion_type_options A list with options for generating
#'   suggestions. Supported options:
#'     - `datasets`: Pass specific datasets to be used whenn adding information
#'          to predictions e.g. use a specific version of the kldb or auxco.
#'          Supported datasets are: "auxco", "kldb". By default the datasets
#'          bundled with this package are used.
#' @param score_thresholds A named list of thresholds between 0 and 1. Each
#'   entry should correspond to one of the `steps`. Results from that step will
#'   only be returned if the sum of their scores is equal to or greater than
#'   the specified threshold. With a threshold of 0 results will always be
#'   returned.
#' @param distinctions Whether or not to add additional distinctions to
#'   similar occupational categories to the source code.
#'   Defaults to TRUE.
#' @param steps A list with the algorithms to use and their parameters. Each
#'   entry of the list should contain a nested list with two entries:
#'   algorithm (the algorithm's function itself) and parameters (the parameters
#'   to pass onto the algorithm). Each algorithm will also always have access
#'   to a default set of three parameters:
#'   - text_processed: The input text after preprocessing
#'   - suggestion_type: Which type of suggestion to output
#'   - num_suggestions: How many suggestions shall be returned
#'   These parameters must not be specified manually and will be provided
#'   automatically instead.
#'   Defaults to:
#'   ```
#'   list(
#'     # try similarity "one word at most 1 letter different" first
#'     list(
#'       algorithm = algo_similarity_based_reasoning,
#'       parameters = list(
#'         sim_name = "wordwise",
#'         min_aggregate_prob = 0.535
#'       )
#'     ),
#'     # since everything else failed, try "substring" similarity
#'     list(
#'       algorithm = algo_similarity_based_reasoning,
#'       parameters = list(
#'         sim_name = "substring",
#'         min_aggregate_prob = 0.02
#'       )
#'     )
#'   )
#'   ```
#'
#' @return A data.table with suggestions or NULL if no suggestions were found.
#' @export
#'
#' @examples
#' get_job_suggestions("Koch")
#'
#' get_job_suggestions("Schlosser")
get_job_suggestions <- function(text,
                                suggestion_type = "auxco", # or "kldb"
                                num_suggestions = 5,
                                suggestion_type_options = list(),
                                score_thresholds = list(
                                  simbased_wordwise = 0.535,
                                  simbased_substring = 0.002
                                ),
                                distinctions = TRUE,
                                steps = list(
                                  # try similarity "one word at most 1 letter different" first
                                  simbased_wordwise = list(
                                    algorithm = algo_similarity_based_reasoning,
                                    parameters = list(
                                      sim_name = "wordwise"
                                    )
                                  ),
                                  # since everything else failed, try "substring" similarity
                                  simbased_substring = list(
                                    algorithm = algo_similarity_based_reasoning,
                                    parameters = list(
                                      sim_name = "substring"
                                    )
                                  )
                                )) {
  # Column names used in data.table (for R CMD CHECK)
  score <- NULL

  if (length(text) > 1) {
    # Support the coding of multiple entries, by applying the function
    # to every entry. This could in theory be handled quicker, by optimizing it
    # to calculate all suggestions at once.
    list_of_multiple_suggestions <- lapply(
      X = text,
      FUN = get_job_suggestions,
      # Pass parameters
      suggestion_type = suggestion_type,
      num_suggestions = num_suggestions,
      distinctions = distinctions,
      steps = steps
    )
    combined_suggestion_df <- do.call(rbind, c(list_of_multiple_suggestions, list(fill = TRUE)))
    return(combined_suggestion_df)
  }

  # Preprocessing of input to clean up e.g. special characters, etc.
  text_processed <- preprocess_string(text)

  # These parameters are always available to the functions
  default_parameters <- list(
    text_processed = text_processed,
    suggestion_type = suggestion_type,
    num_suggestions = num_suggestions
  )

  # Run the different steps to generate suggestions / results
  for (i in seq_along(steps)) {
    # Get info about the step itself
    step_with_name <- steps[i]
    step <- step_with_name[[1]]
    step_name <- names(step_with_name)

    # Combine default and provided parameters
    user_provided_parameters <- step[["parameters"]]
    parameters <- utils::modifyList(
      default_parameters,
      if (!is.null(user_provided_parameters)) user_provided_parameters else list()
    )

    # Call the algorithm with the provided list of parameters
    result <- do.call(step[["algorithm"]], parameters)

    if (!is.null(result)) {
      # Convert suggestions into the correct suggestion_type
      # Note: At the moment all algorithms output "kldb" by default, this may change in the future
      result <- convert_suggestions(result, from = "kldb", to = suggestion_type, suggestion_type_options = suggestion_type_options)

      # Pick top X suggestions
      result <- utils::head(result[order(score, decreasing = TRUE)], num_suggestions)

      threshold <- score_thresholds[[step_name]]
      if (is.null(threshold) || sum(result$score) >= threshold) {
        # Stop running through algorithms if we get good enough results
        break
      }
    }
  }

  # Handle suggestions / results
  if (!is.null(result)) {
    # Add distinctions to highly similar, but different categories
    if (distinctions) {
      if (suggestion_type == "auxco") {
        result <- add_distinctions_auxco(previous_suggestions = result, num_suggestions = num_suggestions)
      } else if (suggestion_type == "kldb") {
        # TODO: Fix this to run properly or remove it. This is currently only intended to work for kldb level 3
        # result <- add_distinctions_kldb(previous_suggestions = result, num_suggestions = num_suggestions)
      } else {
        stop("The chosen suggestion_type does not support adding distinctions. Set distinctions to FALSE or use a different suggestion_type.")
      }
    }

    # Add the input_text to the output
    result <- cbind(
      input_text = text,
      result
    )

    # Add additional information to suggestions i.e. job labels, description, etc.
    if (suggestion_type == "auxco") {
      # Add additional information
      result <- merge(result, get_suggestion_info(suggestion_ids = result$auxco_id, suggestion_type = suggestion_type), by = "auxco_id", sort = FALSE)
    } else if (suggestion_type == "kldb") {
      # Do nothing, maybe add some info from kldb_10 in the future
    }

    return(result)
  } else {
    # No suggestions, return empty result
    return(
      data.table(
        id = NA_character_,
        input_text = text,
        score = 0
      )
    )
  }
}

#' Convert suggestion from one suggestion format into the other
#'
#' @param suggestions A data.table of suggestions as returned by one of the
#'   prediction algorithms e.g. [algo_similarity_based_reasoning()].
#' @param from The current suggestion_type in which suggestions are passed.
#' @param to The suggestion_type in which suggestions should be.
#'
#' @return A data.table of suggestions in the "to" format
#'
#' @inheritParams get_job_suggestions
#'
#' @keywords internal
convert_suggestions <- function(suggestions, from, to, suggestion_type_options = list()) {
  # Column names used in data.table (for R CMD CHECK)
  score <- kldb_id <- auxco_id <- NULL

  # TODO: Maybe use get_suggestion_info here in the future to get the information for merging
  # Convert suggestions from kldb format
  if (from == "kldb") {
    # Convert from kldb to kldb (i.e. just add some extra information to suggestions)
    if (to == "kldb") {
      kldb <- get_data("kldb", user_provided_data = suggestion_type_options$datasets)

      # Merge with kldb information
      joined_suggestions <- merge(kldb, suggestions, by.x = "kldb_id", by.y = "pred.code")

      return(joined_suggestions)
    } else if (to == "auxco") {
      auxco <- get_data("auxco", user_provided_data = suggestion_type_options$datasets)

      # Merge with auxco information
      joined_suggestions <- merge(auxco$mapping_from_kldb, suggestions, by.x = "kldb_id", by.y = "pred.code")
      # if multiple (.N) ids are appropriate for an KldB-category, split the probability mass among IDs
      joined_suggestions[, score := score / .N, by = kldb_id]
      # if multiple categories are connected with a single id, sum them up
      joined_suggestions <- joined_suggestions[, list(score = sum(score)), by = auxco_id]

      return(joined_suggestions)
    }
  }

  stop(paste0("Converting from ", from, " to ", to, " not yet implemented."))
}

# Also show distinctions to highly similar categories,
# based on the auxiliary classification
add_distinctions_auxco <- function(previous_suggestions, num_suggestions, suggestion_type_options = list()) {
  # Column names used in data.table (for R CMD CHECK)
  score <- order_indicator <- similarity <- auxco_id <- similar_auxco_id <- NULL

  auxco <- get_data("auxco", user_provided_data = suggestion_type_options$datasets)

  # if a category has high probability to be correct (> 0.6, value is made-up!) add all abgrenzungen (with similarity = high)
  # preliminary analysis with turtle data suggests that the exact value for the treshold (0.6) and the inserted probability (0.1) has basically no influence. Maybe a smaller threshold would be preferable? Set to 0.3 for testing (looks like a small threshold is most promising if we show many 7 answer options, larger thresholds around 0.7 seem better if we show at most four answer options)
  previous_suggestions[score > 0.5, order_indicator := 2L] # for later ordering, make sure that the most probable ID is on top and other IDs that have abgrenzung = "hoch" are next to it
  very_similar_distinctions <- rbind(previous_suggestions,
    list(auxco_id = auxco$distinctions[similarity == "hoch" & auxco_id %in% previous_suggestions[score > 0.2, auxco_id], similar_auxco_id]),
    fill = TRUE
  )
  if (previous_suggestions[score > 0.5, .N] > 0) very_similar_distinctions[is.na(score), order_indicator := 1] # for later ordering
  very_similar_distinctions[is.na(score), score := 0.05]
  # now add abrenzungen with similarity = "mittel" (thresholds not tested, but values are choses such that we have only a minor effect if one category dominates)
  # setting score := 0.005 means that mittel-abgrenzungen are only added at the end of the list below the other suggestions (if it does not push already existing suggestions up)
  very_similar_distinctions <- rbind(very_similar_distinctions,
    list(auxco_id = auxco$distinctions[similarity == "mittel" & auxco_id %in% previous_suggestions[score > 0.8, auxco_id], similar_auxco_id]),
    fill = TRUE
  )
  very_similar_distinctions[is.na(score), score := 0.005]
  very_similar_distinctions[is.na(order_indicator), order_indicator := 0] # for later ordering
  very_similar_distinctions <- very_similar_distinctions[, list(score = sum(score), order_indicator = max(order_indicator)), by = auxco_id]

  # order by score, remove duplicated auxco_ids and probabilities < 0.005 (anekdotische Evidenze: derart kleiner Wert macht Sinn bei "Buchhalterin") and return only the top 7 auxiliary category ids having highest prob
  # showing 4 categories may have accuracies around 76-77 percent, for 5-7 categories it is 80% (accurate is measured as "possibility that a respondent can choose a category that is linked to the manual coded KldB-category")
  very_similar_distinctions <- utils::head(very_similar_distinctions[order(order_indicator, score, decreasing = TRUE)], num_suggestions) # instead of this deterministic procedure, one might also draw at random in order to try all categories and find best ones to predict

  # Remove order_indicator, now that it has served its purpose
  very_similar_distinctions[, order_indicator := NULL]

  return(very_similar_distinctions)
}

# Also show distinctions to highly similar categories,
# based on the excludes (orig. "Ausschluss") column from the KldB
add_distinctions_kldb <- function(previous_suggestions, num_suggestions, suggestion_type_options = list()) {
  # Column names used in data.table (for R CMD CHECK)
  score <- pred.code <- kldb_id <- title <- excludes <- level <- NULL

  kldb_10 <- get_data("kldb", user_provided_data = suggestion_type_options$datasets)
  kldb_10_lvl_3 <- kldb_10[level == 3]

  three_digit_codes <- previous_suggestions[, list(score = sum(score)), by = list(code = substr(pred.code, 1, 3))]

  three_digit_codes <- merge(kldb_10_lvl_3[, list(kldb_id, title, excludes)], three_digit_codes, by.x = "kldb_id", by.y = "code")

  # get excluded categories that are similar if a category has probability > 0.8
  similar_categories <- strsplit(three_digit_codes[score > 0.8, excludes], "[^0-9]+")
  # and merge the similar categories to suggest them as well
  if (length(similar_categories) == 0) { # no similar categories
    three_digit_codes_similar_cats <- three_digit_codes[, list(kldb_id, title, score)]
  }
  if (length(similar_categories) == 1) { # standard
    three_digit_codes_similar_cats <- rbind(three_digit_codes[, list(kldb_id, title, score)],
      kldb_10_lvl_3[kldb_id %in% similar_categories[[1]][-1], list(kldb_id, title)],
      fill = TRUE
    )
    three_digit_codes_similar_cats[is.na(score), score := 0.1]
  }

  # order by score, remove duplicated ids and probabilities < 0.005 (anekdotische Evidenze: derart kleiner Wert macht Sinn bei "Buchhalterin") and return only the top <num_suggestions> auxiliary category ids having highest prob
  return(utils::head(three_digit_codes_similar_cats[order(score, decreasing = TRUE)][score > 0.005][!duplicated(kldb_id), ], num_suggestions))
}

#' Get potential follow-up questions for a suggestion.
#'
#' @param suggestion_id Id of the suggestion
#' @param tense Which tense i.e. time to use for questions & answers,
#'   this can be "present" or "past". Defaults to "present".
#' @param suggestion_type Which suggestion type is being used.
#'   Only auxco-based suggestion_types are supported.
#' @return List of followup questions and their answer options.
#' @export
#'
#' @inheritParams get_job_suggestions
#'
#' @examples
#' # Get followup questions for "Post- und Zustelldienste"
#' get_followup_questions("1004")
get_followup_questions <- function(suggestion_id, tense = "present", suggestion_type = "auxco", suggestion_type_options = list()) {
  # Column names used in data.table (for R CMD CHECK)
  entry_type <- question_id <- auxco_id <- NULL

  # Follow-up questions only work with auxco
  stopifnot(suggestion_type == "auxco")

  auxco <- get_data("auxco", user_provided_data = suggestion_type_options$datasets)

  all_question_entries <- auxco$followup_questions

  # Find all questions matching the suggestion id
  matching_entries <- all_question_entries[all_question_entries$auxco_id == suggestion_id, ]

  # Separate answer option and question rows
  question_row_mask <- matching_entries$entry_type == "question"
  # Remove entry_type, as we have the information in question_row_mask
  matching_entries[, entry_type := NULL]

  # Separate answer option and question columns
  columns <- colnames(matching_entries)
  columns_to_remove <- c(
    "aggregated_answer_id_combination",
    "answer_id_combination",
    "corresponding_answer_level"
  )
  columns <- columns[!columns %in% columns_to_remove]
  answer_columns <- c(
    "question_id",
    columns[!stringr::str_starts(columns, stringr::fixed("question_"))]
  )
  question_columns <- columns[!stringr::str_starts(columns, stringr::fixed("answer_"))]

  # Apply the two masks
  entries_questions <- matching_entries[question_row_mask, question_columns, with = FALSE]
  entries_answers <- matching_entries[!question_row_mask, answer_columns, with = FALSE]

  # Transform into a usable format
  questions <- list()
  for (q_id in unique(matching_entries[question_row_mask, question_id])) {
    the_question <- entries_questions[entries_questions$question_id == q_id, ]
    the_answer_options <- entries_answers[entries_answers$question_id == q_id, ]

    # Remove auxco / question_id from answer options
    the_answer_options[, auxco_id := NULL]
    the_answer_options[, question_id := NULL]

    questions <- append(questions, list(list(
      question_id = q_id,
      question_text = the_question[[ifelse(tense == "past", "question_text_past", "question_text_present")]],
      type = the_question[["question_type"]],
      answers = the_answer_options
    )))
  }

  return(questions)
}

#' Get additional information for a suggestion id.
#'
#' This includes a descriptive title, job descriptions etc.
#' To retrieve information regarding followup questions use
#' \link{get_followup_questions}.
#'
#' @param suggestion_id Suggestion id(s) to retrieve information for.
#'   This should typically be a character vector.
#' @param suggestion_type Which suggestion type is being used.
#'   Only auxco-based suggestion_types are supported.
#'
#' @return Data table with information about the suggestion.
#' @keywords internal
#' @export
#'
#' @inheritParams get_job_suggestions
#'
#' @examples
#' get_suggestion_info("9079")
get_suggestion_info <- function(suggestion_ids,
                                suggestion_type = "auxco",
                                suggestion_type_options = list()) {
  # Column names used in data.table (for R CMD CHECK)
  auxco_id <- NULL

  if (suggestion_type == "auxco") {
    auxco <- get_data("auxco", user_provided_data = suggestion_type_options$datasets)

    # Return relevant category entries
    categories <- data.table::as.data.table(auxco$categories)
    return(categories[auxco_id %in% suggestion_ids])
  } else if (suggestion_type == "kldb") {
    # TODO: Add some info from kldb_10
    stop("Not implemented yet")
  }
}

#' Get the final occupation codes
#'
#' @param suggestion_id Id of the suggestion
#' @param followup_answers A named list of the question_ids with their
#'   respective answers to the followup_questions.
#'   Question ids correspond to list names, answers correspond to list values.
#' @param code_type Which type of codes should be returned.
#'   Multiple codes can be returned at the same time.
#'   Supported types of codes are "isco_08" and "kldb_10".
#'   Defaults to "isco_08" and "kldb_10".
#' @param suggestion_type Which suggestion type is being used.
#'   Only auxco-based suggestion_types are supported.
#'
#' @inheritParams get_job_suggestions
#'
#' @return A named list corresponding to the code_type(s) specified.
#' @export
#'
#' @examples
#' get_final_codes(
#'   # First option for "Koch"
#'   "9079",
#'   followup_answers = list(
#'     # The first answer option in the followup questions
#'     "Q9079_1" = 1
#'   )
#' )
get_final_codes <- function(suggestion_id, followup_answers, code_type = c("isco_08", "kldb_10"), suggestion_type = "auxco", suggestion_type_options = list()) {
  # Column names used in data.table (for R CMD CHECK)
  entry_type <- auxco_id <- NULL

  stopifnot(suggestion_type == "auxco")

  auxco <- get_data("auxco", user_provided_data = suggestion_type_options$datasets)

  followup_questions <- get_followup_questions(suggestion_id = suggestion_id, suggestion_type = suggestion_type)
  aggregated_answer_encodings <- auxco$followup_questions[
    entry_type == "aggregated_answer_encoding" & auxco_id == suggestion_id,
  ]

  # Check whhether names are set on follow_answers
  if (length(followup_answers) > 0 && !is.character(names(followup_answers))) {
    stop("followup_answers need to be supplied as a named list, with question_ids as names")
  }

  if (nrow(aggregated_answer_encodings) > 0 && length(followup_answers) > 0) {
    # Parse the strings of ids and answers e.g. 1749_1=1&1749_2=1
    # They first get parsed into a list of lists
    parsed_answer_id_combinations <- lapply(
      aggregated_answer_encodings$answer_id_combination,
      shiny::parseQueryString
    ) |>
      # Which is then transformed into a data.frame
      do.call(what = rbind.data.frame)

    question_ids <- colnames(parsed_answer_id_combinations)

    # This data.frame is then added to the existing aggregated_answer_encodings
    aggregated_answer_encodings[
      ,
      (question_ids) := parsed_answer_id_combinations
    ]

    # Check whether all questions in answer_id_combination have been answered
    if (setequal(union(question_ids, names(followup_answers)), question_ids)) {
      # Compare question answers and check whether there are any matches where all questions match
      all_matching <- apply(
        aggregated_answer_encodings[, question_ids, with = FALSE] == followup_answers[question_ids],
        1,
        all
      )

      # Get the matched row
      matched_answer_encoding <- aggregated_answer_encodings[all_matching]

      if (nrow(matched_answer_encoding) == 1) {
        # Retrieve answer codes from aggregate encoding
        result <- list()
        if ("isco_08" %in% code_type) {
          result$isco_08 <- matched_answer_encoding$answer_isco_id
        }
        if ("kldb_10" %in% code_type) {
          result$kldb_10 <- matched_answer_encoding$answer_kldb_id
        }
        return(result)
      }
    }
  }

  # Check normal followup questions (i.e. only look at the last one)
  if (length(followup_questions) > 0 && length(followup_answers) > 0) {
    answer <- NULL
    # Iterate over followup_questions in reverse order
    # (i.e. look at the last one first)
    for (i in rev(seq_along(followup_questions))) {
      followup_question <- followup_questions[[i]]

      # Convert answer to numeric (as sometimes strings are passed)
      # Using the correct type here is important to be able to pick an answer
      followup_answer_id <- followup_answers[[followup_question$question_id]] |>
        as.numeric()

      # When converting NULL to numeric a vector of length 0 is intorduced so we also check for that
      if (length(followup_answer_id) > 0 && !is.null(followup_answer_id) && !is.na(followup_answer_id)) {
        question <- followup_questions[[i]]
        answer <- question$answers[followup_answer_id, ]
        break
      }
    }

    if (!is.null(answer)) {
      # Retrieve answer codes from followup
      result <- list()
      if ("isco_08" %in% code_type) {
        result$isco_08 <- answer$answer_isco_id
      }
      if ("kldb_10" %in% code_type) {
        result$kldb_10 <- answer$answer_kldb_id
      }
      return(result)
    }
  }

  # Fall back to use the default codes from the suggestion
  selected_suggestion_info <- get_suggestion_info(
    suggestion_ids = suggestion_id,
    suggestion_type = suggestion_type
  )
  if (!is.null(selected_suggestion_info)) {
    # Retrieve answer codes from selected suggestion
    result <- list()
    if ("isco_08" %in% code_type) {
      result$isco_08 <- selected_suggestion_info$default_isco_id
    }
    if ("kldb_10" %in% code_type) {
      result$kldb_10 <- selected_suggestion_info$default_kldb_id
    }
    return(result)
  } else {
    # No suggestion found for this code. This should not be possible.
    stop(paste0("Invalid suggestion_id: ", suggestion_id, ". There does not exist a suggestion for this id."))
  }
}
