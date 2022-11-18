#' Create a new questionnaire page.
#'
#' Each page corresponds to a page within the app/questionnaire.
#'
#' Pages are rendered by calling the different life-cycle functions one
#' after another. The order in which they are called is as follows:
#'
#'   1. `condition` (`session`)
#'        Only if this evaluated to `TRUE`, continue.
#'   2. `run_before` (`session`)
#'   3. `render_before` (`session`, `run_before_output`, `input`, `output`)
#'   4. `render` (`session`, `run_before_output`, `input`, `output`)
#'   5. `render_after` (`session`, `run_before_output`, `input`, `output`)
#'       The outputs from `render_before`, `render` & `render_after` are
#'       stitched together to produce the final HTML output of the page.
#'   6. `run_after` (`session`, `input`, `output`)
#'       Run when the user leaves the page (=clicks the next button). Any
#'       user input has to be handled here. For each question asked, one will
#'       typically call [set_question_data()] to save the collected data
#'       internally.
#'
#' Each of the life-cycle functions above is annotated with the
#' paramaters it has access to. `session`, `input` and `output` are
#' passed directly from shiny and correspond to the objects made available by
#' [shiny::shinyServer()], `run_before_output` is available for convenience and
#' corresponds to whatever is returned by `run_before`.
#'
#' Some side-effects occur:
#' - `occupationMeasurement:::init_page_data` is called before 1. `run_before`.
#'   It sets up an internal representation of the page data to be saved.
#' - `occupationMeasurement:::finalize_data` is called before 6. `run_before`.
#' - `occupationMeasurement:::save_page_data` is called after 6. `run_before`.
#'   It saves the responses on a hard drive, i.e. it appends the responses
#'   from this page to `table_name == "answers"`. See the vignette
#'   and [create_app_settings()] for details.
#'
#' Use of `render_before`, `render_after` is discouraged if not necessary,
#' as these two life-cycle functions have only been added to allow for easier
#' modification and extension of existing pages.
#'
#' @param page_id A unique string identifiying this page. (Required)
#'   This will be used to store data.
#' @param render Function to render the page. (Required)
#'   It is expected, that the function returns a list of shiny tags.
#'   Its output will be combined with `render_before` and `render_after.`
#'   This function has access to the shiny `session` and the `run_before_output.`
#' @param condition Function to check whether the page should be shown.
#'   When this function returns `TRUE`, the page will be shown upon navigating
#'   there, if it returns `FALSE` it will be skipped.
#'   Defaults to show the page.
#'   This function has access to the shiny `session`.
#' @param run_before Function that prepares data to render the page.
#'   Called immediately after condition (if `condition` returned `TRUE`).
#'   Whatever run_before returns is available in render, render_before and
#'   render_after as `run_before_output`.
#'   This function has access to the shiny `session`.
#' @param render_before Called exactly like `render`. Output will be added
#'   just *before* the output from render. Mainly used to add additional
#'   outputs to existing pages.
#' @param render_after Called exactly like `render`. Output will be added
#'   just *after* the output from render. Mainly used to add additional
#'   outputs to existing pages.
#' @param run_after Function that handles the user input when they leave the
#'   page. This function has access to the shiny session and shiny input.
#'
#' @return A new `page` object.
#' @export
#'
#' @examples
#' one_page_questionnaire <- list(
#'   new_page(
#'     page_id = "example",
#'     render = function(session, run_before_output, input, output, ...) {
#'       shiny::tags$h1("My test page")
#'     }
#'   )
#' )
#'
#' # TODO: Include an example of a simple page that contains two questions
#' # and thus two calls to set_question_data()?
new_page <- function(page_id, render, condition = NULL, run_before = NULL, render_before = NULL, render_after = NULL, run_after = NULL) {
  page <- list(
    # A unique string identifiying this page. Used to store data.
    page_id = page_id,
    # Function to check whether the page should be shown.
    condition = condition,
    # Function that prepares data to render the page.
    run_before = run_before,
    # Called exactly like `render`. Output will be added just *before* the output from render.
    render_before = render_before,
    # Function to render the page. (Required)
    render = render,
    # Called exactly like `render`. Output will be added just *after* the output from render.
    render_after = render_after,
    # Function that handles the user input when they leave the page.
    run_after = run_after
  )

  class(page) <- "page"

  page
}

#' Called internally by the shiny server.
#'
#' @param session The shiny session
#' @param ... All additional arguments are passed along
#' @keywords internal
check_condition <- function(page, session, ...) {
  if (!is.null(page$condition)) {
    return(page$condition(session = session, page = page, ...))
  } else {
    return(TRUE)
  }
}

#' Called internally by the shiny server.
#'
#' @param session The shiny session
#' @param ... All additional arguments are passed along
#' @keywords internal
execute_run_before <- function(page, session, input, output, ...) {
  if (session$userData$app_settings$verbose) {
    cat("Page:", page$page_id, "\n")
  }

  # Initialize trial data e.g. set first timestamp, set ids etc.
  init_page_data(session = session, page_id = page$page_id)
  if (!is.null(page$run_before)) {
    return(page$run_before(session = session, page = page, input = input, output = output, ...))
  }
}

#' Called internally by the shiny server.
#'
#' @param session The shiny session
#' @param run_before_output The output from `run_before`
#' @param ... All additional arguments are passed along
#' @keywords internal
execute_render <- function(page, session, run_before_output, ...) {
  return(
    list(
      if (!is.null(page$render_before)) page$render_before(session = session, page = page, run_before_output = run_before_output, ...),
      page$render(session = session, page = page, run_before_output = run_before_output, ...),
      if (!is.null(page$render_after)) page$render_after(session = session, page = page, run_before_output = run_before_output, ...)
    )
  )
}

#' Called internally by the shiny server.
#'
#' @param session The shiny session
#' @param input The shiny input
#' @param ... All additional arguments are passed along
#' @keywords internal
execute_run_after <- function(page, session, input, output, ...) {
  # Finalize the trial data e.g. set the final timestamp
  finalize_data(session = session, page_id = page$page_id)

  if (!is.null(page$run_after)) {
    page$run_after(session = session, page = page, input = input, output = output, ...)
  }

  ## Save the page's data
  save_page_data(session = session, page_id = page$page_id)

  # Show a detailed message of the page's data if enabled
  if (session$userData$app_settings$verbose) {
    cat(
      "Page data for", page$page_id, ":",
      get_page_data(session = session, page_id = page$page_id) |>
        utils::str() |>
        utils::capture.output() |>
        paste(collapse = "\n"),
      "\n"
    )
  }
}

#' Set some values in the page/questionnaire data in the current session.
#'
#' Data is automatically linked to a page's page_id.
#' Note that page data is *not* automatically saved and you probably want
#' to use set_question_data instead.
#'
#' @param session The shiny session
#' @param values A named list of values to add / overwrite in the page data.
#'   Values are added / overwritten based on the names provided in the list.
#'
#' @seealso [set_question_data()]
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # This code is expected to be run in e.g. run_before
#' set_page_data(session = session, page_id = "example", values = list(
#'   user_answer = "Some User Answer"
#' ))
#' }
set_page_data <- function(session, page_id, values) {
  # Add / Overwrite all provided values in the questionnaire data
  session$userData$questionnaire_data[[page_id]] <- utils::modifyList(
    session$userData$questionnaire_data[[page_id]],
    values
  )
}

#' Get questionnaire / page data.
#'
#' Note that page data is *not* automatically saved and you probably want
#' to use page$get_question_data instead.
#'
#' @param session The shiny session
#' @param key The key for which to retrieve a value. (Optional)
#'   If no key is provided, the page's whole data will be returned.
#' @param default A default value to return if the key or page is not
#'   present in the questionnaire data.
#' @param page_id The page for which to retrieve data.
#'   Defaults to the page where data the function is being called from.
#'
#' @return The page data value at the provided key or the whole page's data
#'   if no key is provided.
#'
#' @seealso [get_question_data()]
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # This code is expected to be run in e.g. run_before
#' get_page_data(session = session, key = "user_answer")
#' }
get_page_data <- function(session, page_id, key = NULL, default = NULL) {
  stopifnot(!is.null(session) && !is.null(page_id))

  data_entry <- session$userData$questionnaire_data[[page_id]]
  if (!is.null(data_entry)) {
    if (!is.null(key)) {
      if (!is.null(data_entry[[key]])) {
        return(data_entry[[key]])
      } else {
        return(default)
      }
    } else {
      return(data_entry)
    }
  } else {
    return(default)
  }
}

#' Set / save data for a question.
#'
#' There can be multiple questions on any given page. The `question_text` is typically
#' saved in `run_before` and the reply (`response_text` and/or `response_id`) is
#' typically saved in `run_after`.
#'
#' @param session The shiny session
#' @param page_id The page for which to retrieve data.
#' @param question_id The question for which to retrieve data.
#'   This *has* to be different for different questions on the same page.
#'   Since most pages contain only a single question, `question_id` is set to "default" if missing.
#' @param question_text The question's text. (optional)
#' @param response_text The user's response in text form. (optional)
#' @param response_id The user's response as an id from a set of choices.
#'   (optional)
#'
#' @export
#' @seealso [get_question_data()]
#'
#' @examples
#' \dontrun{
#' # This code is expected to be run in e.g. run_before
#' set_question_data(
#'   session = session,
#'   page_id = "example",
#'   question_text = "How are you?"
#' )
#'
#' # This code is expected to be run in e.g. run_after
#' set_question_data(
#'   session = session,
#'   page_id = "example",
#'   response_id = 3,
#'   response_text = "I'm doing great!"
#' )
#' }
set_question_data <- function(session, page_id, question_id = NULL, question_text = NULL, response_text = NULL, response_id = NULL) {
  if (is.null(question_id)) {
    question_id <- "default"
  }
  # TODO: Maybe find a more elegant solution

  # As we can't update the question entry by reference, we first have to get all questions,
  # then the correct question. Change that and overwrite the question and questions again.
  questions <- get_page_data(
    session = session,
    page_id = page_id,
    key = "questions",
    default = list()
  )
  question <- if (!is.null(questions[[question_id]])) questions[[question_id]] else list()

  # Update supported fields on the question if they are not NULL
  supported_fields <- c("question_text", "response_text", "response_id")
  for (field in supported_fields) {
    value <- get(field)
    if (!is.null(value)) {
      question[[field]] <- value
    }
  }

  questions[[question_id]] <- question

  # Set the questions data on the page again
  set_page_data(
    session = session,
    page_id = page_id,
    values = list(questions = questions)
  )
}

#' Retrieve data for a question.
#'
#' Each page in the questionnaire can have multiple questions on it.
#'
#' @param session The shiny session
#' @param page_id The page for which to retrieve data.
#'   Defaults to the page where data the function is being called from.
#' @param question_id The question for which to retrieve data.
#'   This *has* to be different for different questions on the same page.
#'   Since most pages contain only a single question, `question_id` is set to "default" if missing.
#' @param key The key for which to retrieve a value. (Optional)
#'   If no key is provided, the question's whole data will be returned.
#' @param default A default value to return if the key or page is not
#'   present in the questionnaire data.
#'
#' @return The question's data.
#' @export
#' @seealso [set_question_data()]
#'
#' @examples
#' \dontrun{
#' # This code is expected to be run in e.g. run_before
#' get_question_data(
#'   session = session,
#'   page_id = "example",
#'   key = "response_text",
#'   default = "alright"
#' )
#' }
get_question_data <- function(session, page_id, question_id = NULL, key = c("all", "question_text", "response_text", "response_id"), default = NULL) {
  if (is.null(question_id)) {
    question_id <- "default"
  }
  key <- match.arg(key)

  questions <- get_page_data(
    session = session,
    page_id = page_id,
    key = "questions",
    default = list()
  )
  question <- questions[[question_id]]
  if (!is.null(question)) {
    if (!is.null(key) && key != "all") {
      if (!is.null(question[[key]])) {
        return(question[[key]])
      } else {
        return(default)
      }
    } else {
      return(question)
    }
  } else {
    return(default)
  }
}

# Data Handling Functions
init_page_data <- function(session, page_id) {
  if (is.null(session$userData$questionnaire_data[[page_id]])) {
    # Initialize page info
    session$userData$questionnaire_data[[page_id]] <- list(
      page_id = page_id,
      user_id = session$userData$user_info$id,
      session_id = session$userData$user_info$session_id
    )
  }

  # Overwrite certain values when navigation back to a page
  values_to_overwrite <- list(
    status = "old",
    start = as.character(Sys.time()),
    questions = list()
  )
  session$userData$questionnaire_data[[page_id]] <- utils::modifyList(
    session$userData$questionnaire_data[[page_id]],
    values_to_overwrite
  )
}

finalize_data <- function(session, page_id) {
  session$userData$questionnaire_data[[page_id]]$end <- as.character(Sys.time())
  session$userData$questionnaire_data[[page_id]]$status <- "new"
}

save_page_data <- function(session, page_id) {
  save_data(
    "answers",
    extract_questions_df(
      page_data = get_page_data(session = session, page_id = page_id)
    ),
    session = session
  )
}
