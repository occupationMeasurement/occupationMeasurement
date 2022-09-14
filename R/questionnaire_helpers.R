# Small helper functions to be used when creating questionnaire pages

button_next <- function(label = "Weiter") {
  actionButton("nextButton", label)
}
button_previous <- function(label = "Zur\u00fcck") {
  actionButton("previousButton", label)
}
mark_questionnaire_complete <- function() {
  tags$script("window.study_completed = true;")
}

#' Show a page with multiple radio button options where once can be picked.
#'
#' @inheritParams page_freetext
#' @param list_of_options A list of answering options.
#'   This can either be just a simple list of values or a named list with the
#'   names corresponding to what the user sees and the values corresponding to
#'   the actually saved values.
#'   e.g. with `list(One = 1, Two = 2, Three = 3)` people will see One, Two, ...
#'   and numbers 1, 2, 3 will be saved under `response_id`.
#' @param ... Other parametrs are passed on to `new_page()`
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
#'
#' @examples
#' page_choose_one_option(
#'   "test_page_radio",
#'   question_text = "Hello there! Please pick your favorite number from the options below:",
#'   list_of_options = list(One = 1, Two = 2, Three = 3)
#' )
page_choose_one_option <- function(page_id,
                                   question_text = "Please pick one of the following options",
                                   list_of_options = list(One = 1, Two = 2, Three = 3),
                                   next_button = TRUE,
                                   previous_button = TRUE,
                                   run_before = NULL,
                                   run_after = NULL,
                                   ...) {
  new_page(
    page_id = page_id,
    run_before = function(session, page, ...) {
      if (is.function(question_text)) {
        # Handle dynamic question texts if a function is passed instead of a string
        actual_question_text <- question_text(session, page, ...)
      } else {
        actual_question_text <- question_text
      }
      set_question_data(
        session = session,
        page_id = page$page_id,
        question_text = actual_question_text
      )

      # Support a custom run_before
      if (!is.null(run_before)) {
        extra_run_before_output <- run_before(session, page, ...)
      } else {
        extra_run_before_output <- list()
      }

      utils::modifyList(
        list(
          question_text = actual_question_text
        ),
        extra_run_before_output
      )
    },
    render = function(session, page, run_before_output, ...) {
      list(
        p(run_before_output$question_text),
        radioButtons("radioButtonQuestion", NULL,
          width = "100%",
          choices = list_of_options,
          selected = get_question_data(session = session, page_id=page$page_id, key = "response_id")
        ),
        if (previous_button) button_previous(),
        if (next_button) button_next()
      )
    },
    run_after = function(session, page, input, ...) {
      response_id <- input[["radioButtonQuestion"]]
      set_question_data(
        session = session,
        page_id = page$page_id,
        response_id = response_id,
        response_text = names(list_of_options[list_of_options == response_id])
      )

      # Support a custom run_after
      if (!is.null(run_after)) {
        run_after(session, page, input, ...)
      }
    },
    ...
  )
}

#' Show a page with a text field where free text can be entered.
#'
#' @param page_id A unique string identifiying this page. Used to store data.
#' @param question_text The question / text to display.
#'   This can be either a string, which will simply be displayed or a function
#'   to dynamically determine the question_text.
#' @param no_answer_checkbox Whether to provide a checkbox to denote that no
#'   answer has been provided.
#' @param next_button Whether to show the button to navigate to the next page?
#'   Defaults to TRUE.
#' @param previous_button Whether to show the button to navigate to the preivous page?
#'   Defaults to TRUE.
#' @param render_question_text Whether the question text should be displayed?
#'   Only set this to FALSE, if you wish to change the rendering of the
#'   question_text by e.g. using `render_before`.
#'   Defaults to TRUE.
#' @param run_before Similar to `run_before` in `new_page()`, passed explicitly
#'   here as this page adds some of its own code to `run_before`.
#' @param run_after Similar to `run_after` in `new_page()`, passed explicitly
#'   here as this page adds some of its own code to `run_after`.
#' @param ... Other parametrs are passed on to `new_page()`
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
#'
#' @examples
#' page_freetext(
#'   "test_page_freetext",
#'   question_text = "Hello there! Please fill in your name below:",
#'   no_answer_checkbox = TRUE
#' )
page_freetext <- function(page_id,
                          question_text = "Please enter your answer in the box below",
                          no_answer_checkbox = TRUE,
                          next_button = TRUE,
                          previous_button = TRUE,
                          render_question_text = TRUE,
                          run_before = NULL,
                          run_after = NULL,
                          ...) {
  new_page(
    page_id = page_id,
    run_before = function(session, page, ...) {
      if (is.function(question_text)) {
        # Handle dynamic question texts if a function is passed instead of a string
        actual_question_text <- question_text(session, page, ...)
      } else {
        actual_question_text <- question_text
      }
      set_question_data(
        session = session,
        page_id = page$page_id,
        question_text = actual_question_text
      )
      if (no_answer_checkbox) {
        set_question_data(
          session = session,
          page_id = page$page_id,
          question_id = "no_answer",
          question_text = actual_question_text
        )
      }

      # Support a custom run_before
      if (!is.null(run_before)) {
        extra_run_before_output <- run_before(session, page, ...)
      } else {
        extra_run_before_output <- list()
      }

      utils::modifyList(
        list(
          question_text = actual_question_text
        ),
        extra_run_before_output
      )
    },
    render = function(session, page, run_before_output, ...) {
      list(
        if (render_question_text) p(run_before_output$question_text),
        textInput(
          paste0(page$page_id, "_text"),
          NULL,
          value = get_question_data(session = session, page_id=page$page_id, key = "response_text"),
          width = "80%"
        ),
        br(),
        if (no_answer_checkbox) {
          checkboxInput(
            paste0(page$page_id, "_chk_no_answer"),
            label = p(class = "interviewer", "*** Keine Angabe"),
            value = get_question_data(session = session, page_id=page$page_id, question_id = "no_answer", key = "response_id"),
            width = "100%"
          )
        },
        if (previous_button) button_previous(),
        if (next_button) button_next()
      )
    },
    run_after = function(session, page, input, ...) {
      set_question_data(
        session = session,
        page_id = page$page_id,
        response_text = if (is.null(input[[paste0(page$page_id, "_text")]])) "" else input[[paste0(page$page_id, "_text")]]
      )

      # Also record no-answer checkbox
      if (no_answer_checkbox) {
        set_question_data(
          session = session,
          page_id = page$page_id,
          question_id = "no_answer",
          response_id = if (is.null(input[[paste0(page$page_id, "_chk_no_answer")]])) FALSE else input[[paste0(page$page_id, "_chk_no_answer")]]
        )
      }

      # Support a custom run_after
      if (!is.null(run_after)) {
        run_after(session, page, input, ...)
      }
    },
    ...
  )
}
